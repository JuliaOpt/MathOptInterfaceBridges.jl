const MOIU = MathOptInterfaceUtilities
const C{F, S} = Tuple{CR{F, S}, F, S}

# Implementation of MOI for vector of constraint
function _addconstraint!{F, S}(constrs::Vector{C{F, S}}, cr::CR, f::F, s::S)
    push!(constrs, (cr, f, s))
    length(constrs)
end

function _delete!(constrs::Vector, cr::CR, i::Int)
    deleteat!(constrs, i)
    @view constrs[i:end] # will need to shift it in constrmap
end

_getfun(cr::CR, f::MOI.AbstractFunction, s::MOI.AbstractSet) = f
function _getfunction(constrs::Vector, cr::CR, i::Int)
    @assert cr.value == constrs[i][1].value
    _getfun(constrs[i]...)
end

_gets(cr::CR, f::MOI.AbstractFunction, s::MOI.AbstractSet) = s
function _getset(constrs::Vector, cr::CR, i::Int)
    @assert cr.value == constrs[i][1].value
    _gets(constrs[i]...)
end

_modifyconstr{F, S}(cr::CR{F, S}, f::F, s::S, change::F) = (cr, change, s)
_modifyconstr{F, S}(cr::CR{F, S}, f::F, s::S, change::S) = (cr, f, change)
_modifyconstr{F, S}(cr::CR{F, S}, f::F, s::S, change::MOI.AbstractFunctionModification) = (cr, modifyfunction(f, change), s)
function _modifyconstraint!{F, S}(constrs::Vector{C{F, S}}, cr::CR{F}, i::Int, change)
    constrs[i] = _modifyconstr(constrs[i]..., change)
end

_getnoc{F, S}(constrs::Vector{C{F, S}}, noc::MOI.NumberOfConstraints{F, S}) = length(constrs)

function _getloc{F, S}(constrs::Vector{C{F, S}})::Vector{Tuple{DataType, DataType}}
    isempty(constrs) ? [] : [(F, S)]
end

# Implementation of MOI for AbstractInstance
abstract type AbstractInstance{T} end

getconstrloc(m::AbstractInstance, cr::CR) = m.constrmap[cr.value]

# Variables
MOI.getattribute(m::AbstractInstance, ::MOI.NumberOfVariables) = m.nvars
MOI.addvariable!(m::AbstractInstance) = MOI.VariableReference(m.nvars += 1)
function MOI.addvariables!(m::AbstractInstance, n::Integer)
    [MOI.addvariable!(m) for i in 1:n]
end

# Objective
MOI.getattribute(m::AbstractInstance, ::MOI.Sense) = m.sense
MOI.getattribute(m::AbstractInstance, ::MOI.ObjectiveFunction) = m.objective

function MOI.setobjective!(m::AbstractInstance, sense::MOI.OptimizationSense, f::MOI.AbstractFunction)
    m.sense = sense
    # f needs to be copied, see #2
    m.objective = deepcopy(f)
end

function MOI.modifyobjective!(m::AbstractInstance, change::MOI.AbstractFunctionModification)
    m.objective = modifyfunction(m.objective, change)
end

# Constraints
function MOI.addconstraint!{F, S}(m::AbstractInstance, f::F, s::S)
    cr = CR{F, S}(m.nconstrs += 1)
    # f needs to be copied, see #2
    push!(m.constrmap, _addconstraint!(m, cr, deepcopy(f), s))
    cr
end

function MOI.delete!(m::AbstractInstance, cr::CR)
    for (cr_next, _, _) in _delete!(m, cr, getconstrloc(m, cr))
        m.constrmap[cr_next.value] -= 1
    end
    m.constrmap[cr.value] = 0
end

function MOI.modifyconstraint!(m::AbstractInstance, cr::CR, change)
    _modifyconstraint!(m, cr, getconstrloc(m, cr), change)
end

MOI.getattribute(m::AbstractInstance, noc::MOI.NumberOfConstraints) = _getnoc(m, noc)

MOI.cangetattribute(m::AbstractInstance, ::Union{MOI.NumberOfVariables,
                                                 MOI.NumberOfConstraints,
                                                 MOI.ListOfConstraints,
                                                 MOI.ObjectiveFunction,
                                                 MOI.Sense}) = true

MOI.cangetattribute(m::AbstractInstance, ::Union{MOI.ConstraintFunction,
                                                 MOI.ConstraintSet}, ref) = true

function MOI.getattribute(m::AbstractInstance, ::MOI.ConstraintFunction, cr::CR)
    _getfunction(m, cr, getconstrloc(m, cr))
end

function MOI.getattribute(m::AbstractInstance, ::MOI.ConstraintSet, cr::CR)
    _getset(m, cr, getconstrloc(m, cr))
end

# Macro to generate Instance
abstract type Constraints{F} end

struct SymbolSet
    s::Symbol
    typed::Bool
end

# QuoteNode prevents s from being interpolated and keeps it as a symbol
# Expr(:., MOI, s) would be MOI.s
# Expr(:., MOI, $s) would be Expr(:., MOI, EqualTo)
# Expr(:., MOI, :($s)) would be Expr(:., MOI, :EqualTo)
# Expr(:., MOI, :($(QuoteNode(s)))) is Expr(:., MOI, :(:EqualTo)) <- what we want
_mod(m, s::Symbol) = Expr(:., m, :($(QuoteNode(s))))
_set(s::SymbolSet) = _mod(MOI, s.s)

_field(s::SymbolSet) = Symbol(lowercase(string(s.s)))

function _getC(s::SymbolSet)
    if s.typed
        :(MOIU.C{F, $(_set(s)){T}})
    else
        :(MOIU.C{F, $(_set(s))})
    end
end

_getCV(s::SymbolSet) = :($(_getC(s))[])

_getlocfield(s::SymbolSet) = :(MOIU._getloc(m.$(_field(s))))

"""
    macro instance(instancename, scalarsets, typedscalarsets, vectorsets, typedvectorsets)

Creates a type Instance implementing the MOI instance interface and containing `scalarsets` scalar sets `typedscalarsets` typed scalar sets, `vectorsets` vector sets and `typedvectorsets` typed vector sets.
To give no set, write `()`, to give one set `S`, write `(S,)`.

### Examples

The instance describing an linear program would be:
```
@instance Instance () (EqualTo, GreaterThan, LessThan, Interval) (Zeros, Nonnegatives, Nonpositives) ()
```
"""
macro instance(instancename, ss, sst, vs, vst)
    scalarsets = [SymbolSet.(ss.args, false); SymbolSet.(sst.args, true)]
    vectorsets = [SymbolSet.(vs.args, false); SymbolSet.(vst.args, true)]

    scname = Symbol(string(instancename) * "ScalarConstraints")
    vcname = Symbol(string(instancename) * "VectorConstraints")

    scalarconstraints = :(struct $scname{T, F<:MOI.AbstractScalarFunction} <: MOIU.Constraints{F}; end)
    for s in scalarsets
        set = _set(s)
        field = _field(s)
        push!(scalarconstraints.args[3].args, :($field::Vector{$(_getC(s))}))
    end

    vectorconstraints = :(struct $vcname{T, F<:MOI.AbstractVectorFunction} <: MOIU.Constraints{F}; end)
    for s in vectorsets
        set = _set(s)
        field = _field(s)
        push!(vectorconstraints.args[3].args, :($field::Vector{$(_getC(s))}))
    end

    code = :()
    for (f, T) in ((:_addconstraint!, CR), (:_modifyconstraint!, CR), (:_delete!, CR), (:_getfunction, CR), (:_getset, CR), (:_getnoc, MOI.NumberOfConstraints))
        fun = _mod(MOIU, f)
        for s in scalarsets
            set = _set(s)
            field = _field(s)
            code = quote
                $code
                $fun{F}(m::$scname, cr::$T{F, <:$set}, args...) = $fun(m.$field, cr, args...)
            end
        end
        for s in vectorsets
            set = _set(s)
            field = _field(s)
            code = quote
                $code
                $fun{F}(m::$vcname, cr::$T{F, <:$set}, args...) = $fun(m.$field, cr, args...)
            end
        end

        code = quote
            $code
            $fun(m::$instancename, cr::$T{<:MOIU.SVF}, args...) = $fun(m.sv, cr, args...)
            $fun(m::$instancename, cr::$T{<:MOIU.SAF}, args...) = $fun(m.sa, cr, args...)
            $fun(m::$instancename, cr::$T{<:MOIU.SQF}, args...) = $fun(m.sq, cr, args...)
            $fun(m::$instancename, cr::$T{<:MOIU.VVF}, args...) = $fun(m.vv, cr, args...)
            $fun(m::$instancename, cr::$T{<:MOIU.VAF}, args...) = $fun(m.va, cr, args...)
            $fun(m::$instancename, cr::$T{<:MOIU.VQF}, args...) = $fun(m.vq, cr, args...)
        end
    end

    return esc(quote
        $scalarconstraints
        function $scname{T, F}() where {T, F}
            $scname{T, F}($(_getCV.(scalarsets)...))
        end

        $vectorconstraints
        function $vcname{T, F}() where {T, F}
            $vcname{T, F}($(_getCV.(vectorsets)...))
        end

        function MOIU._getloc(m::$scname)
            vcat($(_getlocfield.(scalarsets)...))
        end
        function MOIU._getloc(m::$vcname)
            vcat($(_getlocfield.(vectorsets)...))
        end

        mutable struct $instancename{T} <: MOIU.AbstractInstance{T}
            sense::MOI.OptimizationSense
            objective::MOIU.SAF{T}
            sv::$scname{T, MOIU.SVF}
            sa::$scname{T, MOIU.SAF{T}}
            sq::$scname{T, MOIU.SQF{T}}
            vv::$vcname{T, MOIU.VVF}
            va::$vcname{T, MOIU.VAF{T}}
            vq::$vcname{T, MOIU.VQF{T}}
            nvars::UInt64
            nconstrs::UInt64
            constrmap::Vector{Int} # Constraint Reference value ci -> index in array in Constraints
        end
        function $instancename{T}() where T
            $instancename{T}(MOI.FeasibilitySense, MOIU.SAF{T}(MOI.VariableReference[], T[], zero(T)),
                   $scname{T, MOIU.SVF}(), $scname{T, MOIU.SAF{T}}(), $scname{T, MOIU.SQF{T}}(), $vcname{T, MOIU.VVF}(), $vcname{T, MOIU.VAF{T}}(), $vcname{T, MOIU.VQF{T}}(),
                   0, 0, Int[])
        end

        function MOI.getattribute(m::$instancename, loc::MOI.ListOfConstraints)
            [MOIU._getloc(m.sv); MOIU._getloc(m.sa); MOIU._getloc(m.sq); MOIU._getloc(m.vv); MOIU._getloc(m.va); MOIU._getloc(m.vq)]
        end

        $code

    end)
end
