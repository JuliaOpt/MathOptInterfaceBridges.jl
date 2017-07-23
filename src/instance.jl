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
    push!(m.constrmap, _addconstraint!(m, cr, deepcopy(f), deepcopy(s)))
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

abstract type SymbolFS end
struct SymbolFun <: SymbolFS
    s::Symbol
    typed::Bool
    cname::Symbol
end
struct SymbolSet <: SymbolFS
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
_fun(s::SymbolFun) = _mod(MOI, s.s)

_field(s::SymbolFS) = Symbol(lowercase(string(s.s)))

function _getC(s::SymbolSet)
    if s.typed
        :(MOIU.C{F, $(_set(s)){T}})
    else
        :(MOIU.C{F, $(_set(s))})
    end
end
function _getC(s::SymbolFun)
    if s.typed
        :($(_fun(s)){T})
    else
        _fun(s)
    end
end


_getCV(s::SymbolSet) = :($(_getC(s))[])
_getCV(s::SymbolFun) = :($(s.cname){T, $(_getC(s))}())

_getlocfield(s::SymbolFS) = :(MOIU._getloc(m.$(_field(s))))

"""
    macro instance(instancename, scalarsets, typedscalarsets, vectorsets, typedvectorsets, scalarfunctions, vectorfunctions)

Creates a type Instance implementing the MOI instance interface and containing `scalarsets` scalar sets `typedscalarsets` typed scalar sets, `vectorsets` vector sets, `typedvectorsets` typed vector sets, `scalarfunctions` scalar functions and `vectorfunctions` vector functions.
To give no set/function, write `()`, to give one set `S`, write `(S,)`.

### Examples

The instance describing an linear program would be:
```
@instance Instance () (EqualTo, GreaterThan, LessThan, Interval) (Zeros, Nonnegatives, Nonpositives) () (SingleVariable, ScalarAffineFunction) (VectorOfVariables, VectorAffineFunction)
```
"""
macro instance(instancename, ss, sst, vs, vst, sf, sft, vf, vft)
    scalarsets = [SymbolSet.(ss.args, false); SymbolSet.(sst.args, true)]
    vectorsets = [SymbolSet.(vs.args, false); SymbolSet.(vst.args, true)]

    scname = Symbol(string(instancename) * "ScalarConstraints")
    vcname = Symbol(string(instancename) * "VectorConstraints")

    scalarfuns = [SymbolFun.(sf.args, false, scname); SymbolFun.(sft.args, true, scname)]
    vectorfuns = [SymbolFun.(vf.args, false, vcname); SymbolFun.(vft.args, true, vcname)]
    funs = [scalarfuns; vectorfuns]

    scalarconstraints = :(struct $scname{T, F<:MOI.AbstractScalarFunction} <: MOIU.Constraints{F}; end)
    vectorconstraints = :(struct $vcname{T, F<:MOI.AbstractVectorFunction} <: MOIU.Constraints{F}; end)
    for (c, ss) in ((scalarconstraints, scalarsets), (vectorconstraints, vectorsets))
        for s in ss
            field = _field(s)
            push!(c.args[3].args, :($field::Vector{$(_getC(s))}))
        end
    end

    instancedef = quote
        mutable struct $instancename{T} <: MOIU.AbstractInstance{T}
            sense::MOI.OptimizationSense
            objective::MOIU.SAF{T}
            nvars::UInt64
            nconstrs::UInt64
            constrmap::Vector{Int} # Constraint Reference value ci -> index in array in Constraints
        end
    end
    for f in funs
        cname = f.cname
        field = _field(f)
        push!(instancedef.args[2].args[3].args, :($field::$cname{T, $(_getC(f))}))
    end

    code = :()
    for (func, T) in ((:_addconstraint!, CR), (:_modifyconstraint!, CR), (:_delete!, CR), (:_getfunction, CR), (:_getset, CR), (:_getnoc, MOI.NumberOfConstraints))
        funct = _mod(MOIU, func)
        for (c, ss) in ((scname, scalarsets), (vcname, vectorsets))
            for s in ss
                set = _set(s)
                field = _field(s)
                code = quote
                    $code
                    $funct{F}(m::$c, cr::$T{F, <:$set}, args...) = $funct(m.$field, cr, args...)
                end
            end
        end

        for f in funs
            fun = _fun(f)
            field = _field(f)
            code = quote
                $code
                $funct(m::$instancename, cr::$T{<:$fun}, args...) = $funct(m.$field, cr, args...)
            end
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

        $instancedef
        function $instancename{T}() where T
            $instancename{T}(MOI.FeasibilitySense, MOIU.SAF{T}(MOI.VariableReference[], T[], zero(T)),
                   0, 0, Int[],
                   $(_getCV.(funs)...))
        end

        function MOI.getattribute(m::$instancename, loc::MOI.ListOfConstraints)
            vcat($(_getlocfield.(funs)...))
        end

        $code

    end)
end
