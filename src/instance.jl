const C{F, S} = Tuple{CI{F, S}, F, S}

const EMPTYSTRING = ""

# Implementation of MOI for vector of constraint
function _addconstraint!{F, S}(constrs::Vector{C{F, S}}, ci::CI, f::F, s::S)
    push!(constrs, (ci, f, s))
    length(constrs)
end

function _delete!(constrs::Vector, ci::CI, i::Int)
    deleteat!(constrs, i)
    @view constrs[i:end] # will need to shift it in constrmap
end

_getindex(ci::CI, f::MOI.AbstractFunction, s::MOI.AbstractSet) = ci
function _getindex(constrs::Vector, ci::CI, i::Int)
    _getindex(constrs[i]...)
end

_getfun(ci::CI, f::MOI.AbstractFunction, s::MOI.AbstractSet) = f
function _getfunction(constrs::Vector, ci::CI, i::Int)
    @assert ci.value == constrs[i][1].value
    _getfun(constrs[i]...)
end

_gets(ci::CI, f::MOI.AbstractFunction, s::MOI.AbstractSet) = s
function _getset(constrs::Vector, ci::CI, i::Int)
    @assert ci.value == constrs[i][1].value
    _gets(constrs[i]...)
end

_modifyconstr{F, S}(ci::CI{F, S}, f::F, s::S, change::F) = (ci, change, s)
_modifyconstr{F, S}(ci::CI{F, S}, f::F, s::S, change::S) = (ci, f, change)
_modifyconstr{F, S}(ci::CI{F, S}, f::F, s::S, change::MOI.AbstractFunctionModification) = (ci, modifyfunction(f, change), s)
function _modifyconstraint!{F, S}(constrs::Vector{C{F, S}}, ci::CI{F}, i::Int, change)
    constrs[i] = _modifyconstr(constrs[i]..., change)
end

_getnoc{F, S}(constrs::Vector{C{F, S}}, noc::MOI.NumberOfConstraints{F, S}) = length(constrs)
# Might be called when calling NumberOfConstraint with different coefficient type than the one supported
_getnoc(constrs::Vector, noc::MOI.NumberOfConstraints) = 0

function _getloc{F, S}(constrs::Vector{C{F, S}})::Vector{Tuple{DataType, DataType}}
    isempty(constrs) ? [] : [(F, S)]
end

_getlocr(constrs::Vector{C{F, S}}, ::MOI.ListOfConstraintIndices{F, S}) where {F, S} = map(constr -> constr[1], constrs)
_getlocr(constrs::Vector{<:C}, ::MOI.ListOfConstraintIndices{F, S}) where {F, S} = CI{F, S}[]

# Implementation of MOI for AbstractInstance
abstract type AbstractInstance{T} <: MOI.AbstractStandaloneInstance end

getconstrloc(m::AbstractInstance, ci::CI) = m.constrmap[ci.value]

# Variables
MOI.get(m::AbstractInstance, ::MOI.NumberOfVariables) = length(m.varindices)
function MOI.addvariable!(m::AbstractInstance)
    v = VI(m.nextvariableid += 1)
    push!(m.varindices, v)
    v
end
function MOI.addvariables!(m::AbstractInstance, n::Integer)
    [MOI.addvariable!(m) for i in 1:n]
end

function _removevar(ci::CI, f, s, vi::VI)
    (ci, removevariable(f, vi), s)
end
function _removevar(ci::CI, f::MOI.VectorOfVariables, s, vi::VI)
    g = removevariable(f, vi)
    if length(g.variables) != length(f.variables)
        t = updatedimension(s, length(g.variables))
    else
        t = s
    end
    (ci, g, t)
end
function _removevar!(constrs::Vector, vi::VI)
    for i in eachindex(constrs)
        constrs[i] = _removevar(constrs[i]..., vi)
    end
    []
end
function _removevar!(constrs::Vector{<:C{MOI.SingleVariable}}, vi::VI)
    # If a variable is removed, the SingleVariable constraints using this variable
    # need to be removed too
    rm = []
    for (ci, f, s) in constrs
        if f.variable == vi
            push!(rm, ci)
        end
    end
    rm
end
function MOI.delete!(m::AbstractInstance, vi::VI)
    m.objective = removevariable(m.objective, vi)
    rm = broadcastvcat(constrs -> _removevar!(constrs, vi), m)
    for ci in rm
        MOI.delete!(m, ci)
    end
    delete!(m.varindices, vi)
    if haskey(m.varnames, vi)
        delete!(m.namesvar, m.varnames[vi])
        delete!(m.varnames, vi)
    end
end

function MOI.isvalid(m::AbstractInstance, ci::CI{F, S}) where {F, S}
    if ci.value > length(m.constrmap)
        false
    else
        loc = getconstrloc(m, ci)
        if iszero(loc) # This means that it has been deleted
            false
        elseif loc > MOI.get(m, MOI.NumberOfConstraints{F, S}())
            false
        else
            ci == _getindex(m, ci, getconstrloc(m, ci))
        end
    end
end
MOI.isvalid(m::AbstractInstance, vi::VI) = in(vi, m.varindices)

MOI.get(m::AbstractInstance, ::MOI.ListOfVariableIndices) = collect(m.varindices)
MOI.canget(m::AbstractInstance, ::MOI.ListOfVariableIndices) = true

# Names
MOI.canset(m::AbstractInstance, ::MOI.VariableName, vi::VI) = MOI.isvalid(m, vi)
function MOI.set!(m::AbstractInstance, ::MOI.VariableName, vi::VI, name::String)
    m.varnames[vi] = name
    m.namesvar[name] = vi
end
MOI.canget(m::AbstractInstance, ::MOI.VariableName, ::VI) = true
MOI.get(m::AbstractInstance, ::MOI.VariableName, vi::VI) = get(m.varnames, vi, EMPTYSTRING)

MOI.canget(m::AbstractInstance, ::Type{VI}, name::String) = haskey(m.namesvar, name)
MOI.get(m::AbstractInstance, ::Type{VI}, name::String) = m.namesvar[name]

function MOI.get(instance::AbstractInstance, ::MOI.ListOfVariableAttributesSet)::Vector{MOI.AbstractVariableAttribute}
    isempty(instance.varnames) ? [] : [MOI.VariableName()]
end

MOI.canset(m::AbstractInstance, ::MOI.ConstraintName, ::CI) = true
function MOI.set!(m::AbstractInstance, ::MOI.ConstraintName, ci::CI, name::String)
    m.connames[ci] = name
    m.namescon[name] = ci
end
MOI.canget(m::AbstractInstance, ::MOI.ConstraintName, ::CI) = true
MOI.get(m::AbstractInstance, ::MOI.ConstraintName, ci::CI) = get(m.connames, ci, EMPTYSTRING)

MOI.canget(m::AbstractInstance, ::Type{<:CI}, name::String) = haskey(m.namescon, name)
MOI.get(m::AbstractInstance, ::Type{<:CI}, name::String) = m.namescon[name]

function MOI.get(instance::AbstractInstance, ::MOI.ListOfConstraintAttributesSet)::Vector{MOI.AbstractConstraintAttribute}
    isempty(instance.connames) ? [] : [MOI.ConstraintName()]
end

# Objective
MOI.get(m::AbstractInstance, ::MOI.ObjectiveSense) = m.sense
MOI.canset(m::AbstractInstance, ::MOI.ObjectiveSense) = true
function MOI.set!(m::AbstractInstance, ::MOI.ObjectiveSense, sense::MOI.OptimizationSense)
    m.sense = sense
end
MOI.get(m::AbstractInstance, ::MOI.ObjectiveFunction) = m.objective
MOI.canset(m::AbstractInstance, ::MOI.ObjectiveFunction) = true
function MOI.set!(m::AbstractInstance, ::MOI.ObjectiveFunction, f::MOI.AbstractFunction)
    # f needs to be copied, see #2
    m.objective = deepcopy(f)
end

MOI.canmodifyobjective(m::AbstractInstance, change::MOI.AbstractFunctionModification) = true
function MOI.modifyobjective!(m::AbstractInstance, change::MOI.AbstractFunctionModification)
    m.objective = modifyfunction(m.objective, change)
end

function MOI.get(m::AbstractInstance, ::MOI.ListOfInstanceAttributesSet)::Vector{MOI.AbstractInstanceAttribute}
    [MOI.ObjectiveSense(), MOI.ObjectiveFunction()]
end

# Constraints
MOI.canaddconstraint(instance::AbstractInstance, f::MOI.AbstractFunction, s::MOI.AbstractSet) = false
function MOI.addconstraint!(m::AbstractInstance, f::F, s::S) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    # We give the index value `nextconstraintid + 1` to the new constraint.
    # As the same counter is used for all pairs of F-in-S constraints,
    # the index value is unique across all constraint types as mentionned in `@instance`'s doc.
    ci = CI{F, S}(m.nextconstraintid += 1)
    # f needs to be copied, see #2
    push!(m.constrmap, _addconstraint!(m, ci, deepcopy(f), deepcopy(s)))
    ci
end

MOI.candelete(m::AbstractInstance, i::MOI.Index) = MOI.isvalid(m, i)
function MOI.delete!(m::AbstractInstance, ci::CI)
    for (ci_next, _, _) in _delete!(m, ci, getconstrloc(m, ci))
        m.constrmap[ci_next.value] -= 1
    end
    m.constrmap[ci.value] = 0
    if haskey(m.connames, ci)
        delete!(m.namescon, m.connames[ci])
        delete!(m.connames, ci)
    end
end

MOI.canmodifyconstraint(m::AbstractInstance, ci::CI, change) = true
function MOI.modifyconstraint!(m::AbstractInstance, ci::CI, change)
    _modifyconstraint!(m, ci, getconstrloc(m, ci), change)
end

MOI.get(m::AbstractInstance, noc::MOI.NumberOfConstraints) = _getnoc(m, noc)

function MOI.get(m::AbstractInstance, loc::MOI.ListOfConstraints)
    broadcastvcat(_getloc, m)
end

function MOI.get(m::AbstractInstance, loc::MOI.ListOfConstraintIndices)
    broadcastvcat(constrs -> _getlocr(constrs, loc), m)
end

MOI.canget(m::AbstractInstance, ::Union{MOI.NumberOfVariables,
                                        MOI.NumberOfConstraints,
                                        MOI.ListOfConstraints,
                                        MOI.ListOfConstraintIndices,
                                        MOI.ObjectiveFunction,
                                        MOI.ObjectiveSense}) = true

MOI.canset(m::AbstractInstance, ::Union{MOI.ObjectiveFunction,
                                        MOI.ObjectiveSense}) = true

MOI.canget(m::AbstractInstance, ::Union{MOI.ConstraintFunction,
                                        MOI.ConstraintSet}, ::Type{<:MOI.Index}) = true

function MOI.get(m::AbstractInstance, ::MOI.ConstraintFunction, ci::CI)
    _getfunction(m, ci, getconstrloc(m, ci))
end

function MOI.get(m::AbstractInstance, ::MOI.ConstraintSet, ci::CI)
    _getset(m, ci, getconstrloc(m, ci))
end

function MOI.isempty(m::AbstractInstance)
    m.sense == MOI.FeasibilitySense &&
    isempty(m.objective.variables) && isempty(m.objective.coefficients) && iszero(m.objective.constant) &&
    iszero(m.nextvariableid) && iszero(m.nextconstraintid)
end

MOI.copy!(dest::AbstractInstance, src::MOI.AbstractInstance) = defaultcopy!(dest, src)

# Can be used to access constraints of an instance
"""
broadcastcall(f::Function, m::AbstractInstance)

Calls `f(contrs)` for every vector `constrs::Vector{ConstraintIndex{F, S}, F, S}` of the instance.

# Examples

To add all constraints of the instance to a solver `solver`, one can do
```julia
_addcon(solver, ci, f, s) = MOI.addconstraint!(solver, f, s)
function _addcon(solver, constrs::Vector)
    for constr in constrs
        _addcon(solver, constr...)
    end
end
MOIU.broadcastcall(constrs -> _addcon(solver, constrs), instance)
```
"""
function broadcastcall end
"""
broadcastvcat(f::Function, m::AbstractInstance)

Calls `f(contrs)` for every vector `constrs::Vector{ConstraintIndex{F, S}, F, S}` of the instance and concatenate the results with `vcat` (this is used internally for `ListOfConstraints`).

# Examples

To get the list of all functions:
```julia
_getfun(ci, f, s) = f
_getfun(cindices::Tuple) = _getfun(cindices...)
_getfuns(constrs::Vector) = _getfun.(constrs)
MOIU.broadcastvcat(_getfuns, instance)
"""
function broadcastvcat end

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

# (MOI, :Zeros) -> :(MOI.Zeros)
_mod(m::Module, s::Symbol) = Expr(:., m, :($(QuoteNode(s))))
# (:Zeros) -> :(MOI.Zeros)
_moi(s::Symbol) = _mod(MOI, s)
_set(s::SymbolSet) = _moi(s.s)
_fun(s::SymbolFun) = _moi(s.s)
function _typedset(s::SymbolSet)
    if s.typed
        :($(_set(s)){T})
    else
        _set(s)
    end
end
function _typedfun(s::SymbolFun)
    if s.typed
        :($(_fun(s)){T})
    else
        _fun(s)
    end
end

# Base.lowercase is moved to Unicode.lowercase in Julia v0.7
if VERSION >= v"0.7.0-DEV.2813"
    using Unicode
end
_field(s::SymbolFS) = Symbol(lowercase(string(s.s)))

_getC(s::SymbolSet) = :($MOIU.C{F, $(_typedset(s))})
_getC(s::SymbolFun) = _typedfun(s)

_getCV(s::SymbolSet) = :($(_getC(s))[])
_getCV(s::SymbolFun) = :($(s.cname){T, $(_getC(s))}())

_callfield(f, s::SymbolFS) = :($f(m.$(_field(s))))
_broadcastfield(b, s::SymbolFS) = :($b(f, m.$(_field(s))))

"""
macro instance(instancename, scalarsets, typedscalarsets, vectorsets, typedvectorsets, scalarfunctions, typedscalarfunctions, vectorfunctions, typedvectorfunctions)

Creates a type `instancename` implementing the MOI instance interface and containing `scalarsets` scalar sets `typedscalarsets` typed scalar sets, `vectorsets` vector sets, `typedvectorsets` typed vector sets, `scalarfunctions` scalar functions, `typedscalarfunctions` typed scalar functions, `vectorfunctions` vector functions and `typedvectorfunctions` typed vector functions.
To give no set/function, write `()`, to give one set `S`, write `(S,)`.

This implementation of the MOI instance certifies that the constraint indices, in addition to being different between constraints `F`-in-`S` for the same types `F` and `S`,
are also different between constraints for different types `F` and `S`.
This means that for constraint indices `ci1`, `ci2` of this instance, `ci1 == ci2` if and only if `ci1.value == ci2.value`.
This fact can be used to use the the value of the index directly in a dictionary representing a mapping between constraint indices and something else.

### Examples

The instance describing an linear program would be:
```julia
@instance LPInstance () (EqualTo, GreaterThan, LessThan, Interval) (Zeros, Nonnegatives, Nonpositives) () (SingleVariable,) (ScalarAffineFunction,) (VectorOfVariables,) (VectorAffineFunction,)
```

Let `MOI` denote `MathOptInterface`, `MOIU` denote `MathOptInterfaceUtilities` and `MOIU.C{F, S}` be defined as `MOI.Tuple{CI{F, S}, F, S}`.
The macro would create the types:
```julia
struct LPInstanceScalarConstraints{T, F <: MOI.AbstractScalarFunction} <: MOIU.Constraints{F}
    equalto::Vector{MOIU.C{F, MOI.EqualTo{T}}}
    greaterthan::Vector{MOIU.C{F, MOI.GreaterThan{T}}}
    lessthan::Vector{MOIU.C{F, MOI.LessThan{T}}}
    interval::Vector{MOIU.C{F, MOI.Interval{T}}}
end
struct LPInstanceVectorConstraints{T, F <: MOI.AbstractVectorFunction} <: MOIU.Constraints{F}
    zeros::Vector{MOIU.C{F, MOI.Zeros}}
    nonnegatives::Vector{MOIU.C{F, MOI.Nonnegatives}}
    nonpositives::Vector{MOIU.C{F, MOI.Nonpositives}}
end
mutable struct LPInstance{T} <: MOIU.AbstractInstance{T}
    sense::MOI.OptimizationSense
    objective::Union{MOI.SingleVariable, MOI.ScalarAffineFunction{T}, MOI.ScalarQuadraticFunction{T}}
    nextvariableid::Int64
    varindices::Vector{MOI.VariableIndex}
    varnames::Dict{MOI.VariableIndex, String}
    namesvar::Dict{String, Int64}
    nextconstraintid::Int64
    connames::Dict{MOI.ConstraintIndex, String}
    namescon::Dict{String, MOI.ConstraintIndex}
    constrmap::Vector{Int}
    singlevariable::LPInstanceScalarConstraints{T, MOI.SingleVariable}
    scalaraffinefunction::LPInstanceScalarConstraints{T, MOI.ScalarAffineFunction{T}}
    vectorofvariables::LPInstanceVectorConstraints{T, MOI.VectorOfVariables}
    vectoraffinefunction::LPInstanceVectorConstraints{T, MOI.VectorAffineFunction{T}}
end
```
The type `LPInstance` implements the MathOptInterface API except methods specific to solver instances like `optimize!` or `getattribute` with `VariablePrimal`.
"""
macro instance(instancename, ss, sst, vs, vst, sf, sft, vf, vft)
    scalarsets = [SymbolSet.(ss.args, false); SymbolSet.(sst.args, true)]
    vectorsets = [SymbolSet.(vs.args, false); SymbolSet.(vst.args, true)]

    scname = Symbol(string(instancename) * "ScalarConstraints")
    vcname = Symbol(string(instancename) * "VectorConstraints")

    scalarfuns = [SymbolFun.(sf.args, false, scname); SymbolFun.(sft.args, true, scname)]
    vectorfuns = [SymbolFun.(vf.args, false, vcname); SymbolFun.(vft.args, true, vcname)]
    funs = [scalarfuns; vectorfuns]

    scalarconstraints = :(struct $scname{T, F<:$MOI.AbstractScalarFunction} <: $MOIU.Constraints{F}; end)
    vectorconstraints = :(struct $vcname{T, F<:$MOI.AbstractVectorFunction} <: $MOIU.Constraints{F}; end)
    for (c, ss) in ((scalarconstraints, scalarsets), (vectorconstraints, vectorsets))
        for s in ss
            field = _field(s)
            push!(c.args[3].args, :($field::Vector{$(_getC(s))}))
        end
    end

    instancedef = quote
        mutable struct $instancename{T} <: $MOIU.AbstractInstance{T}
            sense::$MOI.OptimizationSense
            objective::Union{$MOI.SingleVariable, $MOI.ScalarAffineFunction{T}, $MOI.ScalarQuadraticFunction{T}}
            nextvariableid::Int64
            varindices::Set{$VI}
            varnames::Dict{$VI, String}
            namesvar::Dict{String, $VI}
            nextconstraintid::Int64
            connames::Dict{$CI, String}
            namescon::Dict{String, $CI}
            constrmap::Vector{Int} # Constraint Reference value ci -> index in array in Constraints
        end
    end
    for f in funs
        cname = f.cname
        field = _field(f)
        push!(instancedef.args[2].args[3].args, :($field::$cname{T, $(_getC(f))}))
    end

    code = quote
        function MathOptInterfaceUtilities.broadcastcall(f::Function, m::$instancename)
            $(Expr(:block, _broadcastfield.(:(MathOptInterfaceUtilities.broadcastcall), funs)...))
        end
        function MathOptInterfaceUtilities.broadcastvcat(f::Function, m::$instancename)
            vcat($(_broadcastfield.(:(MathOptInterfaceUtilities.broadcastvcat), funs)...))
        end
        function $MOI.empty!(m::$instancename{T}) where T
            m.sense = $MOI.FeasibilitySense
            m.objective = $SAF{T}($VI[], T[], zero(T))
            m.nextvariableid = 0
            m.varindices = Set{$VI}()
            m.varnames = Dict{Int64, String}()
            m.namesvar = Dict{String, $VI}()
            m.nextconstraintid = 0
            m.connames = Dict{Int64, String}()
            m.namescon = Dict{String, $CI}()
            m.constrmap = Int[]
            $(Expr(:block, _callfield.(:($MOI.empty!), funs)...))
        end
    end
    for (cname, sets) in ((scname, scalarsets), (vcname, vectorsets))
        code = quote
            $code
            function MathOptInterfaceUtilities.broadcastcall(f::Function, m::$cname)
                $(Expr(:block, _callfield.(:f, sets)...))
            end
            function MathOptInterfaceUtilities.broadcastvcat(f::Function, m::$cname)
                vcat($(_callfield.(:f, sets)...))
            end
            function $MOI.empty!(m::$cname)
                $(Expr(:block, _callfield.(:(Base.empty!), sets)...))
            end
        end
    end

    for (func, T) in ((:_addconstraint!, CI), (:_modifyconstraint!, CI), (:_delete!, CI), (:_getindex, CI), (:_getfunction, CI), (:_getset, CI), (:_getnoc, MOI.NumberOfConstraints))
        funct = _mod(MathOptInterfaceUtilities, func)
        for (c, ss) in ((scname, scalarsets), (vcname, vectorsets))
            for s in ss
                set = _set(s)
                field = _field(s)
                code = quote
                    $code
                    $funct{F}(m::$c, ci::$T{F, <:$set}, args...) = $funct(m.$field, ci, args...)
                end
            end
        end

        for f in funs
            fun = _fun(f)
            field = _field(f)
            code = quote
                $code
                $funct(m::$instancename, ci::$T{<:$fun}, args...) = $funct(m.$field, ci, args...)
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

        $instancedef
        function $instancename{T}() where T
            $instancename{T}($MOI.FeasibilitySense, $SAF{T}($VI[], T[], zero(T)),
                   0, Set{$VI}(), Dict{$VI, String}(), Dict{String, $VI}(),
                   0, Dict{$CI, String}(), Dict{String, $CI}(), Int[],
                   $(_getCV.(funs)...))
        end

        MOI.canaddconstraint(instance::$instancename{T}, f::Union{$(_typedfun.(scalarfuns)...)}, s::Union{$(_typedset.(scalarsets)...)}) where T = true
        MOI.canaddconstraint(instance::$instancename{T}, f::Union{$(_typedfun.(vectorfuns)...)}, s::Union{$(_typedset.(vectorsets)...)}) where T = true

        $code

    end)
end
