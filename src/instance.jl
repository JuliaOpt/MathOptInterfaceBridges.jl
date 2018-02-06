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

getconstrloc(instance::AbstractInstance, ci::CI) = instance.constrmap[ci.value]

# Variables
MOI.get(instance::AbstractInstance, ::MOI.NumberOfVariables) = length(instance.varindices)
function MOI.addvariable!(instance::AbstractInstance)
    v = VI(instance.nextvariableid += 1)
    push!(instance.varindices, v)
    v
end
function MOI.addvariables!(instance::AbstractInstance, n::Integer)
    [MOI.addvariable!(instance) for i in 1:n]
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
function MOI.delete!(instance::AbstractInstance, vi::VI)
    instance.objective = removevariable(instance.objective, vi)
    rm = broadcastvcat(constrs -> _removevar!(constrs, vi), instance)
    for ci in rm
        MOI.delete!(instance, ci)
    end
    delete!(instance.varindices, vi)
    if haskey(instance.varnames, vi)
        delete!(instance.namesvar, instance.varnames[vi])
        delete!(instance.varnames, vi)
    end
end

function MOI.isvalid(instance::AbstractInstance, ci::CI{F, S}) where {F, S}
    if ci.value > length(instance.constrmap)
        false
    else
        loc = getconstrloc(instance, ci)
        if iszero(loc) # This means that it has been deleted
            false
        elseif loc > MOI.get(instance, MOI.NumberOfConstraints{F, S}())
            false
        else
            ci == _getindex(instance, ci, getconstrloc(instance, ci))
        end
    end
end
MOI.isvalid(instance::AbstractInstance, vi::VI) = in(vi, instance.varindices)

MOI.get(instance::AbstractInstance, ::MOI.ListOfVariableIndices) = collect(instance.varindices)

# Names
MOI.canset(instance::AbstractInstance, ::MOI.VariableName, vi::Type{VI}) = true
function MOI.set!(instance::AbstractInstance, ::MOI.VariableName, vi::VI, name::String)
    if !isempty(name) && haskey(instance.namesvar, name) && instance.namesvar[name] != vi
        error("Variable name $name is already used by $(instance.namesvar[name])")
    end
    instance.varnames[vi] = name
    instance.namesvar[name] = vi
end
MOI.set!(instance::AbstractInstance, ::MOI.VariableName, vi::Vector{VI}, name::Vector{String}) = MOI.set!.(instance, MOI.VariableName(), vi, name)
MOI.canget(instance::AbstractInstance, ::MOI.VariableName, ::Type{VI}) = true
MOI.get(instance::AbstractInstance, ::MOI.VariableName, vi::VI) = get(instance.varnames, vi, EMPTYSTRING)
MOI.get(instance::AbstractInstance, ::MOI.VariableName, vi::Vector{VI}) = MOI.get.(instance, MOI.VariableName(), vi)

MOI.canget(instance::AbstractInstance, ::Type{VI}, name::String) = haskey(instance.namesvar, name)
MOI.get(instance::AbstractInstance, ::Type{VI}, name::String) = instance.namesvar[name]

function MOI.get(instance::AbstractInstance, ::MOI.ListOfVariableAttributesSet)::Vector{MOI.AbstractVariableAttribute}
    isempty(instance.varnames) ? [] : [MOI.VariableName()]
end

MOI.canset(instance::AbstractInstance, ::MOI.ConstraintName, ::Type{<:CI}) = true
function MOI.set!(instance::AbstractInstance, ::MOI.ConstraintName, ci::CI, name::String)
    if !isempty(name) && haskey(instance.namescon, name) && instance.namescon[name] != ci
        error("Constraint name $name is already used by $(instance.namescon[name])")
    end
    instance.connames[ci] = name
    instance.namescon[name] = ci
end
MOI.set!(instance::AbstractInstance, ::MOI.ConstraintName, ci::Vector{<:CI}, name::Vector{String}) = MOI.set!.(instance, MOI.ConstraintName(), ci, name)
MOI.canget(instance::AbstractInstance, ::MOI.ConstraintName, ::Type{<:CI}) = true
MOI.get(instance::AbstractInstance, ::MOI.ConstraintName, ci::CI) = get(instance.connames, ci, EMPTYSTRING)
MOI.get(instance::AbstractInstance, ::MOI.ConstraintName, ci::Vector{<:CI}) = MOI.get.(instance, MOI.ConstraintName(), ci)

MOI.canget(instance::AbstractInstance, ::Type{<:CI}, name::String) = haskey(instance.namescon, name)
MOI.get(instance::AbstractInstance, ::Type{<:CI}, name::String) = instance.namescon[name]

function MOI.get(instance::AbstractInstance, ::MOI.ListOfConstraintAttributesSet)::Vector{MOI.AbstractConstraintAttribute}
    isempty(instance.connames) ? [] : [MOI.ConstraintName()]
end

# Objective
MOI.get(instance::AbstractInstance, ::MOI.ObjectiveSense) = instance.sense
MOI.canset(instance::AbstractInstance, ::MOI.ObjectiveSense) = true
function MOI.set!(instance::AbstractInstance, ::MOI.ObjectiveSense, sense::MOI.OptimizationSense)
    instance.sense = sense
end
MOI.get(instance::AbstractInstance, ::MOI.ObjectiveFunction) = instance.objective
MOI.canset(instance::AbstractInstance, ::MOI.ObjectiveFunction) = true
function MOI.set!(instance::AbstractInstance, ::MOI.ObjectiveFunction, f::MOI.AbstractFunction)
    # f needs to be copied, see #2
    instance.objective = deepcopy(f)
end

MOI.canmodifyobjective(instance::AbstractInstance, ::Type{<:MOI.AbstractFunctionModification}) = true
function MOI.modifyobjective!(instance::AbstractInstance, change::MOI.AbstractFunctionModification)
    instance.objective = modifyfunction(instance.objective, change)
end

function MOI.get(instance::AbstractInstance, ::MOI.ListOfInstanceAttributesSet)::Vector{MOI.AbstractInstanceAttribute}
    [MOI.ObjectiveSense(), MOI.ObjectiveFunction{typeof(instance.objective)}()]
end

# Constraints
function MOI.addconstraint!(instance::AbstractInstance, f::F, s::S) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    # We give the index value `nextconstraintid + 1` to the new constraint.
    # As the same counter is used for all pairs of F-in-S constraints,
    # the index value is unique across all constraint types as mentionned in `@instance`'s doc.
    ci = CI{F, S}(instance.nextconstraintid += 1)
    # f needs to be copied, see #2
    push!(instance.constrmap, _addconstraint!(instance, ci, deepcopy(f), deepcopy(s)))
    ci
end

MOI.candelete(instance::AbstractInstance, i::MOI.Index) = MOI.isvalid(instance, i)
function MOI.delete!(instance::AbstractInstance, ci::CI)
    for (ci_next, _, _) in _delete!(instance, ci, getconstrloc(instance, ci))
        instance.constrmap[ci_next.value] -= 1
    end
    instance.constrmap[ci.value] = 0
    if haskey(instance.connames, ci)
        delete!(instance.namescon, instance.connames[ci])
        delete!(instance.connames, ci)
    end
end

MOI.canmodifyconstraint(instance::AbstractInstance, ci::CI, change) = true
function MOI.modifyconstraint!(instance::AbstractInstance, ci::CI, change)
    _modifyconstraint!(instance, ci, getconstrloc(instance, ci), change)
end

MOI.get(instance::AbstractInstance, noc::MOI.NumberOfConstraints) = _getnoc(instance, noc)

function MOI.get(instance::AbstractInstance, loc::MOI.ListOfConstraints)
    broadcastvcat(_getloc, instance)
end

function MOI.get(instance::AbstractInstance, loc::MOI.ListOfConstraintIndices)
    broadcastvcat(constrs -> _getlocr(constrs, loc), instance)
end

MOI.canget(instance::AbstractInstance, ::Union{MOI.NumberOfVariables,
                                               MOI.ListOfVariableIndices,
                                               MOI.NumberOfConstraints,
                                               MOI.ListOfConstraints,
                                               MOI.ListOfConstraintIndices,
                                               MOI.ObjectiveFunction,
                                               MOI.ObjectiveSense}) = true

MOI.canget(instance::AbstractInstance, ::Union{MOI.ConstraintFunction,
                                               MOI.ConstraintSet}, ::Type{<:MOI.Index}) = true

function MOI.get(instance::AbstractInstance, ::MOI.ConstraintFunction, ci::CI)
    _getfunction(instance, ci, getconstrloc(instance, ci))
end

function MOI.get(instance::AbstractInstance, ::MOI.ConstraintSet, ci::CI)
    _getset(instance, ci, getconstrloc(instance, ci))
end

function MOI.isempty(instance::AbstractInstance)
    instance.sense == MOI.FeasibilitySense &&
    isempty(instance.objective.variables) && isempty(instance.objective.coefficients) && iszero(instance.objective.constant) &&
    iszero(instance.nextvariableid) && iszero(instance.nextconstraintid)
end

MOI.copy!(dest::AbstractInstance, src::MOI.AbstractInstance) = defaultcopy!(dest, src)

# Can be used to access constraints of an instance
"""
broadcastcall(f::Function, instance::AbstractInstance)

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
broadcastvcat(f::Function, instance::AbstractInstance)

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

_callfield(f, s::SymbolFS) = :($f(instance.$(_field(s))))
_broadcastfield(b, s::SymbolFS) = :($b(f, instance.$(_field(s))))

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
    namesvar::Dict{String, MOI.VariableIndex}
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
        function MathOptInterfaceUtilities.broadcastcall(f::Function, instance::$instancename)
            $(Expr(:block, _broadcastfield.(:(MathOptInterfaceUtilities.broadcastcall), funs)...))
        end
        function MathOptInterfaceUtilities.broadcastvcat(f::Function, instance::$instancename)
            vcat($(_broadcastfield.(:(MathOptInterfaceUtilities.broadcastvcat), funs)...))
        end
        function $MOI.empty!(instance::$instancename{T}) where T
            instance.sense = $MOI.FeasibilitySense
            instance.objective = $SAF{T}($VI[], T[], zero(T))
            instance.nextvariableid = 0
            instance.varindices = Set{$VI}()
            instance.varnames = Dict{Int64, String}()
            instance.namesvar = Dict{String, $VI}()
            instance.nextconstraintid = 0
            instance.connames = Dict{Int64, String}()
            instance.namescon = Dict{String, $CI}()
            instance.constrmap = Int[]
            $(Expr(:block, _callfield.(:($MOI.empty!), funs)...))
        end
    end
    for (cname, sets) in ((scname, scalarsets), (vcname, vectorsets))
        code = quote
            $code
            function MathOptInterfaceUtilities.broadcastcall(f::Function, instance::$cname)
                $(Expr(:block, _callfield.(:f, sets)...))
            end
            function MathOptInterfaceUtilities.broadcastvcat(f::Function, instance::$cname)
                vcat($(_callfield.(:f, sets)...))
            end
            function $MOI.empty!(instance::$cname)
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
                    $funct{F}(instance::$c, ci::$T{F, <:$set}, args...) = $funct(instance.$field, ci, args...)
                end
            end
        end

        for f in funs
            fun = _fun(f)
            field = _field(f)
            code = quote
                $code
                $funct(instance::$instancename, ci::$T{<:$fun}, args...) = $funct(instance.$field, ci, args...)
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

        MOI.canaddconstraint(instance::$instancename{T}, ::Type{<:Union{$(_typedfun.(scalarfuns)...)}}, ::Type{<:Union{$(_typedset.(scalarsets)...)}}) where T = true
        MOI.canaddconstraint(instance::$instancename{T}, ::Type{<:Union{$(_typedfun.(vectorfuns)...)}}, ::Type{<:Union{$(_typedset.(vectorsets)...)}}) where T = true

        $code

    end)
end
