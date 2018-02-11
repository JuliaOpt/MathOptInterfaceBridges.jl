export @bridge

"""
    AbstractBridge

A bridge represents a bridged constraint in an `AbstractBridgeOptimizer`. It contains the indices of the constraints that it has created in the model.
These can be obtained using `MOI.NumberOfConstraints` and `MOI.ListOfConstraintIndices` and using the bridge in place of a `ModelLike`.
Attributes of the bridged model such as `MOI.ConstraintDual` and `MOI.ConstraintPrimal`, can be obtained using the bridge in place of the constraint index.
These calls are used by the `AbstractBridgeOptimizer` to communicate with the bridge so they should be implemented by the bridge.
"""
abstract type AbstractBridge end

"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfVariables)

The number of variables created by the bridge `b` in the model.
"""
MOI.get(b::AbstractBridge, ::MOI.NumberOfVariables) = 0
"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfConstraints{F, S}) where {F, S}

The number of constraints of the type `F`-in-`S` created by the bridge `b` in the model.
"""
MOI.get(b::AbstractBridge, ::MOI.NumberOfConstraints) = 0
"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfConstraints{F, S}) where {F, S}

A `Vector{ConstraintIndex{F,S}}` with indices of all constraints of
type `F`-in`S` created by the bride `b` in the model (i.e., of length equal to the value of `NumberOfConstraints{F,S}()`).
"""
MOI.get(b::AbstractBridge, ::MOI.ListOfConstraintIndices{F, S}) where {F, S} = CI{F, S}[]

"""
    MOI.candelete(model::MOI.ModelLike, b::AbstractBridge)

Return a `Bool` indicating whether the bridge `b` can be removed from the model `model`.
"""
MOI.candelete(model::MOI.ModelLike, c::AbstractBridge) = true

const InstanceConstraintAttribute = Union{MOI.ConstraintName, MOI.ConstraintFunction, MOI.ConstraintSet}
const SolverConstraintAttribute = Union{MOI.ConstraintPrimalStart, MOI.ConstraintDualStart, MOI.ConstraintPrimal, MOI.ConstraintDual, MOI.ConstraintBasisStatus}

abstract type AbstractBridgeOptimizer <: MOI.AbstractOptimizer end
bridge(b::AbstractBridgeOptimizer, ci::CI) = b.bridges[ci.value]
MOI.optimize!(b::AbstractBridgeOptimizer) = MOI.optimize!(b.model)

MOI.isempty(b::AbstractBridgeOptimizer) = isempty(b.bridges) && MOI.isempty(b.model)
function MOI.empty!(b::AbstractBridgeOptimizer)
    MOI.empty!(b.model)
    MOI.empty!(b.bridged)
    empty!(b.bridges)
end
function MOI.copy!(b::AbstractBridgeOptimizer, src::MOI.ModelLike)
    if needsallocateload(b.model)
        allocateload!(b, src)
    else
        defaultcopy!(b, src)
    end
end

# References
MOI.candelete(b::AbstractBridgeOptimizer, r::MOI.Index) = MOI.candelete(b.model, r)
MOI.isvalid(b::AbstractBridgeOptimizer, r::MOI.Index) = MOI.isvalid(b.model, r)
MOI.delete!(b::AbstractBridgeOptimizer, r::MOI.Index) = MOI.delete!(b.model, r)

# Attributes
function MOI.get(b::AbstractBridgeOptimizer, loc::MOI.ListOfConstraintIndices)
    locr = MOI.get(b.model, loc)
    for bridge in values(b.bridges)
        for c in MOI.get(bridge, loc)
            i = findfirst(locr, c)
            if (VERSION >= v"0.7.0-DEV.3395" && i !== nothing) || (VERSION < v"0.7.0-DEV.3395" && !iszero(i))
                MOI.deleteat!(locr, i)
            end
        end
    end
    locr
end
function MOI.get(b::AbstractBridgeOptimizer, attr::Union{MOI.NumberOfConstraints, MOI.NumberOfVariables})
    s = MOI.get(b.model, attr)
    for v in values(b.bridges)
        s -= MOI.get(v, attr)
    end
    s
end
MOI.canget(b::AbstractBridgeOptimizer, attr::MOI.ListOfConstraints) = MOI.canget(b.model, attr) && MOI.canget(b.bridged, attr)
_noc(b, fs) = MOI.get(b, MOI.NumberOfConstraints{fs...}())
function MOI.get(b::AbstractBridgeOptimizer, attr::MOI.ListOfConstraints)
    loc = MOI.get(b.model, attr)
    rm = find(_noc.(b, loc) .== 0)
    deleteat!(loc, rm)
    append!(loc, MOI.get(b.bridged, attr))
end
for f in (:canget, :canset, :set!, :get, :get!)
    @eval begin
        MOI.$f(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractModelAttribute, MOI.AbstractOptimizerAttribute}) = MOI.$f(b.model, attr)
        # Objective function
        MOI.$f(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractModelAttribute, MOI.AbstractOptimizerAttribute}, arg::Union{MOI.OptimizationSense, MOI.AbstractScalarFunction}) = MOI.$f(b.model, attr, arg)
    end
end
for f in (:canget, :canset)
    @eval begin
        MOI.$f(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, index::Type{<:MOI.Index}) = MOI.$f(b.model, attr, index)
    end
end
for f in (:set!, :get, :get!)
    @eval begin
        MOI.$f(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, index::MOI.Index) = MOI.$f(b.model, attr, index)
        MOI.$f(b::AbstractBridgeOptimizer, attr::Union{MOI.AbstractVariableAttribute, MOI.AbstractConstraintAttribute}, indices::Vector{<:MOI.Index}) = MOI.$f(b.model, attr, indices)
    end
end

# Constraints
MOI.canaddconstraint(b::AbstractBridgeOptimizer, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet} = MOI.canaddconstraint(b.model, F, S)
function MOI.addconstraint!(b::AbstractBridgeOptimizer, f::MOI.AbstractFunction, s::MOI.AbstractSet)
    MOI.addconstraint!(b.model, f, s)
end
MOI.canmodifyconstraint(b::AbstractBridgeOptimizer, ci::CI, change) = MOI.canmodifyconstraint(b.model, ci, change)
MOI.modifyconstraint!(b::AbstractBridgeOptimizer, ci::CI, change) = MOI.modifyconstraint!(b.model, ci, change)

# Objective
MOI.canmodifyobjective(b::AbstractBridgeOptimizer, ::Type{M}) where M<:MOI.AbstractFunctionModification = MOI.canmodifyobjective(b.model, M)
MOI.modifyobjective!(b::AbstractBridgeOptimizer, change::MOI.AbstractFunctionModification) = MOI.modifyobjective!(b.model, change)

# Variables
MOI.canaddvariable(b::AbstractBridgeOptimizer) = MOI.canaddvariable(b.model)
MOI.addvariable!(b::AbstractBridgeOptimizer) = MOI.addvariable!(b.model)
MOI.addvariables!(b::AbstractBridgeOptimizer, n) = MOI.addvariables!(b.model, n)

function _mois(t)
    _moi.(t.args)
end

"""
macro bridge(modelname, bridge, scalarsets, typedscalarsets, vectorsets, typedvectorsets, scalarfunctions, typedscalarfunctions, vectorfunctions, typedvectorfunctions)

Creates a type `modelname` implementing the MOI model interface and bridging the `scalarsets` scalar sets `typedscalarsets` typed scalar sets, `vectorsets` vector sets, `typedvectorsets` typed vector sets, `scalarfunctions` scalar functions, `typedscalarfunctions` typed scalar functions, `vectorfunctions` vector functions and `typedvectorfunctions` typed vector functions.
To give no set/function, write `()`, to give one set `S`, write `(S,)`.

### Examples

The model bridging the constraints `ScalarAffineFunction in Interval` is created as follows:
```julia
@bridge SplitInterval MOIU.SplitIntervalBridge () (Interval,) () () () (ScalarAffineFunction,) () ()
```
Given an model `model` implementing `ScalarAffineFunction in GreaterThan` and `ScalarAffineFunction in LessThan`, the model
```
bridgedmodel = SplitInterval(model)
```
will additionally support `ScalarAffineFunction in Interval`
"""
macro bridge(modelname, bridge, ss, sst, vs, vst, sf, sft, vf, vft)
    bridgedmodelname = Symbol(string(modelname) * "Instance")
    bridgedfuns = :(Union{$(_mois(sf)...), $(_mois(sft)...), $(_mois(vf)...), $(_mois(vft)...)})
    bridgedsets = :(Union{$(_mois(ss)...), $(_mois(sst)...), $(_mois(vs)...), $(_mois(vst)...)})

    # Attributes
    attributescode = :()

    for f in (:canget, :get)
        attributescode = quote
            $attributescode

            function $MOI.$f(b::$modelname, attr::Union{$MOI.ListOfConstraintIndices{<:$bridgedfuns, <:$bridgedsets}, $MOI.NumberOfConstraints{<:$bridgedfuns, <:$bridgedsets}})
                $MOI.$f(b.bridged, attr)
            end
        end
    end

    for f in (:canget, :canset)
        attributescode = quote
            $attributescode

            function $MOI.$f(b::$modelname, attr::$MOIU.InstanceConstraintAttribute, ci::Type{$CI{<:$bridgedfuns, <:$bridgedsets}})
                $MOI.$f(b.bridged, attr, ci)
            end
            function $MOI.$f(b::$modelname, attr::$MOIU.SolverConstraintAttribute, ci::Type{$CI{<:$bridgedfuns, <:$bridgedsets}})
                $MOI.$f(b.model, attr, $MOIU.bridge(b, ci))
            end
        end
    end

    for f in (:set!, :get, :get!)
        attributescode = quote
            $attributescode

            function $MOI.$f(b::$modelname, attr::$MOIU.InstanceConstraintAttribute, ci::$CI{<:$bridgedfuns, <:$bridgedsets})
                $MOI.$f(b.bridged, attr, ci)
            end
            function $MOI.$f(b::$modelname, attr::$MOIU.SolverConstraintAttribute, ci::$CI{<:$bridgedfuns, <:$bridgedsets})
                $MOI.$f(b.model, attr, $MOIU.bridge(b, ci))
            end
        end
    end

    esc(quote
        $MOIU.@model $bridgedmodelname $ss $sst $vs $vst $sf $sft $vf $vft

        struct $modelname{T, IT<:$MOI.ModelLike} <: $MOIU.AbstractBridgeOptimizer
            model::IT
            bridged::$bridgedmodelname{T}
            bridges::Dict{Int64, $bridge{T}}
            function $modelname{T}(model::IT) where {T, IT <: $MOI.ModelLike}
                new{T, IT}(model, $bridgedmodelname{T}(), Dict{Int64, $bridge{T}}())
            end
        end

        # References
        $MOI.candelete(b::$modelname{T}, ci::$CI{<:$bridgedfuns, <:$bridgedsets}) where T = $MOI.candelete(b.bridged, ci) && $MOI.candelete(b.model, $MOIU.bridge(b, ci))

        $MOI.isvalid(b::$modelname{T}, ci::$CI{<:$bridgedfuns, <:$bridgedsets}) where T = $MOI.isvalid(b.bridged, ci)

        function $MOI.delete!(b::$modelname{T}, ci::$CI{<:$bridgedfuns, <:$bridgedsets}) where T
            $MOI.delete!(b.model, $MOIU.bridge(b, ci))
            delete!(b.model, ci.value)
            $MOI.delete!(b.bridged, ci)
        end

        $attributescode

        # Constraints
        $MOI.canaddconstraint(b::$modelname, ::Type{F}, ::Type{S}) where {F<:$bridgedfuns, S<:$bridgedsets} = $MOI.canaddconstraint(b.bridged, F, S)
        function $MOI.addconstraint!(b::$modelname{T}, f::$bridgedfuns, s::$bridgedsets) where T
            ci = $MOI.addconstraint!(b.bridged, f, s)
            @assert !haskey(b.bridges, ci.value)
            b.bridges[ci.value] = $bridge{T}(b.model, f, s)
            ci
        end
        function $MOI.canmodifyconstraint(b::$modelname, ci::$CI{<:$bridgedfuns, <:$bridgedsets}, change)
            $MOI.canmodifyconstraint(b.bridged, ci, change) && $MOI.canmodifyconstraint(b.model, $MOIU.bridge(b, ci), change)
        end
        function $MOI.modifyconstraint!(b::$modelname, ci::$CI{<:$bridgedfuns, <:$bridgedsets}, change)
            $MOI.modifyconstraint!(b.model, $MOIU.bridge(b, ci), change)
            $MOI.modifyconstraint!(b.bridged, ci, change)
        end
    end)
end
