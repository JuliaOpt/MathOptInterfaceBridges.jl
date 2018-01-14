export @bridge

"""
    AbstractBridge

A bridge represents a bridged constraint in an `AbstractBridgeInstance`. It contains the indices of the constraints that it has created in the instance.
These can be obtained using `MOI.NumberOfConstraints` and `MOI.ListOfConstraintIndices` and using the bridge in place of a `AbstractInstance`.
Attributes of the bridged instance such as `MOI.ConstraintDual` and `MOI.ConstraintPrimal`, can be obtained using the bridge in place of the constraint index.
These calls are used by the `AbstractBridgeInstance` to communicate with the bridge so they should be implemented by the bridge.
"""
abstract type AbstractBridge end

"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfVariables)

The number of variables created by the bridge `b` in the instance.
"""
MOI.get(b::AbstractBridge, ::MOI.NumberOfVariables) = 0
"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfConstraints{F, S}) where {F, S}

The number of constraints of the type `F`-in-`S` created by the bridge `b` in the instance.
"""
MOI.get(b::AbstractBridge, ::MOI.NumberOfConstraints) = 0
"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfConstraints{F, S}) where {F, S}

A `Vector{ConstraintIndex{F,S}}` with indices of all constraints of
type `F`-in`S` created by the bride `b` in the instance (i.e., of length equal to the value of `NumberOfConstraints{F,S}()`).
"""
MOI.get(b::AbstractBridge, ::MOI.ListOfConstraintIndices{F, S}) where {F, S} = CI{F, S}[]

"""
    MOI.candelete(instance::MOI.AbstractInstance, b::AbstractBridge)

Return a `Bool` indicating whether the bridge `b` can be removed from the instance `instance`.
"""
MOI.candelete(instance::MOI.AbstractInstance, c::AbstractBridge) = true

const InstanceConstraintAttribute = Union{MOI.ConstraintName, MOI.ConstraintFunction, MOI.ConstraintSet}
const SolverConstraintAttribute = Union{MOI.ConstraintPrimalStart, MOI.ConstraintDualStart, MOI.ConstraintPrimal, MOI.ConstraintDual, MOI.ConstraintBasisStatus}

abstract type AbstractBridgeInstance <: MOI.AbstractSolverInstance end
bridge(b::AbstractBridgeInstance, ci::CI) = b.bridges[ci.value]
MOI.optimize!(b::AbstractBridgeInstance) = MOI.optimize!(b.instance)

# References
MOI.candelete(b::AbstractBridgeInstance, r::MOI.Index) = MOI.candelete(b.instance, r)
MOI.isvalid(b::AbstractBridgeInstance, r::MOI.Index) = MOI.isvalid(b.instance, r)
MOI.delete!(b::AbstractBridgeInstance, r::MOI.Index) = MOI.delete!(b.instance, r)

# Attributes
function MOI.get(b::AbstractBridgeInstance, loc::MOI.ListOfConstraintIndices)
    locr = MOI.get(b.instance, loc)
    for bridge in values(b.bridges)
        for c in MOI.get(bridge, loc)
            i = findfirst(locr, c)
            if !iszero(i)
                MOI.deleteat!(locr, i)
            end
        end
    end
    locr
end
function MOI.get(b::AbstractBridgeInstance, attr::Union{MOI.NumberOfConstraints, MOI.NumberOfVariables})
    s = MOI.get(b.instance, attr)
    for v in values(b.bridges)
        s -= MOI.get(v, attr)
    end
    s
end
MOI.canget(b::AbstractBridgeInstance, attr::MOI.ListOfConstraints) = MOI.canget(b.instance, attr) && MOI.canget(b.bridged, attr)
_noc(b, fs) = MOI.get(b, MOI.NumberOfConstraints{fs...}())
function MOI.get(b::AbstractBridgeInstance, attr::MOI.ListOfConstraints)
    loc = MOI.get(b.instance, attr)
    rm = find(_noc.(b, loc) .== 0)
    deleteat!(loc, rm)
    append!(loc, MOI.get(b.bridged, attr))
end
for f in (:canget, :canset, :set!, :get, :get!)
    @eval begin
        MOI.$f(b::AbstractBridgeInstance, attr::MOI.AnyAttribute) = MOI.$f(b.instance, attr)
        # Objective function
        MOI.$f(b::AbstractBridgeInstance, attr::MOI.AnyAttribute, arg::Union{MOI.OptimizationSense, MOI.AbstractScalarFunction}) = MOI.$f(b.instance, attr, arg)
    end
end
for f in (:canget, :canset)
    @eval begin
        MOI.$f(b::AbstractBridgeInstance, attr::MOI.AnyAttribute, index::Type{<:MOI.Index}) = MOI.$f(b.instance, attr, index)
    end
end
for f in (:set!, :get, :get!)
    @eval begin
        MOI.$f(b::AbstractBridgeInstance, attr::MOI.AnyAttribute, index::MOI.Index) = MOI.$f(b.instance, attr, index)
        MOI.$f(b::AbstractBridgeInstance, attr::MOI.AnyAttribute, indices::Vector{<:MOI.Index}) = MOI.$f(b.instance, attr, indices)
    end
end

# Constraints
MOI.canaddconstraint(b::AbstractBridgeInstance, f::MOI.AbstractFunction, s::MOI.AbstractSet) = MOI.canaddconstraint(b.instance, f, s)
function MOI.addconstraint!(b::AbstractBridgeInstance, f::MOI.AbstractFunction, s::MOI.AbstractSet)
    MOI.addconstraint!(b.instance, f, s)
end
MOI.canmodifyconstraint(b::AbstractBridgeInstance, ci::CI, change) = MOI.canmodifyconstraint(b.instance, ci, change)
MOI.modifyconstraint!(b::AbstractBridgeInstance, ci::CI, change) = MOI.modifyconstraint!(b.instance, ci, change)

# Objective
MOI.canmodifyobjective(b::AbstractBridgeInstance, change::MOI.AbstractFunctionModification) = MOI.canmodifyobjective(b.instance, change)
MOI.modifyobjective!(b::AbstractBridgeInstance, change::MOI.AbstractFunctionModification) = MOI.modifyobjective!(b.instance, change)

# Variables
MOI.addvariable!(b::AbstractBridgeInstance) = MOI.addvariable!(b.instance)
MOI.addvariables!(b::AbstractBridgeInstance, n) = MOI.addvariables!(b.instance, n)

function _mois(t)
    _moi.(t.args)
end

"""
macro bridge(instancename, bridge, scalarsets, typedscalarsets, vectorsets, typedvectorsets, scalarfunctions, typedscalarfunctions, vectorfunctions, typedvectorfunctions)

Creates a type `instancename` implementing the MOI instance interface and bridging the `scalarsets` scalar sets `typedscalarsets` typed scalar sets, `vectorsets` vector sets, `typedvectorsets` typed vector sets, `scalarfunctions` scalar functions, `typedscalarfunctions` typed scalar functions, `vectorfunctions` vector functions and `typedvectorfunctions` typed vector functions.
To give no set/function, write `()`, to give one set `S`, write `(S,)`.

### Examples

The instance bridging the constraints `ScalarAffineFunction in Interval` is created as follows:
```julia
@bridge SplitInterval MOIU.SplitIntervalBridge () (Interval,) () () () (ScalarAffineFunction,) () ()
```
Given an instance `instance` implementing `ScalarAffineFunction in GreaterThan` and `ScalarAffineFunction in LessThan`, the instance
```
bridgedinstance = SplitInterval(instance)
```
will additionally support `ScalarAffineFunction in Interval`
"""
macro bridge(instancename, bridge, ss, sst, vs, vst, sf, sft, vf, vft)
    bridgedinstancename = Symbol(string(instancename) * "Instance")
    bridgedfuns = :(Union{$(_mois(sf)...), $(_mois(sft)...), $(_mois(vf)...), $(_mois(vft)...)})
    bridgedsets = :(Union{$(_mois(ss)...), $(_mois(sst)...), $(_mois(vs)...), $(_mois(vst)...)})

    # Attributes
    attributescode = :()

    for f in (:canget, :get)
        attributescode = quote
            $attributescode

            function $MOI.$f(b::$instancename, attr::Union{$MOI.ListOfConstraintIndices{<:$bridgedfuns, <:$bridgedsets}, $MOI.NumberOfConstraints{<:$bridgedfuns, <:$bridgedsets}})
                $MOI.$f(b.bridged, attr)
            end
        end
    end

    for f in (:canget, :canset)
        attributescode = quote
            $attributescode

            function $MOI.$f(b::$instancename, attr::$MOIU.InstanceConstraintAttribute, ci::Type{$CI{<:$bridgedfuns, <:$bridgedsets}})
                $MOI.$f(b.bridged, attr, ci)
            end
            function $MOI.$f(b::$instancename, attr::$MOIU.SolverConstraintAttribute, ci::Type{$CI{<:$bridgedfuns, <:$bridgedsets}})
                $MOI.$f(b.instance, attr, $MOIU.bridge(b, ci))
            end
        end
    end

    for f in (:set!, :get, :get!)
        attributescode = quote
            $attributescode

            function $MOI.$f(b::$instancename, attr::$MOIU.InstanceConstraintAttribute, ci::$CI{<:$bridgedfuns, <:$bridgedsets})
                $MOI.$f(b.bridged, attr, ci)
            end
            function $MOI.$f(b::$instancename, attr::$MOIU.SolverConstraintAttribute, ci::$CI{<:$bridgedfuns, <:$bridgedsets})
                $MOI.$f(b.instance, attr, $MOIU.bridge(b, ci))
            end
        end
    end

    esc(quote
        $MOIU.@instance $bridgedinstancename $ss $sst $vs $vst $sf $sft $vf $vft

        struct $instancename{T, IT<:$MOI.AbstractInstance} <: $MOIU.AbstractBridgeInstance
            instance::IT
            bridged::$bridgedinstancename{T}
            bridges::Dict{Int64, $bridge{T}}
            function $instancename{T}(instance::IT) where {T, IT <: $MOI.AbstractInstance}
                new{T, IT}(instance, $bridgedinstancename{T}(), Dict{Int64, $bridge{T}}())
            end
        end

        # References
        $MOI.candelete(b::$instancename{T}, ci::$CI{<:$bridgedfuns, <:$bridgedsets}) where T = $MOI.candelete(b.bridged, ci) && $MOI.candelete(b.instance, $MOIU.bridge(b, ci))

        $MOI.isvalid(b::$instancename{T}, ci::$CI{<:$bridgedfuns, <:$bridgedsets}) where T = $MOI.isvalid(b.bridged, ci)

        function $MOI.delete!(b::$instancename{T}, ci::$CI{<:$bridgedfuns, <:$bridgedsets}) where T
            $MOI.delete!(b.instance, $MOIU.bridge(b, ci))
            delete!(b.instance, ci.value)
            $MOI.delete!(b.bridged, ci)
        end

        $attributescode

        # Constraints
        $MOI.canaddconstraint(b::$instancename, f::$bridgedfuns, s::$bridgedsets) = $MOI.canaddconstraint(b.bridged, f, s)
        function $MOI.addconstraint!(b::$instancename{T}, f::$bridgedfuns, s::$bridgedsets) where T
            ci = $MOI.addconstraint!(b.bridged, f, s)
            @assert !haskey(b.bridges, ci.value)
            b.bridges[ci.value] = $bridge{T}(b.instance, f, s)
            ci
        end
        function $MOI.canmodifyconstraint(b::$instancename, ci::$CI{<:$bridgedfuns, <:$bridgedsets}, change)
            $MOI.canmodifyconstraint(b.bridged, ci, change) && $MOI.canmodifyconstraint(b.instance, $MOIU.bridge(b, ci), change)
        end
        function $MOI.modifyconstraint!(b::$instancename, ci::$CI{<:$bridgedfuns, <:$bridgedsets}, change)
            $MOI.modifyconstraint!(b.instance, $MOIU.bridge(b, ci), change)
            $MOI.modifyconstraint!(b.bridged, ci, change)
        end
    end)
end
