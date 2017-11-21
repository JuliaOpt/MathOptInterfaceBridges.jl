export @bridge

"""
    AbstractBridge

A bridge represents a bridged constraint in an `AbstractBridgeInstance`. It contains the references towards the constraints that it has created in the instance.
These can be obtained using `MOI.NumberOfConstraints` and `MOI.ListOfConstraintReferences` and using the bridge in place of a `AbstractInstance`.
Attributes of the bridged instance such as `MOI.ConstraintDual` and `MOI.ConstraintPrimal`, can be obtained using the bridge in place of the constraint reference.
These call are used by the `AbstractBridgeInstance` to communicate with the bridge so they should be implemented by the bridge.
"""
abstract type AbstractBridge end

"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfConstraints{F, S}) where {F, S}

The number of constraints of the type `F`-in-`S` created by the bridge `b` in the instance.
"""
MOI.get(b::AbstractBridge, ::MOI.NumberOfConstraints) = 0
"""
    MOI.get(b::AbstractBridge, ::MOI.NumberOfConstraints{F, S}) where {F, S}

A `Vector{ConstraintReferences{F,S}}` with references to all constraints of
type `F`-in`S` created by the bride `b` in the instance (i.e., of length equal to the value of `NumberOfConstraints{F,S}()`).
"""
MOI.get(b::AbstractBridge, ::MOI.ListOfConstraintReferences{F, S}) where {F, S} = CR{F, S}[]

"""
    MOI.candelete(instance::MOI.AbstractInstance, b::AbstractBridge)

Return a `Bool` indicating whether the bridge `b` can be removed from the instance `instance`.
"""
MOI.candelete(instance::MOI.AbstractInstance, c::AbstractBridge) = true

const InstanceConstraintAttribute = Union{MOI.ConstraintName, MOI.ConstraintFunction, MOI.ConstraintSet}
const SolverConstraintAttribute = Union{MOI.ConstraintPrimalStart, MOI.ConstraintDualStart, MOI.ConstraintPrimal, MOI.ConstraintDual, MOI.ConstraintBasisStatus}

abstract type AbstractBridgeInstance <: MOI.AbstractSolverInstance end
bridge(b::AbstractBridgeInstance, cr::CR) = b.bridges[cr.value]
MOI.optimize!(b::AbstractBridgeInstance) = MOI.optimize!(b.instance)

# References
MOI.candelete(b::AbstractBridgeInstance, r::MOI.AnyReference) = MOI.candelete(b.instance, r)
MOI.isvalid(b::AbstractBridgeInstance, r::MOI.AnyReference) = MOI.isvalid(b.instance, r)
MOI.delete!(b::AbstractBridgeInstance, r::MOI.AnyReference) = MOI.delete!(b.instance, r)

# Attributes
function MOI.get(b::AbstractBridgeInstance, loc::MOI.ListOfConstraintReferences)
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
function MOI.get(b::AbstractBridgeInstance, attr::MOI.NumberOfConstraints)
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
        MOI.$f(b::AbstractBridgeInstance, attr::MOI.AnyAttribute, ref::MOI.AnyReference) = MOI.$f(b.instance, attr, ref)
        MOI.$f(b::AbstractBridgeInstance, attr::MOI.AnyAttribute, refs::Vector{<:MOI.AnyReference}) = MOI.$f(b.instance, attr, refs)
        # Objective function
        MOI.$f(b::AbstractBridgeInstance, attr::MOI.AnyAttribute, arg::Union{MOI.OptimizationSense, MOI.AbstractScalarFunction}) = MOI.$f(b.instance, attr, arg)
    end
end

# Constraints
MOI.canaddconstraint(b::AbstractBridgeInstance, f::MOI.AbstractFunction, s::MOI.AbstractSet) = MOI.canaddconstraint(b.instance, f, s)
MOI.addconstraint!(b::AbstractBridgeInstance, f::MOI.AbstractFunction, s::MOI.AbstractSet) = MOI.addconstraint!(b.instance, f, s)
MOI.canmodifyconstraint(b::AbstractBridgeInstance, cr::CR, change) = MOI.canmodifyconstraint(b.instance, cr, change)
MOI.modifyconstraint!(b::AbstractBridgeInstance, cr::CR, change) = MOI.modifyconstraint!(b.instance, cr, change)

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

            function MOI.$f(b::$instancename, attr::Union{$MOI.ListOfConstraintReferences{<:$bridgedfuns, <:$bridgedsets}, $MOI.NumberOfConstraints{<:$bridgedfuns, <:$bridgedsets}})
                MOI.$f(b.bridged, attr)
            end
        end
    end

    for f in (:canget, :canset, :set!, :get, :get!)
        attributescode = quote
            $attributescode

            function MOI.$f(b::$instancename, attr::$MOIU.InstanceConstraintAttribute, cr::$CR{<:$bridgedfuns, <:$bridgedsets})
                MOI.$f(b.bridged, attr, cr)
            end
            function MOI.$f(b::$instancename, attr::$MOIU.SolverConstraintAttribute, cr::$CR{<:$bridgedfuns, <:$bridgedsets})
                MOI.$f(b.instance, attr, $MOIU.bridge(b, cr))
            end
        end
    end

    esc(quote
        $MOIU.@instance $bridgedinstancename $ss $sst $vs $vst $sf $sft $vf $vft

        struct $instancename{T, IT<:$MOI.AbstractInstance} <: $MOIU.AbstractBridgeInstance
            instance::IT
            bridged::$bridgedinstancename{T}
            bridges::Dict{UInt64, $bridge{T}}
            function $instancename{T}(instance::IT) where {T, IT <: MOI.AbstractInstance}
                new{T, IT}(instance, $bridgedinstancename{T}(), Dict{UInt64, $bridge{T}}())
            end
        end

        # References
        MOI.candelete(b::$instancename{T}, cr::$CR{<:$bridgedfuns, <:$bridgedsets}) where T = MOI.candelete(b.bridged, cr) && MOI.candelete(b.instance, $MOIU.bridge(b, cr))

        MOI.isvalid(b::$instancename{T}, cr::$CR{<:$bridgedfuns, <:$bridgedsets}) where T = MOI.isvalid(b.bridged, cr)

        function MOI.delete!(b::$instancename{T}, cr::$CR{<:$bridgedfuns, <:$bridgedsets}) where T
            MOI.delete!(b.instance, $MOIU.bridge(b, cr))
            delete!(b.instance, cr.value)
            MOI.delete!(b.bridged, cr)
        end

        $attributescode

        # Constraints
        $MOI.canaddconstraint(b::$instancename, f::$bridgedfuns, s::$bridgedsets) = $MOI.canaddconstraint(b.bridged, f, s)
        function $MOI.addconstraint!(b::$instancename, f::$bridgedfuns, s::$bridgedsets)
            cr = $MOI.addconstraint!(b.bridged, f, s)
            @assert !haskey(b.bridges, cr.value)
            b.bridges[cr.value] = $bridge(b.instance, f, s)
            cr
        end
        function $MOI.canmodifyconstraint(b::$instancename, cr::$CR{<:$bridgedfuns, <:$bridgedsets}, change)
            $MOI.canmodifyconstraint(b.bridged, cr, change) && $MOI.canmodifyconstraint(b.instance, $MOIU.bridge(b, cr), change)
        end
        function $MOI.modifyconstraint!(b::$instancename, cr::$CR{<:$bridgedfuns, <:$bridgedsets}, change)
            $MOI.modifyconstraint!(b.instance, $MOIU.bridge(b, cr), change)
            $MOI.modifyconstraint!(b.bridged, cr, change)
        end
    end)
end
