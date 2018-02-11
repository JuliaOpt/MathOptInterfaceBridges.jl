
@enum CachingOptimizerState NoOptimizer EmptyOptimizer AttachedOptimizer
@enum CachingOptimizerMode Manual Automatic

# TODO: Benchmark to check if CachingOptimizer should be parameterized on the ModelLike type.

"""
    CachingOptimizer

`CachingOptimizer` is an intermediate layer that stores a cache of the model
and links it with an optimizer. It supports incremental model
construction and modification even when the optimizer doesn't.

A `CachingOptimizer` may be in one of three possible states (`CachingOptimizerState`):

- `NoOptimizer`: The CachingOptimizer does not have any optimizer.
- `EmptyOptimizer`: The CachingOptimizer an empty optimizer. The optimizer is not synchronized with the cached model.
- `AttachedOptimizer`: The CachingOptimizer a optimizer, and it is synchronized with the cached model.

A `CachingOptimizer` has two modes of operation (`CachingOptimizerMode`):

- `Manual`: The only methods that change the state of the `CachingOptimizer` are [`resetoptimizer!`](@ref), [`dropoptimizer!`](@ref), and [`attachoptimizer!`](@ref). Attempting to perform an operation in the incorrect state results in an error.
- `Automatic`: The `CachingOptimizer` changes its state when necessary. For example, `optimize!` will automatically call `attachoptimizer!` (an optimizer must have been previously set). Attempting to add a constraint or perform a modification not supported by the optimizer results in a drop to `EmptyOptimizer` mode.
"""
mutable struct CachingOptimizer <: MOI.AbstractOptimizer
    model_cache::MOI.ModelLike
    optimizer::Union{Void,MOI.AbstractOptimizer}
    state::CachingOptimizerState
    mode::CachingOptimizerMode
    model_to_optimizer_map::IndexMap
    optimizer_to_model_map::IndexMap
    # CachingOptimizer externally uses the same variable and constraint indices
    # as the model_cache. model_to_optimizer_map maps from the model_cache indices to the
    # optimizer indices.
end

CachingOptimizer(model_cache::MOI.ModelLike, mode::CachingOptimizerMode) = CachingOptimizer(model_cache, nothing, NoOptimizer, mode, IndexMap(), IndexMap())

"""
    CachingOptimizer(model_cache::MOI.ModelLike, optimizer::AbstractOptimizer)

Creates an `CachingOptimizer` in `Automatic` mode, with the optimizer `optimizer`.
The model_cache manager returned behaves like an `AbstractOptimizer` as long as no
`CachingOptimizer`-specific functions (e.g. `dropoptimizer!`) are called on it.
"""
CachingOptimizer(model_cache::MOI.ModelLike, optimizer::MOI.AbstractOptimizer) = CachingOptimizer(model_cache, optimizer, AttachedOptimizer, Automatic, IndexMap(), IndexMap())

## Methods for managing the state of CachingOptimizer.

"""
    state(m::CachingOptimizer)::CachingOptimizerState

Returns the state of the CachingOptimizer `m`. See [`CachingOptimizer`](@ref).
"""
state(m::CachingOptimizer) = m.state

"""
    mode(m::CachingOptimizer)::CachingOptimizerMode

Returns the operating mode of the CachingOptimizer `m`. See [`CachingOptimizer`](@ref).
"""
mode(m::CachingOptimizer) = m.mode

"""
    resetoptimizer!(m::CachingOptimizer, optimizer::MOI.AbstractOptimizer)

Sets or resets `m` to have the given empty optimizer. Can be called
from any state. The `CachingOptimizer` will be in state `EmptyOptimizer` after the call.
"""
function resetoptimizer!(m::CachingOptimizer, optimizer::MOI.AbstractOptimizer)
    @assert MOI.isempty(optimizer)
    m.optimizer = optimizer
    m.state = EmptyOptimizer
    return
end

"""
    resetoptimizer!(m::CachingOptimizer)

Detaches and empties the current optimizer. Can be called from `AttachedOptimizer`
or `EmptyOptimizer` state. The `CachingOptimizer` will be in state `EmptyOptimizer`
after the call.
"""
function resetoptimizer!(m::CachingOptimizer)
    m.state == EmptyOptimizer && return
    @assert m.state == AttachedOptimizer
    MOI.empty!(m.optimizer)
    m.state = EmptyOptimizer
    return
end

"""
    dropoptimizer!(m::CachingOptimizer)

Drops the optimizer, if one is present. Can be called from any state.
The `CachingOptimizer` will be in state `NoOptimizer` after the call.
"""
function dropoptimizer!(m::CachingOptimizer)
    m.optimizer = nothing
    m.state = NoOptimizer
    return
end

"""
    attachoptimizer!(m::CachingOptimizer)

Attaches the optimizer to `m`, copying all model data into it. Can be called only
from the `EmptyOptimizer` state. The `CachingOptimizer` will be in state `AttachedOptimizer`
after the call. Returns an `MOI.CopyResult`. `MOI.CopySuccess` means that the
optimizer is correctly attached, otherwise the status indicates why the `copy!`
from the model cache to the optimizer failed.
"""
function attachoptimizer!(m::CachingOptimizer)
    @assert m.state == EmptyOptimizer
    copy_result = MOI.copy!(m.optimizer, m.model_cache)
    if copy_result.status != MOI.CopySuccess
        return copy_result
    end
    m.state = AttachedOptimizer
    # MOI does not define the type of index_map, so we have to copy it into a
    # concrete container. Also load the reverse map.
    m.model_to_optimizer_map = IndexMap()
    m.optimizer_to_model_map = IndexMap()
    for k in keys(copy_result.indexmap)
        m.model_to_optimizer_map[k] = copy_result.indexmap[k]
        m.optimizer_to_model_map[copy_result.indexmap[k]] = k
    end
    return copy_result
end

function MOI.empty!(m::CachingOptimizer)
    MOI.empty!(m.model_cache)
    if m.state == AttachedOptimizer
        MOI.empty!(m.optimizer)
    end
    if m.state == EmptyOptimizer && m.mode == Automatic
        m.state = AttachedOptimizer
    end
    m.model_to_optimizer_map = IndexMap()
    m.optimizer_to_model_map = IndexMap()
end
MOI.isempty(m::CachingOptimizer) = MOI.isempty(m.model_cache)

# Optimizing and adding/modifying constraints and variables.

function MOI.optimize!(m::CachingOptimizer)
    if m.mode == Automatic && m.state == EmptyOptimizer
        attachoptimizer!(m)
    end
    # TODO: better error message if no optimizer is set
    @assert m.state == AttachedOptimizer
    MOI.optimize!(m.optimizer)
end

function MOI.canaddvariable(m::CachingOptimizer)
    MOI.canaddvariable(m.model_cache) || return false
    if m.state == AttachedOptimizer && m.mode == Manual
        MOI.canaddvariable(m.optimizer) || return false
    end
    return true
end

function MOI.addvariable!(m::CachingOptimizer)
    # Same note as for addconstraint!
    if m.mode == Automatic && m.state == AttachedOptimizer && !MOI.canaddvariable(m.optimizer)
        resetoptimizer!(m)
    end
    vindex = MOI.addvariable!(m.model_cache)
    if m.state == AttachedOptimizer
        vindex_optimizer = MOI.addvariable!(m.optimizer)
        m.model_to_optimizer_map[vindex] = vindex_optimizer
        m.optimizer_to_model_map[vindex_optimizer] = vindex
    end
    return vindex
end

function MOI.addvariables!(m::CachingOptimizer, n)
    # Same note as for addconstraint!
    if m.mode == Automatic && m.state == AttachedOptimizer && !MOI.canaddvariable(m.optimizer)
        resetoptimizer!(m)
    end
    vindices = MOI.addvariables!(m.model_cache, n)
    if m.state == AttachedOptimizer
        vindices_optimizer = MOI.addvariables!(m.optimizer, n)
        for (vindex, vindex_optimizer) in zip(vindices, vindices_optimizer)
            m.model_to_optimizer_map[vindex] = vindex_optimizer
            m.optimizer_to_model_map[vindex_optimizer] = vindex
        end
    end
    return vindices
end

function MOI.canaddconstraint(m::CachingOptimizer, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    MOI.canaddconstraint(m.model_cache, F, S) || return false
    if m.state == AttachedOptimizer && m.mode == Manual
        MOI.canaddconstraint(m.optimizer, F, S) || return false
    end
    return true
end

function MOI.addconstraint!(m::CachingOptimizer, func::MOI.AbstractFunction, set::MOI.AbstractSet)
    # The canaddconstraint checks should catch most issues, but if an
    # addconstraint! call fails then the model_cache and the optimizer may no longer
    # be in sync.
    if m.mode == Automatic && m.state == AttachedOptimizer && !MOI.canaddconstraint(m.optimizer, typeof(func), typeof(set))
        resetoptimizer!(m)
    end
    @assert MOI.canaddconstraint(m, typeof(func), typeof(set))
    cindex = MOI.addconstraint!(m.model_cache, func, set)
    if m.state == AttachedOptimizer
        cindex_optimizer = MOI.addconstraint!(m.optimizer, mapvariables(m.model_to_optimizer_map,func), set)
        m.model_to_optimizer_map[cindex] = cindex_optimizer
        m.optimizer_to_model_map[cindex_optimizer] = cindex
    end
    return cindex
end

function MOI.canmodifyconstraint(m::CachingOptimizer, cindex::CI, change)
    MOI.canmodifyconstraint(m.model_cache, cindex, change) || return false
    if m.state == AttachedOptimizer && m.mode == Manual
        MOI.canmodifyconstraint(m.optimizer, m.model_to_optimizer_map[cindex], change) || return false
    end
    return true
end

function MOI.modifyconstraint!(m::CachingOptimizer, cindex::CI, change)
    if m.mode == Automatic && m.state == AttachedOptimizer && !MOI.canmodifyconstraint(m.optimizer, cindex, typeof(change))
        resetoptimizer!(m)
    end
    @assert MOI.canmodifyconstraint(m, cindex, typeof(change))
    MOI.modifyconstraint!(m.model_cache, cindex, change)
    if m.state == AttachedOptimizer
        MOI.modifyconstraint!(m.optimizer, m.model_to_optimizer_map[cindex], mapvariables(m.model_to_optimizer_map,change))
    end
    return
end

function MOI.modifyconstraint!(m::CachingOptimizer, cindex::CI{F,S}, set::S) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    if m.mode == Automatic && m.state == AttachedOptimizer && !MOI.canmodifyconstraint(m.optimizer, cindex, S)
        resetoptimizer!(m)
    end
    @assert MOI.canmodifyconstraint(m, cindex, S)
    MOI.modifyconstraint!(m.model_cache, cindex, set)
    if m.state == AttachedOptimizer
        MOI.modifyconstraint!(m.optimizer, m.model_to_optimizer_map[cindex], set)
    end
    return
end

function MOI.canmodifyobjective(m::CachingOptimizer, change)
    MOI.canmodifyobjective(m.model_cache, change) || return false
    if m.state == AttachedOptimizer && m.mode == Manual
        MOI.canmodifyobjective(m.optimizer, change) || return false
    end
    return true
end

function MOI.modifyobjective!(m::CachingOptimizer, change::MOI.AbstractFunctionModification)
    if m.mode == Automatic && m.state == AttachedOptimizer && !MOI.canmodifyobjective(m.optimizer, typeof(change))
        resetoptimizer!(m)
    end
    @assert MOI.canmodifyobjective(m, typeof(change))
    MOI.modifyobjective!(m.model_cache, change)
    if m.state == AttachedOptimizer
        MOI.modifyobjective!(m.optimizer, mapvariables(m.model_to_optimizer_map,change))
    end
    return
end

MOI.isvalid(m::CachingOptimizer, index::MOI.Index) = MOI.isvalid(m.model_cache, index)

function MOI.candelete(m::CachingOptimizer, index::MOI.Index)
    MOI.candelete(m.model_cache, index) || return false
    if m.state == AttachedOptimizer && m.mode == Manual
        MOI.candelete(m.optimizer, m.model_to_optimizer_map[index]) || return false
    end
    return true
end

function MOI.delete!(m::CachingOptimizer, index::MOI.Index)
    if m.mode == Automatic && m.state == AttachedOptimizer && !MOI.candelete(m.optimizer, index)
        resetoptimizer!(m)
    end
    @assert MOI.candelete(m, index)
    if m.state == AttachedOptimizer
        MOI.delete!(m.optimizer, m.model_to_optimizer_map[index])
        delete!(m.optimizer_to_model_map, m.model_to_optimizer_map[index])
        delete!(m.model_to_optimizer_map, index)
    end
    MOI.delete!(m.model_cache, index)
end


# TODO: addconstraints!, transformconstraint!, cantransformconstraint

## CachingOptimizer get and set attributes

# Attributes are mapped through attribute_value_map (defined in copy.jl) before
# they are sent to the optimizer and when they are returned from the optimizer.
# This map currently only translates indices on MOI.AbstractFunction objects
# between the optimizer indices and the (user-facing) model_cache indices. As a result,
# all MOI.AbstractFunctions must implement mapvariables. Other attributes that
# store indices need to be handled with care.

function MOI.set!(m::CachingOptimizer, attr::MOI.AbstractModelAttribute, value)
    # The canset checks should catch most issues, but if a set! call fails then
    # the model_cache and the optimizer may no longer be in sync.
    if m.mode == Automatic && m.state == AttachedOptimizer && !MOI.canset(m.optimizer, attr)
        resetoptimizer!(m)
    end
    @assert MOI.canset(m.model_cache, attr)
    if m.state == AttachedOptimizer
        @assert MOI.canset(m.optimizer, attr)
        MOI.set!(m.optimizer, attr, attribute_value_map(m.model_to_optimizer_map,value))
    end
    MOI.set!(m.model_cache, attr, value)
end

function MOI.set!(m::CachingOptimizer, attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}, index::MOI.Index, value)
    if m.mode == Automatic && m.state == AttachedOptimizer && !MOI.canset(m.optimizer, attr, typeof(index))
        resetoptimizer!(m)
    end
    @assert MOI.canset(m.model_cache, attr, typeof(index))
    if m.state == AttachedOptimizer
        @assert MOI.canset(m.optimizer, attr, typeof(index))
        MOI.set!(m.optimizer, attr, m.model_to_optimizer_map[index], attribute_value_map(m.model_to_optimizer_map,value))
    end
    MOI.set!(m.model_cache, attr, index, value)
end

# TODO: Automatic mode is broken in the case that the user tries to set
# an objective function of a type that's not supported by the optimizer.
# Two possible solutions are:
# 1. Let canset take a value argument.
# 2. Be more precise about what types are allowed in each attribute
# (https://github.com/JuliaOpt/MathOptInterface.jl/issues/31).
function MOI.canset(m::CachingOptimizer, attr::MOI.AbstractModelAttribute)
    MOI.canset(m.model_cache, attr) || return false
    if m.state == AttachedOptimizer && m.mode == Manual
        MOI.canset(m.optimizer, attr) || return false
    end
    return true
end

function MOI.canset(m::CachingOptimizer, attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}, idxtype::Type{<:MOI.Index})
    MOI.canset(m.model_cache, attr, idxtype) || return false
    if m.state == AttachedOptimizer && m.mode == Manual
        MOI.canset(m.optimizer, attr, idxtype) || return false
    end
    return true
end

function MOI.get(m::CachingOptimizer, attr::MOI.AbstractModelAttribute)
    if MOI.canget(m.model_cache, attr)
        return MOI.get(m.model_cache, attr)
    elseif m.state == AttachedOptimizer && MOI.canget(m.optimizer, attr)
        return attribute_value_map(m.optimizer_to_model_map,MOI.get(m.optimizer, attr))
    end
    error("Attribute $attr not accessible")
end

function MOI.get(m::CachingOptimizer, attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}, index::MOI.Index)
    if MOI.canget(m.model_cache, attr, typeof(index))
        return MOI.get(m.model_cache, attr, index)
    elseif m.state == AttachedOptimizer && MOI.canget(m.optimizer, attr, typeof(index))
        return attribute_value_map(m.optimizer_to_model_map,MOI.get(m.optimizer, attr, m.model_to_optimizer_map[index]))
    end
    error("Attribute $attr not accessible")
end

function MOI.get(m::CachingOptimizer, attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}, indices::Vector{<:MOI.Index})
    if MOI.canget(m.model_cache, attr, eltype(indices))
        return MOI.get(m.model_cache, attr, indices)
    elseif m.state == AttachedOptimizer && MOI.canget(m.optimizer, attr, eltype(indices))
        return attribute_value_map.(m.optimizer_to_model_map,MOI.get(m.optimizer, attr, getindex.(m.model_to_optimizer_map,indices)))
    end
    error("Attribute $attr not accessible")
end

function MOI.canget(m::CachingOptimizer, attr::MOI.AbstractModelAttribute)
    MOI.canget(m.model_cache, attr) && return true
    if m.state == AttachedOptimizer
        MOI.canget(m.optimizer, attr) && return true
    end
    return false
end

function MOI.canget(m::CachingOptimizer, attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}, idxtype::Type{<:MOI.Index})
    MOI.canget(m.model_cache, attr, idxtype) && return true
    if m.state == AttachedOptimizer
        MOI.canget(m.optimizer, attr, idxtype) && return true
    end
    return false
end

# Force users to specify whether the attribute should be queried from the
# model_cache or the optimizer. Maybe we could consider a small whitelist of
# attributes to handle automatically.

# These are expert methods to get or set attributes directly in the model_cache
# or optimizer.

struct AttributeFromModelCache{T <: MOI.AnyAttribute}
    attr::T
end

struct AttributeFromOptimizer{T <: MOI.AnyAttribute}
    attr::T
end

function MOI.get(m::CachingOptimizer, attr::AttributeFromModelCache{T}) where {T <: MOI.AbstractModelAttribute}
    return MOI.get(m.model_cache, attr.attr)
end

function MOI.get(m::CachingOptimizer, attr::AttributeFromModelCache{T}, idx) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    return MOI.get(m.model_cache, attr.attr, idx)
end

function MOI.get(m::CachingOptimizer, attr::AttributeFromOptimizer{T}) where {T <: MOI.AbstractModelAttribute}
    @assert m.state == AttachedOptimizer
    return attribute_value_map(m.optimizer_to_model_map,MOI.get(m.optimizer, attr.attr))
end

function MOI.get(m::CachingOptimizer, attr::AttributeFromOptimizer{T}, idx::MOI.Index) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    @assert m.state == AttachedOptimizer
    return attribute_value_map(m.optimizer_to_model_map,MOI.get(m.optimizer, attr.attr, m.model_to_optimizer_map[idx]))
end

function MOI.get(m::CachingOptimizer, attr::AttributeFromOptimizer{T}, idx::Vector{<:MOI.Index}) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    @assert m.state == AttachedOptimizer
    return attribute_value_map(m.optimizer_to_model_map,MOI.get(m.optimizer, attr.attr, getindex.(m.model_to_optimizer_map,idx)))
end

function MOI.canget(m::CachingOptimizer, attr::AttributeFromModelCache{T}) where {T <: MOI.AbstractModelAttribute}
    return MOI.canget(m.model_cache, attr.attr)
end

function MOI.canget(m::CachingOptimizer, attr::AttributeFromModelCache{T}, idxtype::Type{<:MOI.Index}) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    return MOI.canget(m.model_cache, attr.attr, idxtype)
end

function MOI.canget(m::CachingOptimizer, attr::AttributeFromOptimizer{T}) where {T <: MOI.AbstractModelAttribute}
    m.state == AttachedOptimizer || return false
    return MOI.canget(m.optimizer, attr.attr)
end

function MOI.canget(m::CachingOptimizer, attr::AttributeFromOptimizer{T}, idxtype::Type{<:MOI.Index}) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    m.state == AttachedOptimizer || return false
    return MOI.canget(m.optimizer, attr.attr, idxtype)
end

function MOI.set!(m::CachingOptimizer, attr::AttributeFromModelCache{T}, v) where {T <: MOI.AbstractModelAttribute}
    return MOI.set!(m.model_cache, attr.attr, v)
end

function MOI.set!(m::CachingOptimizer, attr::AttributeFromModelCache{T}, idx, v) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    return MOI.set!(m.model_cache, attr.attr, idx, v)
end

function MOI.set!(m::CachingOptimizer, attr::AttributeFromOptimizer{T}, v) where {T <: MOI.AbstractModelAttribute}
    @assert m.state == AttachedOptimizer
    return MOI.set!(m.optimizer, attr.attr, attribute_value_map(m.model_to_optimizer_map,v))
end

function MOI.set!(m::CachingOptimizer, attr::AttributeFromOptimizer{T}, idx, v) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    @assert m.state == AttachedOptimizer
    # TODO: getindex. causes this to return a vector of results for scalar idx
    return MOI.set!(m.optimizer, attr.attr, getindex.(m.model_to_optimizer_map,idx), attribute_value_map(m.model_to_optimizer_map,v))
end

function MOI.canset(m::CachingOptimizer, attr::AttributeFromModelCache{T}) where {T <: MOI.AbstractModelAttribute}
    return MOI.canset(m.model_cache, attr.attr)
end

function MOI.canset(m::CachingOptimizer, attr::AttributeFromModelCache{T}, idxtype::Type{<:MOI.Index}) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    return MOI.canset(m.model_cache, attr.attr, idxtype)
end

function MOI.canset(m::CachingOptimizer, attr::AttributeFromOptimizer{T}) where {T <: MOI.AbstractModelAttribute}
    @assert m.state == AttachedOptimizer
    return MOI.canset(m.optimizer, attr.attr)
end

function MOI.canset(m::CachingOptimizer, attr::AttributeFromOptimizer{T}, idxtype::Type{<:MOI.Index}) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    @assert m.state == AttachedOptimizer
    return MOI.canset(m.optimizer, attr.attr, idxtype)
end

# TODO: get and set methods to look up/set name strings
