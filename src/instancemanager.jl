
@enum InstanceManagerState NoSolver EmptySolver AttachedSolver
@enum InstanceManagerMode Manual Automatic

# TODO: Rename AbstractStandaloneInstance to AbstractInstance and AbstractSolverInstance to AbstractSolver?

# TODO: Benchmark to check if InstanceManager should be parameterized on the instance type.

"""
    InstanceManager

`InstanceManager` manages the connection between an AbstractStandaloneInstance (instance) and an AbstractSolverInstance (solver).
An `InstanceManager` may be in one of three possible states (`InstanceManagerState`):

- `NoSolver`: The InstanceManager has an instance to hold problem data and does not have any solver.
- `EmptySolver`: The InstanceManager has an instance and an empty solver. The solver is not synchronized with the instance.
- `AttachedSolver`: The InstanceManager has an instance and a solver, and they hold identical problem data.

An `InstanceManger` has two modes of operation (`InstanceManagerMode`):

- `Manual`: The only methods that change the state of the `InstanceManager` are [`resetsolver!`](@ref), [`dropsolver!`](@ref), and [`attachsolver!`](@ref). Attempting to perform an operation in the incorrect state results in an error.
- `Automatic`: The `InstanceManager` changes its state when necessary. For example, `optimize!` will automatically call `attachsolver!` (a solver must have been previously set). Attempting to add a constraint or perform a modification not supported by the solver results in a drop to `EmptySolver` mode.
"""
mutable struct InstanceManager <: MOI.AbstractInstance
    instance::MOI.AbstractStandaloneInstance
    solver::Union{Void,MOI.AbstractSolverInstance}
    state::InstanceManagerState
    mode::InstanceManagerMode
    instancetosolvermap::IndexMap
    solvertoinstancemap::IndexMap
    # InstanceManager externally uses the same variable and constraint indices
    # as the instance. instancetosolvermap maps from the instance indices to the
    # solver indices.
end

InstanceManager(instance::MOI.AbstractStandaloneInstance, mode::InstanceManagerMode) = InstanceManager(instance, nothing, NoSolver, mode, IndexMap(), IndexMap())

"""
    InstanceManager(instance::AbstractStandaloneInstance, solver::AbstractSolverInstance)

Creates an `InstanceManager` in `Automatic` mode, with the solver `solver`. The instance manager returned behaves like an `AbstractSolverInstance` as long as no `InstanceManager`-specific functions (e.g. `dropsolver!`) are called on it.
"""
InstanceManager(instance::MOI.AbstractStandaloneInstance, solver::MOI.AbstractSolverInstance) = InstanceManager(instance, solver, EmptySolver, Automatic, IndexMap(), IndexMap())

## Methods for managing the state of InstanceManager.

"""
    state(m::InstanceManager)::InstanceManagerState

Returns the state of the InstanceManager `m`. See [`InstanceManager`](@ref).
"""
state(m::InstanceManager) = m.state

"""
    mode(m::InstanceManager)::InstanceManagerMode

Returns the operating mode of the InstanceManager `m`. See [`InstanceManager`](@ref).
"""
mode(m::InstanceManager) = m.mode

"""
    resetsolver!(m::InstanceManager, solver::MOI.AbstractSolverInstance)

Sets or resets the instance to have the given empty solver. Can be called
from any state. The `InstanceManager` will be in state `EmptySolver` after the call.
"""
function resetsolver!(m::InstanceManager, solver::MOI.AbstractSolverInstance)
    @assert MOI.isempty(solver)
    m.solver = solver
    m.state = EmptySolver
    return
end

"""
    resetsolver!(m::InstanceManager)

Detaches and empties the current solver. Can be called from `AttachedSolver`
or `EmptySolver` state. The `InstanceManager` will be in state `EmptySolver`
after the call.
"""
function resetsolver!(m::InstanceManager)
    m.state == EmptySolver && return
    @assert m.state == AttachedSolver
    MOI.empty!(m.solver)
    m.state = EmptySolver
    return
end

"""
    dropsolver!(m::InstanceManager)

Drops the solver, if one is present. Can be called from any state.
The `InstanceManager` will be in state `NoSolver` after the call.
"""
function dropsolver!(m::InstanceManager)
    m.solver = nothing
    m.state = NoSolver
    return
end

"""
    attachsolver!(m::InstanceManager)

Attaches the solver, copying all instance data into it. Can be called only
from the `EmptySolver` state. The `InstanceManager` will be in state `AttachedSolver`
after the call. Returns an `MOI.CopyResult`. `MOI.CopySuccess` means that the
solver is correctly attached, otherwise the status indicates why the `copy!`
from the instance to the solver failed.
"""
function attachsolver!(m::InstanceManager)
    @assert m.state == EmptySolver
    copy_result = MOI.copy!(m.solver, m.instance)
    if copy_result.status != MOI.CopySuccess
        return copy_result
    end
    m.state = AttachedSolver
    # MOI does not define the type of index_map, so we have to copy it into a
    # concrete container. Also load the reverse map.
    m.instancetosolvermap = IndexMap()
    m.solvertoinstancemap = IndexMap()
    for k in keys(copy_result.indexmap)
        m.instancetosolvermap[k] = copy_result.indexmap[k]
        m.solvertoinstancemap[copy_result.indexmap[k]] = k
    end
    return copy_result
end

# Optimizing and adding/modifying constraints and variables.

function MOI.optimize!(m::InstanceManager)
    if m.mode == Automatic && m.state == EmptySolver
        attachsolver!(m)
    end
    # TODO: better error message if no solver is set
    @assert m.state == AttachedSolver
    MOI.optimize!(m.solver)
end

function MOI.addvariable!(m::InstanceManager)
    # TODO: This is currently broken for automatic mode if the solver doesn't
    # support addvariable!. We probably need a "canaddvariable".
    vindex = MOI.addvariable!(m.instance)
    if m.state == AttachedSolver
        vindex_solver = MOI.addvariable!(m.solver)
        m.instancetosolvermap[vindex] = vindex_solver
        m.solvertoinstancemap[vindex_solver] = vindex
    end
    return vindex
end

function MOI.addvariables!(m::InstanceManager, n)
    # TODO: Same note as above.
    vindices = MOI.addvariables!(m.instance, n)
    if m.state == AttachedSolver
        vindices_solver = MOI.addvariables!(m.solver, n)
        for (vindex, vindex_solver) in zip(vindices, vindices_solver)
            m.instancetosolvermap[vindex] = vindex_solver
            m.solvertoinstancemap[vindex_solver] = vindex
        end
    end
    return vindices
end

function MOI.canaddconstraint(m::InstanceManager, func::MOI.AbstractFunction, set::MOI.AbstractSet)
    MOI.canaddconstraint(m.instance, func, set) || return false
    if m.state == AttachedSolver && m.mode == Manual
        # TODO: The indices of func should be mapped, but that's an expensive
        # operation. Hope that the solver doesn't care.
        # Maybe canaddconstraint should just be a function of the types.
        MOI.canaddconstraint(m.solver, func, set) || return false
    end
    return true
end

function MOI.addconstraint!(m::InstanceManager, func::MOI.AbstractFunction, set::MOI.AbstractSet)
    # The canaddconstraint checks should catch most issues, but if an
    # addconstraint! call fails then the instance and the solver may no longer
    # be in sync.
    if m.mode == Automatic && m.state == AttachedSolver && !MOI.canaddconstraint(m.solver, func, set)
        resetsolver!(m)
    end
    @assert MOI.canaddconstraint(m, func, set)
    cindex = MOI.addconstraint!(m.instance, func, set)
    if m.state == AttachedSolver
        cindex_solver = MOI.addconstraint!(m.solver, mapvariables(m.instancetosolvermap,func), set)
        m.instancetosolvermap[cindex] = cindex_solver
        m.solvertoinstancemap[cindex_solver] = cindex
    end
    return cindex
end

function MOI.canmodifyconstraint(m::InstanceManager, cindex::CI{F,S}, func::F) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    MOI.canmodifyconstraint(m.instance, cindex, func) || return false
    if m.state == AttachedSolver && m.mode == Manual
        MOI.canmodifyconstraint(m.solver, m.instancetosolvermap[cindex], func) || return false
    end
    return true
end

function MOI.canmodifyconstraint(m::InstanceManager, cindex::CI{F,S}, set::S) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    MOI.canmodifyconstraint(m.instance, cindex, set) || return false
    if m.state == AttachedSolver && m.mode == Manual
        MOI.canmodifyconstraint(m.solver, m.instancetosolvermap[cindex], set) || return false
    end
    return true
end

function MOI.modifyconstraint!(m::InstanceManager, cindex::CI{F,S}, func::F) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    if m.mode == Automatic && m.state == AttachedSolver && !MOI.canmodifyconstraint(m.solver, cindex, func)
        resetsolver!(m)
    end
    @assert MOI.canmodifyconstraint(m, cindex, func)
    MOI.modifyconstraint!(m.instance, cindex, func)
    if m.state == AttachedSolver
        MOI.modifyconstraint!(m.solver, m.instancetosolvermap[cindex], mapvariables(m.instancetosolvermap,func))
    end
    return
end

function MOI.modifyconstraint!(m::InstanceManager, cindex::CI{F,S}, set::S) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    if m.mode == Automatic && m.state == AttachedSolver && !MOI.canmodifyconstraint(m.solver, cindex, set)
        resetsolver!(m)
    end
    @assert MOI.canmodifyconstraint(m, cindex, set)
    MOI.modifyconstraint!(m.instance, cindex, set)
    if m.state == AttachedSolver
        MOI.modifyconstraint!(m.solver, m.instancetosolvermap[cindex], set)
    end
    return
end

MOI.isvalid(m::InstanceManager, index::MOI.Index) = MOI.isvalid(m.instance, index)

function MOI.candelete(m::InstanceManager, index::MOI.Index)
    MOI.candelete(m.instance, index) || return false
    if m.state == AttachedSolver && m.mode == Manual
        MOI.candelete(m.solver, m.instancetosolvermap[index]) || return false
    end
    return true
end

function MOI.delete!(m::InstanceManager, index::MOI.Index)
    if m.mode == Automatic && m.state == AttachedSolver && !MOI.candelete(m.solver, index)
        resetsolver!(m)
    end
    @assert MOI.candelete(m, index)
    if m.state == AttachedSolver
        MOI.delete!(m.solver, m.instancetosolvermap[cindex])
        delete!(m.solvertoinstancemap, m.instancetosolvermap[cindex])
        delete!(m.instancetosolvermap, cindex)
    end
    MOI.delete!(m.instance, index)
end


# TODO: addconstraints!, transformconstraint!, cantransformconstraint

## InstanceManager get and set attributes

# Attributes are mapped through attribute_value_map (defined in copy.jl) before
# they are sent to the solver and when they are returned from the solver.
# This map currently only translates indices on MOI.AbstractFunction objects
# between the solver indices and the (user-facing) instance indices. As a result,
# all MOI.AbstractFunctions must implement mapvariables. Other attributes that
# store indices need to be handled with care.

function MOI.set!(m::InstanceManager, attr::MOI.AbstractInstanceAttribute, value)
    # The canset checks should catch most issues, but if a set! call fails then
    # the instance and the solver may no longer be in sync.
    if m.mode == Automatic && m.state == AttachedSolver && !MOI.canset(m.solver, attr)
        resetsolver!(m)
    end
    @assert MOI.canset(m.instance, attr)
    if m.state == AttachedSolver
        @assert MOI.canset(m.solver, attr)
        MOI.set!(m.solver, attr, attribute_value_map(m.instancetosolvermap,value))
    end
    MOI.set!(m.instance, attr, value)
end

function MOI.set!(m::InstanceManager, attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}, index::MOI.Index, value)
    if m.mode == Automatic && m.state == AttachedSolver && !MOI.canset(m.solver, attr, typeof(index))
        resetsolver!(m)
    end
    @assert MOI.canset(m.instance, attr, typeof(index))
    if m.state == AttachedSolver
        @assert MOI.canset(m.solver, attr, typeof(index))
        MOI.set!(m.solver, attr, m.instancetosolvermap[index], attribute_value_map(m.instancetosolvermap,value))
    end
    MOI.set!(m.instance, attr, index, value)
end

# TODO: Automatic mode is broken in the case that the user tries to set
# an objective function of a type that's not supported by the solver.
# Two possible solutions are:
# 1. Let canset take a value argument.
# 2. Be more precise about what types are allowed in each attribute
# (https://github.com/JuliaOpt/MathOptInterface.jl/issues/31).
function MOI.canset(m::InstanceManager, attr::MOI.AbstractInstanceAttribute)
    MOI.canset(m.instance, attr) || return false
    if m.state == AttachedSolver && m.mode == Manual
        MOI.canset(m.solver, attr) || return false
    end
    return true
end

function MOI.canset(m::InstanceManager, attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}, idxtype::Type{<:MOI.Index})
    MOI.canset(m.instance, attr, idxtype) || return false
    if m.state == AttachedSolver && m.mode == Manual
        MOI.canset(m.solver, attr, idxtype) || return false
    end
    return true
end

function MOI.get(m::InstanceManager, attr::MOI.AbstractInstanceAttribute)
    if MOI.canget(m.instance, attr)
        return MOI.get(m.instance, attr)
    elseif m.state == AttachedSolver && MOI.canget(m.solver, attr)
        return attribute_value_map(m.solvertoinstancemap,MOI.get(m.solver, attr))
    end
    error("Attribute $attr not accessible")
end

function MOI.get(m::InstanceManager, attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}, index::MOI.Index)
    if MOI.canget(m.instance, attr, typeof(index))
        return MOI.get(m.instance, attr, index)
    elseif m.state == AttachedSolver && MOI.canget(m.solver, attr, typeof(index))
        return attribute_value_map(m.solvertoinstancemap,MOI.get(m.solver, attr, m.instancetosolvermap[index]))
    end
    error("Attribute $attr not accessible")
end

function MOI.get(m::InstanceManager, attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}, indices::Vector{<:MOI.Index})
    if MOI.canget(m.instance, attr, eltype(indices))
        return MOI.get(m.instance, attr, indices)
    elseif m.state == AttachedSolver && MOI.canget(m.solver, attr, eltype(indices))
        return attribute_value_map.(m.solvertoinstancemap,MOI.get(m.solver, attr, getindex.(m.instancetosolvermap,indices)))
    end
    error("Attribute $attr not accessible")
end

function MOI.canget(m::InstanceManager, attr::MOI.AbstractInstanceAttribute)
    MOI.canget(m.instance, attr) && return true
    if m.state == AttachedSolver
        MOI.canget(m.solver, attr) && return true
    end
    return false
end

function MOI.canget(m::InstanceManager, attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}, idxtype::Type{<:MOI.Index})
    MOI.canget(m.instance, attr, idxtype) && return true
    if m.state == AttachedSolver
        MOI.canget(m.solver, attr, idxtype) && return true
    end
    return false
end

# Force users to specify whether the attribute should be queried from the
# instance or the solver. Maybe we could consider a small whitelist of
# attributes to handle automatically.

# These are expert methods to get or set attributes directly in the instance
# or solver.

struct AttributeFromInstance{T <: MOI.AnyAttribute}
    attr::T
end

struct AttributeFromSolver{T <: MOI.AnyAttribute}
    attr::T
end

function MOI.get(m::InstanceManager, attr::AttributeFromInstance{T}) where {T <: MOI.AbstractInstanceAttribute}
    return MOI.get(m.instance, attr.attr)
end

function MOI.get(m::InstanceManager, attr::AttributeFromInstance{T}, idx) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    return MOI.get(m.instance, attr.attr, idx)
end

function MOI.get(m::InstanceManager, attr::AttributeFromSolver{T}) where {T <: MOI.AbstractInstanceAttribute}
    @assert m.state == AttachedSolver
    return attribute_value_map(m.solvertoinstancemap,MOI.get(m.solver, attr.attr))
end

function MOI.get(m::InstanceManager, attr::AttributeFromSolver{T}, idx::MOI.Index) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    @assert m.state == AttachedSolver
    return attribute_value_map(m.solvertoinstancemap,MOI.get(m.solver, attr.attr, m.instancetosolvermap[idx]))
end

function MOI.get(m::InstanceManager, attr::AttributeFromSolver{T}, idx::Vector{<:MOI.Index}) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    @assert m.state == AttachedSolver
    return attribute_value_map(m.solvertoinstancemap,MOI.get(m.solver, attr.attr, getindex.(m.instancetosolvermap,idx)))
end

function MOI.canget(m::InstanceManager, attr::AttributeFromInstance{T}) where {T <: MOI.AbstractInstanceAttribute}
    return MOI.canget(m.instance, attr.attr)
end

function MOI.canget(m::InstanceManager, attr::AttributeFromInstance{T}, idxtype::Type{<:MOI.Index}) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    return MOI.canget(m.instance, attr.attr, idxtype)
end

function MOI.canget(m::InstanceManager, attr::AttributeFromSolver{T}) where {T <: MOI.AbstractInstanceAttribute}
    m.state == AttachedSolver || return false
    return MOI.canget(m.solver, attr.attr)
end

function MOI.canget(m::InstanceManager, attr::AttributeFromSolver{T}, idxtype::Type{<:MOI.Index}) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    m.state == AttachedSolver || return false
    return MOI.canget(m.solver, attr.attr, idxtype)
end

function MOI.set!(m::InstanceManager, attr::AttributeFromInstance{T}, v) where {T <: MOI.AbstractInstanceAttribute}
    return MOI.set!(m.instance, attr.attr, v)
end

function MOI.set!(m::InstanceManager, attr::AttributeFromInstance{T}, idx, v) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    return MOI.set!(m.instance, attr.attr, idx, v)
end

function MOI.set!(m::InstanceManager, attr::AttributeFromSolver{T}, v) where {T <: MOI.AbstractInstanceAttribute}
    @assert m.state == AttachedSolver
    return MOI.set!(m.solver, attr.attr, attribute_value_map(m.instancetosolvermap,v))
end

function MOI.set!(m::InstanceManager, attr::AttributeFromSolver{T}, idx, v) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    @assert m.state == AttachedSolver
    # TODO: getindex. causes this to return a vector of results for scalar idx
    return MOI.set!(m.solver, attr.attr, getindex.(m.instancetosolvermap,idx), attribute_value_map(m.instancetosolvermap,v))
end

function MOI.canset(m::InstanceManager, attr::AttributeFromInstance{T}) where {T <: MOI.AbstractInstanceAttribute}
    return MOI.canset(m.instance, attr.attr)
end

function MOI.canset(m::InstanceManager, attr::AttributeFromInstance{T}, idxtype::Type{<:MOI.Index}) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    return MOI.canset(m.instance, attr.attr, idxtype)
end

function MOI.canset(m::InstanceManager, attr::AttributeFromSolver{T}) where {T <: MOI.AbstractInstanceAttribute}
    @assert m.state == AttachedSolver
    return MOI.canset(m.solver, attr.attr)
end

function MOI.canset(m::InstanceManager, attr::AttributeFromSolver{T}, idxtype::Type{<:MOI.Index}) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    @assert m.state == AttachedSolver
    return MOI.canset(m.solver, attr.attr, idxtype)
end

# TODO: get and set methods to look up/set name strings
