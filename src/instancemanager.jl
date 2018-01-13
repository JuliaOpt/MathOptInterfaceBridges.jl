
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
mutable struct InstanceManager
    instance::MOI.AbstractStandaloneInstance
    solver::Union{Void,MOI.AbstractSolverInstance}
    state::InstanceManagerState
    mode::InstanceManagerMode
    instancetosolvermap::IndexMap
    # InstanceManager externally uses the same variable and constraint indices
    # as the instance. instancetosolvermap maps from the instance indices to the
    # solver indices.
end

# TODO: Fuzz this code with a solver that is known to use a different indexing
# scheme from the instance.

InstanceManager(instance::MOI.AbstractStandaloneInstance, mode::InstanceManagerMode) = InstanceManager(instance, nothing, NoSolver, mode, IndexMap())

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
    # concrete container.
    m.instancetosolvermap = IndexMap()
    for k in keys(copy_result.indexmap)
        m.instancetosolvermap[k] = copy_result.indexmap[k]
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
        end
    end
    return vindices
end

function MOI.canaddconstraint(m::InstanceManager, func::MOI.AbstractFunction, set::MOI.AbstractSet)
    MOI.canaddconstraint(m.instance, func, set) || return false
    if m.state == AttachedSolver && m.mode == Manual
        MOI.canaddconstraint(m.solver, func, set) || return false
    end
    return true
end

function MOI.addconstraint!(m::InstanceManager, func::MOI.AbstractFunction, set::MOI.AbstractSet)
    # The canaddconstraint checks should catch most issues, but if an
    # addconstraint! call fails then the instance and the solver may no longer
    # be in sync.
    if m.mode == Automatic && !MOI.canaddconstraint(m.solver, func, set)
        resetsolver!(m)
    end
    @assert MOI.canaddconstraint(m, func, set)
    cindex = MOI.addconstraint!(m.instance, func, set)
    if m.state == AttachedSolver
        cindex_solver = MOI.addconstraint!(m.solver, func, set)
        m.instancetosolvermap[cindex] = cindex_solver
    end
    return cindex
end

# TODO: addconstraints!, canmodifyconstraint, modifyconstraint!,
# transformconstraint!, cantransformconstraint

## InstanceManager get and set attributes

function MOI.set!(m::InstanceManager, attr::MOI.AbstractInstanceAttribute, value)
    # The canset checks should catch most issues, but if a set! call fails then
    # the instance and the solver may no longer be in sync.
    if m.mode == Automatic && !MOI.canset(m.solver, attr)
        resetsolver!(m)
    end
    @assert MOI.canset(m.instance, attr)
    if m.state == AttachedSolver
        @assert MOI.canset(m.solver, attr)
        MOI.set!(m.solver, attr, value)
    end
    MOI.set!(m.instance, attr, value)
end

function MOI.set!(m::InstanceManager, attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}, ref, value)
    if m.mode == Automatic && !MOI.canset(m.solver, attr, typeof(ref))
        resetsolver!(m)
    end
    @assert MOI.canset(m.instance, attr, ref)
    if m.state == AttachedSolver
        @assert MOI.canset(m.solver, attr, typeof(ref))
        MOI.set!(m.solver, attr, ref, value)
    end
    MOI.set!(m.instance, attr, ref, value)
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

function MOI.canset(m::InstanceManager, attr::Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}, idxtype)
    MOI.canset(m.instance, attr, idxtype) || return false
    if m.state == AttachedSolver && m.mode == Manual
        MOI.canset(m.solver, attr, idxtype) || return false
    end
    return true
end

# Force users to specify whether the attribute should be queried from the
# instance or the solver. Maybe we could consider a small whitelist of
# attributes to handle automatically.

# TODO/Warning: AttributeFromSolver is somewhat broken and dangerous because it
# deals with objects indexed in terms of solver variables (if applicable).
# This leads to surprising behavior when used with ObjectiveFunction() and other similar
# attributes. Maybe we should process AbstractFunctions that are set/get
# with AttributeFromSolver to map the indices from the instance to the solver.

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
    return MOI.get(m.solver, attr.attr)
end

function MOI.get(m::InstanceManager, attr::AttributeFromSolver{T}, idx) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    @assert m.state == AttachedSolver
    return MOI.get(m.solver, attr.attr, getindex.(m.instancetosolvermap,idx))
end

function MOI.canget(m::InstanceManager, attr::AttributeFromInstance{T}) where {T <: MOI.AbstractInstanceAttribute}
    return MOI.canget(m.instance, attr.attr)
end

function MOI.canget(m::InstanceManager, attr::AttributeFromInstance{T}, idxtype) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    return MOI.canget(m.instance, attr.attr, idxtype)
end

function MOI.canget(m::InstanceManager, attr::AttributeFromSolver{T}) where {T <: MOI.AbstractInstanceAttribute}
    @assert m.state == AttachedSolver
    return MOI.canget(m.solver, attr.attr)
end

function MOI.canget(m::InstanceManager, attr::AttributeFromSolver{T}, idxtype) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    @assert m.state == AttachedSolver
    return MOI.canget(m.solver, attr.attr, idxtype)
end

# These are expert methods. Usually you wouldn't want to set an attribute only
# in instance or the solver.

function MOI.set!(m::InstanceManager, attr::AttributeFromInstance{T}, v) where {T <: MOI.AbstractInstanceAttribute}
    return MOI.set!(m.instance, attr.attr, v)
end

function MOI.set!(m::InstanceManager, attr::AttributeFromInstance{T}, idx, v) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    return MOI.set!(m.instance, attr.attr, idx, v)
end

function MOI.set!(m::InstanceManager, attr::AttributeFromSolver{T}, v) where {T <: MOI.AbstractInstanceAttribute}
    @assert m.state == AttachedSolver
    return MOI.set!(m.solver, attr.attr, v)
end

function MOI.set!(m::InstanceManager, attr::AttributeFromSolver{T}, idx, v) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    @assert m.state == AttachedSolver
    return MOI.set!(m.solver, attr.attr, getindex.(m.instancetosolvermap,idx), v)
end

function MOI.canset(m::InstanceManager, attr::AttributeFromInstance{T}) where {T <: MOI.AbstractInstanceAttribute}
    return MOI.canset(m.instance, attr.attr)
end

function MOI.canset(m::InstanceManager, attr::AttributeFromInstance{T}, idxtype) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    return MOI.canset(m.instance, attr.attr, idxtype)
end

function MOI.canset(m::InstanceManager, attr::AttributeFromSolver{T}) where {T <: MOI.AbstractInstanceAttribute}
    @assert m.state == AttachedSolver
    return MOI.canset(m.solver, attr.attr)
end

function MOI.canset(m::InstanceManager, attr::AttributeFromSolver{T}, idxtype) where {T <: Union{MOI.AbstractVariableAttribute,MOI.AbstractConstraintAttribute}}
    @assert m.state == AttachedSolver
    return MOI.canset(m.solver, attr.attr, idxtype)
end

# TODO: get and set methods to look up/set name strings

# TODO: delete! and candelete
