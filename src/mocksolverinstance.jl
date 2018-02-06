
# An Int-valued attribute
struct MockInstanceAttribute <: MOI.AbstractInstanceAttribute
end

# An Int-valued attribute
struct MockVariableAttribute <: MOI.AbstractVariableAttribute
end

# An Int-valued attribute
struct MockConstraintAttribute <: MOI.AbstractConstraintAttribute
end

# A mock solver instance used for testing.
mutable struct MockSolverInstance <: MOI.AbstractSolverInstance
    instance::MOI.AbstractStandaloneInstance
    attribute::Int # MockInstanceAttribute
    varattribute::Dict{MOI.VariableIndex,Int} # MockVariableAttribute
    varname::Dict{MOI.VariableIndex,String} # VariableName
    conattribute::Dict{MOI.ConstraintIndex,Int} # MockConstraintAttribute
    conname::Dict{MOI.ConstraintIndex,String} # ConstraintName
    solved::Bool
    terminationstatus::MOI.TerminationStatusCode
    resultcount::Int
    objectivevalue::Float64
    primalstatus::MOI.ResultStatusCode
    dualstatus::MOI.ResultStatusCode
    varprimal::Dict{MOI.VariableIndex,Float64}
    condual::Dict{MOI.ConstraintIndex,Any}
    # TODO: constraint primal
end

# All user-facing indices are xor'd with this mask to produce unusual indices.
# This is good at catching bugs.
const internal_xor_mask = Int64(12345678)
xor_index(vi::VI) = VI(xor(vi.value, internal_xor_mask))
xor_index(ci::CI{F,S}) where {F,S} = CI{F,S}(xor(ci.value, internal_xor_mask))
xor_variables(f) = mapvariables(xor_index, f)

MockSolverInstance(instance::MOI.AbstractStandaloneInstance) =
    MockSolverInstance(instance,
                       0,
                       Dict{MOI.VariableIndex,Int}(),
                       Dict{MOI.VariableIndex,String}(),
                       Dict{MOI.ConstraintIndex,Int}(),
                       Dict{MOI.ConstraintIndex,String}(),
                       false,
                       MOI.Success,
                       0,
                       NaN,
                       MOI.UnknownResultStatus,
                       MOI.UnknownResultStatus,
                       Dict{MOI.VariableIndex,Float64}(),
                       Dict{MOI.ConstraintIndex,Any}())

MOI.addvariable!(mock::MockSolverInstance) = xor_index(MOI.addvariable!(mock.instance))
MOI.addvariables!(mock::MockSolverInstance, n::Int) = xor_index.(MOI.addvariables!(mock.instance, n))
MOI.canaddconstraint(mock::MockSolverInstance, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet} = MOI.canaddconstraint(mock.instance, F, S)
MOI.addconstraint!(mock::MockSolverInstance, F::MOI.AbstractFunction, S::MOI.AbstractSet) = xor_index(MOI.addconstraint!(mock.instance, xor_variables(F), S))
MOI.optimize!(mock::MockSolverInstance) = (mock.solved = true)

MOI.canset(mock::MockSolverInstance, ::Union{MOI.ResultCount,MOI.TerminationStatus,MOI.ObjectiveValue,MOI.PrimalStatus,MOI.DualStatus,MOI.ObjectiveSense,MOI.ObjectiveFunction,MockInstanceAttribute}) = true
MOI.canset(mock::MockSolverInstance, ::Union{MOI.VariablePrimal,MOI.VariableName,MockVariableAttribute}, ::Type{MOI.VariableIndex}) = true
MOI.canset(mock::MockSolverInstance, ::Union{MOI.ConstraintDual,MOI.ConstraintName,MockConstraintAttribute}, ::Type{<:MOI.ConstraintIndex}) = true

MOI.set!(mock::MockSolverInstance, ::MOI.ResultCount, value::Integer) = (mock.resultcount = value)
MOI.set!(mock::MockSolverInstance, ::MOI.TerminationStatus, value::MOI.TerminationStatusCode) = (mock.terminationstatus = value)
MOI.set!(mock::MockSolverInstance, ::MOI.ObjectiveValue, value::Real) = (mock.objectivevalue = value)
MOI.set!(mock::MockSolverInstance, ::MOI.PrimalStatus, value::MOI.ResultStatusCode) = (mock.primalstatus = value)
MOI.set!(mock::MockSolverInstance, ::MOI.DualStatus, value::MOI.ResultStatusCode) = (mock.dualstatus = value)
MOI.set!(mock::MockSolverInstance, ::MockInstanceAttribute, value::Integer) = (mock.attribute = value)
MOI.set!(mock::MockSolverInstance, attr::MOI.ObjectiveSense, value) = MOI.set!(mock.instance, attr, value)
MOI.set!(mock::MockSolverInstance, attr::MOI.ObjectiveFunction, value) = MOI.set!(mock.instance, attr, xor_variables(value))

MOI.set!(mock::MockSolverInstance, ::MOI.VariablePrimal, idx::MOI.VariableIndex, value) = (mock.varprimal[xor_index(idx)] = value)
MOI.set!(mock::MockSolverInstance, ::MOI.VariableName, idx::MOI.VariableIndex, value) = (mock.varname[xor_index(idx)] = value)
MOI.set!(mock::MockSolverInstance, ::MockVariableAttribute, idx::MOI.VariableIndex, value) = (mock.varattribute[xor_index(idx)] = value)
MOI.set!(mock::MockSolverInstance, ::MockConstraintAttribute, idx::MOI.ConstraintIndex, value) = (mock.conattribute[xor_index(idx)] = value)
MOI.set!(mock::MockSolverInstance, ::MOI.ConstraintDual, idx::MOI.ConstraintIndex, value) = (mock.condual[xor_index(idx)] = value)
MOI.set!(mock::MockSolverInstance, ::MOI.ConstraintName, idx::MOI.ConstraintIndex, value) = (mock.conname[xor_index(idx)] = value)
function MOI.set!(mock::MockSolverInstance, ::MOI.VariablePrimal, idx::Vector{MOI.VariableIndex}, value)
    for (i,v) in zip(idx, value)
        mock.varprimal[xor_index(i)] = v
    end
end
function MOI.set!(mock::MockSolverInstance, ::MOI.VariableName, idx::Vector{MOI.VariableIndex}, value)
    for (i,v) in zip(idx, value)
        mock.varname[xor_index(i)] = v
    end
end
function MOI.set!(mock::MockSolverInstance, ::MockVariableAttribute, idx::Vector{MOI.VariableIndex}, value)
    for (i,v) in zip(idx, value)
        mock.varattribute[xor_index(i)] = v
    end
end
function MOI.set!(mock::MockSolverInstance, ::MockConstraintAttribute, idx::Vector{<:MOI.ConstraintIndex}, value)
    for (i,v) in zip(idx, value)
        mock.conattribute[xor_index(i)] = v
    end
end
function MOI.set!(mock::MockSolverInstance, ::MOI.ConstraintDual, idx::Vector{<:MOI.ConstraintIndex}, value)
    for (i,v) in zip(idx, value)
        mock.condual[xor_index(i)] = v
    end
end
function MOI.set!(mock::MockSolverInstance, ::MOI.ConstraintName, idx::Vector{<:MOI.ConstraintIndex}, value)
    for (i,v) in zip(idx, value)
        mock.conname[xor_index(i)] = v
    end
end


MOI.canget(mock::MockSolverInstance, ::MOI.ResultCount) = mock.solved
MOI.canget(mock::MockSolverInstance, ::MOI.TerminationStatus) = mock.solved
MOI.canget(mock::MockSolverInstance, ::MOI.ObjectiveValue) = mock.solved # TODO: may want to simulate false
MOI.canget(mock::MockSolverInstance, ::MOI.PrimalStatus) = mock.solved && (mock.resultcount > 0)
MOI.canget(mock::MockSolverInstance, ::MOI.DualStatus) = mock.solved && (mock.resultcount > 0)
MOI.canget(mock::MockSolverInstance, ::MockInstanceAttribute) = true

MOI.canget(mock::MockSolverInstance, attr::Union{MOI.NumberOfVariables,
                                                 MOI.ListOfVariableIndices,
                                                 MOI.NumberOfConstraints,
                                                 MOI.ListOfConstraints,
                                                 MOI.ListOfConstraintIndices,
                                                 MOI.ObjectiveFunction,
                                                 MOI.ObjectiveSense}) = MOI.canget(mock.instance, attr)

MOI.get(mock::MockSolverInstance, attr::Union{MOI.NumberOfVariables,
                                              MOI.NumberOfConstraints,
                                              MOI.ListOfConstraints,
                                              MOI.ObjectiveSense}) = MOI.get(mock.instance, attr)
MOI.get(mock::MockSolverInstance, attr::Union{MOI.ListOfVariableIndices,
                                              MOI.ListOfConstraintIndices}) = xor_index.(MOI.get(mock.instance, attr))
MOI.get(mock::MockSolverInstance, attr::Union{MOI.ObjectiveFunction}) = xor_variables(MOI.get(mock.instance, attr))

MOI.canget(mock::MockSolverInstance, attr::Union{MOI.ConstraintFunction,
                                                 MOI.ConstraintSet}, idx::Type{<:MOI.Index}) = MOI.canget(mock.instance, attr, idx)

MOI.get(mock::MockSolverInstance, attr::Union{MOI.ConstraintSet}, idx::MOI.Index) = MOI.get(mock.instance, attr, xor_index(idx))
MOI.get(mock::MockSolverInstance, attr::Union{MOI.ConstraintFunction}, idx::MOI.Index) = xor_variables(MOI.get(mock.instance, attr, xor_index(idx)))

# We assume that a full result is loaded if resultcount > 0
MOI.canget(mock::MockSolverInstance, ::MOI.VariablePrimal, ::Type{MOI.VariableIndex}) = mock.solved && (mock.resultcount > 0)
MOI.canget(mock::MockSolverInstance, ::MOI.ConstraintDual, ::Type{<:MOI.ConstraintIndex}) = mock.solved && (mock.resultcount > 0) && mock.dualstatus != MOI.UnknownResultStatus
MOI.canget(mock::MockSolverInstance, ::MOI.ConstraintName, idx::Type{MOI.ConstraintIndex}) = length(mock.conname) > 0

MOI.canget(mock::MockSolverInstance, ::MockVariableAttribute, idx::Type{MOI.VariableIndex}) = length(mock.varattribute) > 0
MOI.canget(mock::MockSolverInstance, ::MOI.VariableName, idx::Type{MOI.VariableIndex}) = length(mock.varname) > 0
MOI.canget(mock::MockSolverInstance, ::MockConstraintAttribute, idx::Type{<:MOI.ConstraintIndex}) = length(mock.conattribute) > 0

MOI.get(mock::MockSolverInstance, ::MOI.ResultCount) = mock.resultcount
MOI.get(mock::MockSolverInstance, ::MOI.TerminationStatus) = mock.terminationstatus
MOI.get(mock::MockSolverInstance, ::MOI.ObjectiveValue) = mock.objectivevalue
MOI.get(mock::MockSolverInstance, ::MOI.PrimalStatus) = mock.primalstatus
MOI.get(mock::MockSolverInstance, ::MOI.DualStatus) = mock.dualstatus
MOI.get(mock::MockSolverInstance, ::MockInstanceAttribute) = mock.attribute

MOI.get(mock::MockSolverInstance, ::MockVariableAttribute, idx::MOI.VariableIndex) = mock.varattribute[xor_index(idx)]
MOI.get(mock::MockSolverInstance, ::MockVariableAttribute, idx::Vector{MOI.VariableIndex}) = getindex.(mock.varattribute, xor_index.(idx))
MOI.get(mock::MockSolverInstance, ::MOI.VariablePrimal, idx::MOI.VariableIndex) = mock.varprimal[xor_index(idx)]
MOI.get(mock::MockSolverInstance, ::MOI.VariablePrimal, idx::Vector{MOI.VariableIndex}) = getindex.(mock.varprimal, xor_index.(idx))
MOI.get(mock::MockSolverInstance, ::MOI.VariableName, idx::MOI.VariableIndex) = mock.varname[xor_index(idx)]
MOI.get(mock::MockSolverInstance, ::MOI.VariableName, idx::Vector{MOI.VariableIndex}) = getindex.(mock.varname, xor_index.(idx))
MOI.get(mock::MockSolverInstance, ::MOI.ConstraintDual, idx::MOI.ConstraintIndex) = mock.condual[xor_index(idx)]
MOI.get(mock::MockSolverInstance, ::MOI.ConstraintDual, idx::Vector{<:MOI.ConstraintIndex}) = getindex.(mock.condual, xor_index.(idx))
MOI.get(mock::MockSolverInstance, ::MOI.ConstraintName, idx::MOI.ConstraintIndex) = mock.conname[xor_index(idx)]
MOI.get(mock::MockSolverInstance, ::MOI.ConstraintName, idx::Vector{<:MOI.ConstraintIndex}) = getindex.(mock.conname, xor_index.(idx))
MOI.get(mock::MockSolverInstance, ::MockConstraintAttribute, idx::MOI.ConstraintIndex) = mock.conattribute[xor_index(idx)]
MOI.get(mock::MockSolverInstance, ::MockConstraintAttribute, idx::Vector{<:MOI.ConstraintIndex}) = getindex.(mock.conattribute, xor_index.(idx))

function MOI.empty!(mock::MockSolverInstance)
    MOI.empty!(mock.instance)
    mock.attribute = 0
    mock.varattribute = Dict{MOI.VariableIndex,Int}()
    mock.varname = Dict{MOI.VariableIndex,Int}()
    mock.conattribute = Dict{MOI.ConstraintIndex,Int}()
    mock.conname = Dict{MOI.ConstraintIndex,String}()
    mock.solved = false
    mock.terminationstatus = MOI.Success
    mock.resultcount = 0
    mock.objectivevalue = NaN
    mock.primalstatus = MOI.UnknownResultStatus
    mock.dualstatus = MOI.UnknownResultStatus
    mock.varprimal = Dict{MOI.VariableIndex,Float64}()
    mock.condual = Dict{MOI.ConstraintIndex,Any}()
    return
end

function MOI.isempty(mock::MockSolverInstance)
    # Assumes that variable and constraint attributes can't be set if
    # mock.instance is empty.
    # TODO: Default values are currently copied in three places, not good.
    return MOI.isempty(mock.instance) && mock.attribute == 0 &&
        mock.solved == false && mock.terminationstatus == MOI.Success &&
        mock.resultcount == 0 && isnan(mock.objectivevalue) &&
        mock.primalstatus == MOI.UnknownResultStatus &&
        mock.dualstatus == MOI.UnknownResultStatus
end

MOI.isvalid(mock::MockSolverInstance, idx::MOI.Index) = MOI.isvalid(mock.instance, xor_index(idx))

MOI.candelete(mock::MockSolverInstance, idx::MOI.Index) = MOI.candelete(mock.instance, xor_index(idx))
function MOI.delete!(mock::MockSolverInstance, idx::MOI.VariableIndex)
    MOI.delete!(mock.instance, xor_index(idx))
    MOI.delete!(mock.varprimal, idx)
end
function MOI.delete!(mock::MockSolverInstance, idx::MOI.ConstraintIndex)
    MOI.delete!(mock.instance, xor_index(idx))
    MOI.delete!(mock.condual, idx)
end

function MOI.canmodifyconstraint(mock::MockSolverInstance, c::CI, change)
    MOI.canmodifyconstraint(mock.instance, xor_index(c), change)
end

function MOI.modifyconstraint!(mock::MockSolverInstance, c::CI, change)
    MOI.modifyconstraint!(mock.instance, xor_index(c), xor_variables(change))
end

function MOI.modifyconstraint!(mock::MockSolverInstance, c::CI{F,S}, set::S) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    MOI.modifyconstraint!(mock.instance, xor_index(c), set)
end

function MOI.canmodifyobjective(mock::MockSolverInstance, change)
    MOI.canmodifyobjective(mock.instance, change)
end

function MOI.modifyobjective!(mock::MockSolverInstance, change::MOI.AbstractFunctionModification)
    MOI.modifyobjective!(mock.instance, xor_variables(change))
end

# TODO: transformconstraint and cantransformconstraint


MOI.copy!(mock::MockSolverInstance, src::MOI.AbstractInstance) = defaultcopy!(mock, src)
