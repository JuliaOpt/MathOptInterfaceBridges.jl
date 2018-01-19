
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
    conattribute::Dict{MOI.ConstraintIndex,Int} # MockConstraintAttribute
    solved::Bool
    terminationstatus::MOI.TerminationStatusCode
    resultcount::Int
    objectivevalue::Float64
    primalstatus::MOI.ResultStatusCode
    dualstatus::MOI.ResultStatusCode
    varprimal::Dict{MOI.VariableIndex,Float64}
    condual::Dict{MOI.ConstraintIndex,Float64}
    # TODO: constraint primal
end

MockSolverInstance(instance::MOI.AbstractStandaloneInstance) =
    MockSolverInstance(instance,
                       0,
                       Dict{MOI.VariableIndex,Int}(),
                       Dict{MOI.ConstraintIndex,Int}(),
                       false,
                       MOI.Success,
                       0,
                       NaN,
                       MOI.UnknownResultStatus,
                       MOI.UnknownResultStatus,
                       Dict{MOI.VariableIndex,Float64}(),
                       Dict{MOI.ConstraintIndex,Float64}())

MOI.addvariable!(mock::MockSolverInstance) = MOI.addvariable!(mock.instance)
MOI.addvariables!(mock::MockSolverInstance, n::Int) = MOI.addvariables!(mock.instance, n)
MOI.addconstraint!(mock::MockSolverInstance, F, S) = MOI.addconstraint!(mock.instance, F, S)
MOI.optimize!(mock::MockSolverInstance) = (mock.solved = true)

MOI.canset(mock::MockSolverInstance, ::Union{MOI.ResultCount,MOI.TerminationStatus,MOI.ObjectiveValue,MOI.PrimalStatus,MOI.DualStatus,MOI.ObjectiveSense,MOI.ObjectiveFunction,MockInstanceAttribute}) = true
MOI.canset(mock::MockSolverInstance, ::Union{MOI.VariablePrimal,MockVariableAttribute}, ::Type{MOI.VariableIndex}) = true
MOI.canset(mock::MockSolverInstance, ::Union{MOI.ConstraintDual,MockConstraintAttribute}, ::Type{<:MOI.ConstraintIndex}) = true

MOI.set!(mock::MockSolverInstance, ::MOI.ResultCount, value::Integer) = (mock.resultcount = value)
MOI.set!(mock::MockSolverInstance, ::MOI.TerminationStatus, value::MOI.TerminationStatusCode) = (mock.terminationstatus = value)
MOI.set!(mock::MockSolverInstance, ::MOI.ObjectiveValue, value::Real) = (mock.objectivevalue = value)
MOI.set!(mock::MockSolverInstance, ::MOI.PrimalStatus, value::MOI.ResultStatusCode) = (mock.primalstatus = value)
MOI.set!(mock::MockSolverInstance, ::MOI.DualStatus, value::MOI.ResultStatusCode) = (mock.dualstatus = value)
MOI.set!(mock::MockSolverInstance, ::MockInstanceAttribute, value::Integer) = (mock.attribute = value)
MOI.set!(mock::MockSolverInstance, attr::Union{MOI.ObjectiveSense,MOI.ObjectiveFunction}, value) = MOI.set!(mock.instance, attr, value)

MOI.set!(mock::MockSolverInstance, ::MOI.VariablePrimal, idx::MOI.VariableIndex, value) = (mock.varprimal[idx] = value)
MOI.set!(mock::MockSolverInstance, ::MockVariableAttribute, idx::MOI.VariableIndex, value) = (mock.varattribute[idx] = value)
MOI.set!(mock::MockSolverInstance, ::MockConstraintAttribute, idx::MOI.ConstraintIndex, value) = (mock.conattribute[idx] = value)
MOI.set!(mock::MockSolverInstance, ::MOI.ConstraintDual, idx::MOI.ConstraintIndex, value) = (mock.condual[idx] = value)
function MOI.set!(mock::MockSolverInstance, ::MOI.VariablePrimal, idx::Vector{MOI.VariableIndex}, value)
    for (i,v) in zip(idx, value)
        mock.varprimal[i] = v
    end
end
function MOI.set!(mock::MockSolverInstance, ::MockVariableAttribute, idx::Vector{MOI.VariableIndex}, value)
    for (i,v) in zip(idx, value)
        mock.varattribute[i] = v
    end
end
function MOI.set!(mock::MockSolverInstance, ::MockConstraintAttribute, idx::Vector{<:MOI.ConstraintIndex}, value)
    for (i,v) in zip(idx, value)
        mock.conattribute[i] = v
    end
end
function MOI.set!(mock::MockSolverInstance, ::MOI.ConstraintDual, idx::Vector{<:MOI.ConstraintIndex}, value)
    for (i,v) in zip(idx, value)
        mock.condual[i] = v
    end
end


MOI.canget(mock::MockSolverInstance, ::MOI.ResultCount) = mock.solved
MOI.canget(mock::MockSolverInstance, ::MOI.TerminationStatus) = mock.solved
MOI.canget(mock::MockSolverInstance, ::MOI.ObjectiveValue) = mock.solved # TODO: may want to simulate false
MOI.canget(mock::MockSolverInstance, ::MOI.PrimalStatus) = mock.solved && (mock.resultcount > 0)
MOI.canget(mock::MockSolverInstance, ::MOI.DualStatus) = mock.solved && (mock.resultcount > 0)
MOI.canget(mock::MockSolverInstance, ::MockInstanceAttribute) = true
MOI.canget(mock::MockSolverInstance, ::MOI.ObjectiveSense) = true
MOI.canget(mock::MockSolverInstance, ::MOI.ObjectiveFunction) = true

# We assume that a full result is loaded if resultcount > 0
MOI.canget(mock::MockSolverInstance, ::MOI.VariablePrimal, ::Type{MOI.VariableIndex}) = mock.solved && (mock.resultcount > 0)
MOI.canget(mock::MockSolverInstance, ::MOI.ConstraintDual, ::Type{<:MOI.ConstraintIndex}) = mock.solved && (mock.resultcount > 0) && mock.dualstatus != MOI.UnknownResultStatus

MOI.canget(mock::MockSolverInstance, ::MockVariableAttribute, idx::Type{MOI.VariableIndex}) = length(mock.varattribute) > 0
MOI.canget(mock::MockSolverInstance, ::MockConstraintAttribute, idx::Type{<:MOI.ConstraintIndex}) = length(mock.conattribute) > 0

MOI.get(mock::MockSolverInstance, ::MOI.ResultCount) = mock.resultcount
MOI.get(mock::MockSolverInstance, ::MOI.TerminationStatus) = mock.terminationstatus
MOI.get(mock::MockSolverInstance, ::MOI.ObjectiveValue) = mock.objectivevalue
MOI.get(mock::MockSolverInstance, ::MOI.PrimalStatus) = mock.primalstatus
MOI.get(mock::MockSolverInstance, ::MOI.DualStatus) = mock.dualstatus
MOI.get(mock::MockSolverInstance, ::MockInstanceAttribute) = mock.attribute
MOI.get(mock::MockSolverInstance, attr::Union{MOI.ObjectiveSense,MOI.ObjectiveFunction}) = MOI.get(mock.instance, attr)

MOI.get(mock::MockSolverInstance, ::MockVariableAttribute, idx::MOI.VariableIndex) = mock.varattribute[idx]
MOI.get(mock::MockSolverInstance, ::MockVariableAttribute, idx::Vector{MOI.VariableIndex}) = getindex.(mock.varattribute, idx)
MOI.get(mock::MockSolverInstance, ::MOI.VariablePrimal, idx::MOI.VariableIndex) = mock.varprimal[idx]
MOI.get(mock::MockSolverInstance, ::MOI.VariablePrimal, idx::Vector{MOI.VariableIndex}) = getindex.(mock.varprimal, idx)
MOI.get(mock::MockSolverInstance, ::MOI.ConstraintDual, idx::MOI.ConstraintIndex) = mock.condual[idx]
MOI.get(mock::MockSolverInstance, ::MOI.ConstraintDual, idx::Vector{<:MOI.ConstraintIndex}) = getindex.(mock.condual, idx)
MOI.get(mock::MockSolverInstance, ::MockConstraintAttribute, idx::MOI.ConstraintIndex) = mock.conattribute[idx]
MOI.get(mock::MockSolverInstance, ::MockConstraintAttribute, idx::Vector{<:MOI.ConstraintIndex}) = getindex.(mock.conattribute, idx)

function MOI.empty!(mock::MockSolverInstance)
    MOI.empty!(mock.instance)
    mock.attribute = 0
    mock.varattribute = Dict{MOI.VariableIndex,Int}()
    mock.conattribute = Dict{MOI.ConstraintIndex,Int}()
    mock.solved = false
    mock.terminationstatus = MOI.Success
    mock.resultcount = 0
    mock.objectivevalue = NaN
    mock.primalstatus = MOI.UnknownResultStatus
    mock.varprimal = Dict{MOI.VariableIndex,Float64}()
    return
end

function MOI.isempty(mock::MockSolverInstance)
    # Assumes that variable and constraint attributes can't be set if
    # mock.instance is empty.
    # TODO: Default values are currently copied in three places, not good.
    return MOI.isempty(mock.instance) && mock.attribute == 0 &&
        mock.solved == false && mock.terminationstatus == MOI.Success &&
        mock.resultcount == 0 && isnan(mock.objectivevalue) &&
        mock.primalstatus == MOI.UnknownResultStatus
end


MOI.copy!(mock::MockSolverInstance, src::MOI.AbstractInstance) = defaultcopy!(mock, src)
