# l ≤ ⟨a, x⟩ + α ≤ u
# is transformed into
# ⟨a, x⟩ + α ≥ l
# ⟨a, x⟩ + α ≤ u
struct SplitIntervalBridge{T} <: AbstractBridge
    lower::CI{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}}
    upper::CI{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}
end
function SplitIntervalBridge{T}(model, f::MOI.ScalarAffineFunction{T}, s::MOI.Interval{T}) where T
    lower = MOI.addconstraint!(model, f, MOI.GreaterThan(s.lower))
    upper = MOI.addconstraint!(model, f, MOI.LessThan(s.upper))
    SplitIntervalBridge(lower, upper)
end
# Attributes, Bridge acting as an model
MOI.get(b::SplitIntervalBridge{T}, ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}) where T = 1
MOI.get(b::SplitIntervalBridge{T}, ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}}) where T = 1
MOI.get(b::SplitIntervalBridge{T}, ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}}) where {T} = [b.lower]
MOI.get(b::SplitIntervalBridge{T}, ::MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}) where {T} = [b.upper]

# References
function MOI.delete!(model::MOI.ModelLike, c::SplitIntervalBridge)
    MOI.delete!(model, c.lower)
    MOI.delete!(model, c.upper)
end

# Attributes, Bridge acting as a constraint
MOI.canget(model::MOI.ModelLike, a::MOI.ConstraintPrimal, c::SplitIntervalBridge) = true
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintPrimal, c::SplitIntervalBridge)
    # lower and upper should give the same value
    MOI.get(model, MOI.ConstraintPrimal(), c.lower)
end
MOI.canget(model::MOI.ModelLike, a::MOI.ConstraintDual, c::SplitIntervalBridge) = true
function MOI.get(model::MOI.ModelLike, a::MOI.ConstraintDual, c::SplitIntervalBridge)
    lowd = MOI.get(model, MOI.ConstraintDual(), c.lower) # Should be nonnegative
    uppd = MOI.get(model, MOI.ConstraintDual(), c.upper) # Should be nonpositive
    if lowd > -uppd
        lowd
    else
        uppd
    end
end

# Constraints
MOI.canmodifyconstraint(model::MOI.ModelLike, c::SplitIntervalBridge, change) = true
function MOI.modifyconstraint!(model::MOI.ModelLike, c::SplitIntervalBridge, change::Union{MOI.ScalarAffineFunction, MOI.AbstractFunctionModification})
    MOI.modifyconstraint!(model, c.lower, change)
    MOI.modifyconstraint!(model, c.upper, change)
end
function MOI.modifyconstraint!(model::MOI.ModelLike, c::SplitIntervalBridge, change::MOI.Interval)
    MOI.modifyconstraint!(model, c.lower, MOI.GreaterThan(change.lower))
    MOI.modifyconstraint!(model, c.upper, MOI.LessThan(change.upper))
end
