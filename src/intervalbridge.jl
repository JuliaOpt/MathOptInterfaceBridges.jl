# l ≤ ⟨a, x⟩ + α ≤ u
# is transformed into
# ⟨a, x⟩ + α ≥ l
# ⟨a, x⟩ + α ≤ u
struct SplitIntervalBridge{T} <: AbstractBridge
    lower::CR{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}}
    upper::CR{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}
end
function SplitIntervalBridge{T}(instance, f::MOI.ScalarAffineFunction{T}, s::MOI.Interval{T}) where T
    lower = MOI.addconstraint!(instance, f, MOI.GreaterThan(s.lower))
    upper = MOI.addconstraint!(instance, f, MOI.LessThan(s.upper))
    SplitIntervalBridge(lower, upper)
end
# Attributes, Bridge acting as an instance
MOI.get(b::SplitIntervalBridge{T}, ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}) where T = 1
MOI.get(b::SplitIntervalBridge{T}, ::MOI.NumberOfConstraints{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}}) where T = 1
MOI.get(b::SplitIntervalBridge{T}, ::MOI.ListOfConstraintReferences{MOI.ScalarAffineFunction{T}, MOI.GreaterThan{T}}) where {T} = [b.lower]
MOI.get(b::SplitIntervalBridge{T}, ::MOI.ListOfConstraintReferences{MOI.ScalarAffineFunction{T}, MOI.LessThan{T}}) where {T} = [b.upper]

# References
function MOI.delete!(instance::MOI.AbstractInstance, c::SplitIntervalBridge)
    MOI.delete!(instance, c.lower)
    MOI.delete!(instance, c.upper)
end

# Attributes, Bridge acting as a constraint reference
MOI.canget(instance::MOI.AbstractInstance, a::MOI.ConstraintPrimal, c::SplitIntervalBridge) = true
function MOI.get(instance::MOI.AbstractInstance, a::MOI.ConstraintPrimal, c::SplitIntervalBridge)
    # lower and upper should give the same value
    MOI.get(instance, MOI.ConstraintPrimal(), c.lower)
end
MOI.canget(instance::MOI.AbstractInstance, a::MOI.ConstraintDual, c::SplitIntervalBridge) = true
function MOI.get(instance::MOI.AbstractInstance, a::MOI.ConstraintDual, c::SplitIntervalBridge)
    lowd = MOI.get(instance, MOI.ConstraintDual(), c.lower) # Should be nonnegative
    uppd = MOI.get(instance, MOI.ConstraintDual(), c.upper) # Should be nonpositive
    if lowd > -uppd
        lowd
    else
        uppd
    end
end

# Constraints
MOI.canmodifyconstraint(instance::MOI.AbstractInstance, c::SplitIntervalBridge, change) = true
function MOI.modifyconstraint!(instance::MOI.AbstractInstance, c::SplitIntervalBridge, change::Union{MOI.ScalarAffineFunction, MOI.AbstractFunctionModification})
    MOI.modifyconstraint!(instance, c.lower, change)
    MOI.modifyconstraint!(instance, c.upper, change)
end
function MOI.modifyconstraint!(instance::MOI.AbstractInstance, c::SplitIntervalBridge, change::MOI.Interval)
    MOI.modifyconstraint!(instance, c.lower, MOI.GreaterThan(change.lower))
    MOI.modifyconstraint!(instance, c.upper, MOI.LessThan(change.upper))
end
