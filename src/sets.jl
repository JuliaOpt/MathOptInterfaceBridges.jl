"""
    getconstant(s::Union{MOI.EqualTo, MOI.GreaterThan, MOI.LessThan})

Returns the constant of the set.
"""
getconstant(s::MOI.EqualTo) = s.value
getconstant(s::MOI.GreaterThan) = s.lower
getconstant(s::MOI.LessThan) = s.upper
