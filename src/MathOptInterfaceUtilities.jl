module MathOptInterfaceUtilities

using MathOptInterface
const MOI = MathOptInterface

const SVF = MOI.SingleVariable
const VVF = MOI.VectorOfVariables
const SAF{T} = MOI.ScalarAffineFunction{T}
const VAF{T} = MOI.VectorAffineFunction{T}
const SQF{T} = MOI.ScalarQuadraticFunction{T}
const VQF{T} = MOI.VectorQuadraticFunction{T}

include("functions.jl")
include("sets.jl")

const CR = MOI.ConstraintReference

include("instance.jl")

end # module
