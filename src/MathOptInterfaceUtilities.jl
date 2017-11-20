__precompile__()
module MathOptInterfaceUtilities

using MathOptInterface
const MOI = MathOptInterface

const MOIU = MathOptInterfaceUtilities # used in macro

const SVF = MOI.SingleVariable
const VVF = MOI.VectorOfVariables
const SAF{T} = MOI.ScalarAffineFunction{T}
const VAF{T} = MOI.VectorAffineFunction{T}
const SQF{T} = MOI.ScalarQuadraticFunction{T}
const VQF{T} = MOI.VectorQuadraticFunction{T}

const VR = MOI.VariableReference
const CR = MOI.ConstraintReference

include("functions.jl")
include("sets.jl")

include("instance.jl")
include("parser.jl")

include("bridge.jl")
include("intervalbridge.jl")

end # module
