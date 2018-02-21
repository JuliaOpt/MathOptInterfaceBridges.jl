__precompile__()
module MathOptInterfaceBridges
const MOIB = MathOptInterfaceBridges # used in macro

using Compat # For lastindex

using MathOptInterface
const MOI = MathOptInterface
const MOIU = MathOptInterface.Utilities

const SVF = MOI.SingleVariable
const VVF = MOI.VectorOfVariables
const SAF{T} = MOI.ScalarAffineFunction{T}
const VAF{T} = MOI.VectorAffineFunction{T}
const SQF{T} = MOI.ScalarQuadraticFunction{T}
const VQF{T} = MOI.VectorQuadraticFunction{T}

const VI = MOI.VariableIndex
const CI = MOI.ConstraintIndex

include("bridge.jl")
include("intervalbridge.jl")
include("geomeanbridge.jl")
include("detbridge.jl")
include("soctosdpbridge.jl")

end # module
