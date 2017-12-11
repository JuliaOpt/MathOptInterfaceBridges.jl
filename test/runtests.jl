using MathOptInterfaceUtilities
const MOIU = MathOptInterfaceUtilities

using Base.Test

using MathOptInterface
const MOI = MathOptInterface

include("functions.jl")
include("sets.jl")
include("instance.jl")
include("parser.jl")
include("bridge.jl")
include("mocksolverinstance.jl")
