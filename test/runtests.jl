using Base.Test

using MathOptInterface
const MOI = MathOptInterface

using MathOptInterfaceTests
const MOIT = MathOptInterfaceTests

using MathOptInterfaceUtilities
const MOIU = MathOptInterfaceUtilities

include("functions.jl")
include("sets.jl")
include("instance.jl")
include("parser.jl")
include("bridge.jl")
include("mocksolverinstance.jl")
include("instancemanager.jl")
