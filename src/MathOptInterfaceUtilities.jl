module MathOptInterfaceUtilities

using MathOptInterface
const MOI = MathOptInterface

include("functions.jl")

const CR = MOI.ConstraintReference

include("instance.jl")

end # module
