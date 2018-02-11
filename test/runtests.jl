using Base.Test

using MathOptInterface
const MOI = MathOptInterface

using MathOptInterfaceTests
const MOIT = MathOptInterfaceTests

using MathOptInterfaceUtilities
const MOIU = MathOptInterfaceUtilities

# Needed by test spread over several files, defining it here make it easier to comment out tests
# Model supporting every MOI functions and sets
MOIU.@model(Model,
               (), # <- example of giving no set
               (EqualTo, GreaterThan, LessThan, Interval),
               (Reals, Zeros, Nonnegatives, Nonpositives, SecondOrderCone, RotatedSecondOrderCone, GeometricMeanCone, ExponentialCone, DualExponentialCone, PositiveSemidefiniteConeTriangle, RootDetConeTriangle, LogDetConeTriangle),
               (PowerCone, DualPowerCone),
               (SingleVariable,), # <- example of giving only one set
               (ScalarAffineFunction, ScalarQuadraticFunction),
               (VectorOfVariables,),
               (VectorAffineFunction, VectorQuadraticFunction))
# Model supporting only SecondOrderCone as non-LP cone.
@MOIU.model ModelForMock (ZeroOne, Integer) (EqualTo, GreaterThan, LessThan, Interval) (Zeros, Nonnegatives, Nonpositives, SecondOrderCone) () (SingleVariable,) (ScalarAffineFunction,) (VectorOfVariables,) (VectorAffineFunction,)

include("functions.jl")
include("sets.jl")
include("model.jl")
include("parser.jl")
include("bridge.jl")
include("mockoptimizer.jl")
include("cachingoptimizer.jl")
include("copy.jl")
