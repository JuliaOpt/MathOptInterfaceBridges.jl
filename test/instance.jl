MOIU.@instance LPInstance () (EqualTo, GreaterThan, LessThan, Interval) (Zeros, Nonnegatives, Nonpositives) () (SingleVariable,) (ScalarAffineFunction,) (VectorOfVariables,) (VectorAffineFunction,)

MOIU.@instance(Instance,
               (), # <- example of giving no set
               (EqualTo, GreaterThan, LessThan, Interval),
               (Reals, Zeros, Nonnegatives, Nonpositives, SecondOrderCone, RotatedSecondOrderCone, GeometricMeanCone, ExponentialCone, DualExponentialCone, PositiveSemidefiniteConeTriangle, RootDetConeTriangle, LogDetConeTriangle),
               (PowerCone, DualPowerCone),
               (SingleVariable,), # <- example of giving only one set
               (ScalarAffineFunction, ScalarQuadraticFunction),
               (VectorOfVariables,),
               (VectorAffineFunction, VectorQuadraticFunction))

using MathOptInterfaceTests
const MOIT = MathOptInterfaceTests

@testset "Name test" begin
    MOIT.nametest(Instance{Float64}())
end

@testset "Valid test" begin
    MOIT.validtest(Instance{Float64}())
end

@testset "Empty test" begin
    MOIT.emptytest(Instance{Float64}())
end

@testset "Copy test" begin
    MOIT.failcopytestc(Instance{Float64}())
    MOIT.failcopytestia(Instance{Float64}())
    MOIT.failcopytestva(Instance{Float64}())
    MOIT.failcopytestca(Instance{Float64}())
    MOIT.copytest(Instance{Float64}(), Instance{Float64}())
end

@testset "canaddconstraint test" begin
    MOIT.canaddconstrainttest(Instance{Float64}(), Float64, Int)
    MOIT.canaddconstrainttest(Instance{Int}(), Int, Float64)
end

const solver = () -> Instance{Float64}()
# Only config.query is true as MOI.optimize! is not implemented
const config = MOIT.TestConfig(solve=false)

@testset "Continuous Linear tests" begin
    MOIT.contlineartest(solver, config)
end

@testset "Continuous Conic tests" begin
    MOIT.contconictest(solver, config)
end

@testset "Quadratic functions" begin

    instance = Instance{Int}()

    x, y = MOI.addvariables!(instance, 2)
    @test MOI.get(instance, MOI.NumberOfVariables()) == 2

    f1 = MOI.ScalarQuadraticFunction([x], [3], [x, y, x], [x, y, y], [1, 2, 3], 7)
    c1 = MOI.addconstraint!(instance, f1, MOI.Interval(-1, 1))

    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarQuadraticFunction{Int},MOI.Interval{Int}}()) == 1
    @test MOI.canget(instance, MOI.ListOfConstraintIndices{MOI.ScalarQuadraticFunction{Int},MOI.Interval{Int}}())
    @test (@inferred MOI.get(instance, MOI.ListOfConstraintIndices{MOI.ScalarQuadraticFunction{Int},MOI.Interval{Int}}())) == [c1]

    f2 = MOI.VectorQuadraticFunction([1, 2, 2], [x, x, y], [3, 1, 2], [1, 1, 2], [x, y, x], [x, y, y], [1, 2, 3], [7, 3, 4])
    c2 = MOI.addconstraint!(instance, f2, MOI.PositiveSemidefiniteConeTriangle(3))

    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle}()) == 1
    @test MOI.canget(instance, MOI.ListOfConstraintIndices{MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle}())
    @test (@inferred MOI.get(instance, MOI.ListOfConstraintIndices{MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle}())) == [c2]

    @test MOI.canget(instance, MOI.ListOfConstraints())
    loc = MOI.get(instance, MOI.ListOfConstraints())
    @test length(loc) == 2
    @test (MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle) in loc
    @test (MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle) in loc

    f3 = MOIU.modifyfunction(f1, MOI.ScalarConstantChange(9))
    f3 = MOIU.modifyfunction(f3, MOI.ScalarCoefficientChange(y, 2))

    @test !(MOI.get(instance, MOI.ConstraintFunction(), c1) ≈ f3)
    MOI.modifyconstraint!(instance, c1, f3)
    @test MOI.get(instance, MOI.ConstraintFunction(), c1) ≈ f3

    f4 = MOI.VectorAffineFunction([1, 1, 2], [x, y, y], [2, 4, 3], [5, 7])
    c4 = MOI.addconstraint!(instance, f4, MOI.SecondOrderCone(2))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Int},MOI.SecondOrderCone}()) == 1

    f5 = MOI.VectorOfVariables([x])
    c5 = MOI.addconstraint!(instance, f5, MOI.RotatedSecondOrderCone(1))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.RotatedSecondOrderCone}()) == 1

    f6 = MOI.VectorAffineFunction([1, 2], [x, y], [2, 9], [6, 8])
    c6 = MOI.addconstraint!(instance, f6, MOI.SecondOrderCone(2))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Int},MOI.SecondOrderCone}()) == 2

    f7 = MOI.VectorOfVariables([x, y])
    c7 = MOI.addconstraint!(instance, f7, MOI.Nonpositives(2))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonpositives}()) == 1

    loc1 = MOI.get(instance, MOI.ListOfConstraints())
    loc2 = Vector{Tuple{DataType, DataType}}()
    function _pushloc{F, S}(constrs::Vector{MOIU.C{F, S}})
        if !isempty(constrs)
            push!(loc2, (F, S))
        end
    end
    MOIU.broadcastcall(_pushloc, instance)
    for loc in (loc1, loc2)
        @test length(loc) == 5
        @test (MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle) in loc
        @test (MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle) in loc
        @test (MOI.VectorOfVariables,MOI.RotatedSecondOrderCone) in loc
        @test (MOI.VectorAffineFunction{Int},MOI.SecondOrderCone) in loc
        @test (MOI.VectorOfVariables,MOI.Nonpositives) in loc
    end

    @test MOI.isvalid(instance, c4)
    MOI.delete!(instance, c4)
    @test !MOI.isvalid(instance, c4)

    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Int},MOI.SecondOrderCone}()) == 1
    @test MOI.get(instance, MOI.ConstraintFunction(), c6).constant == f6.constant

    MOI.delete!(instance, y)

    f = MOI.get(instance, MOI.ConstraintFunction(), c2)
    @test f.affine_outputindex == [1, 2]
    @test f.affine_variables == [x, x]
    @test f.affine_coefficients == [3, 1]
    @test f.quadratic_outputindex == [1]
    @test f.quadratic_rowvariables == [x]
    @test f.quadratic_colvariables == [x]
    @test f.quadratic_coefficients == [1]
    @test f.constant == [7, 3, 4]

    f =  MOI.get(instance, MOI.ConstraintFunction(), c6)
    @test f.outputindex == [1]
    @test f.variables == [x]
    @test f.coefficients == [2]
    @test f.constant == [6, 8]

    f =  MOI.get(instance, MOI.ConstraintFunction(), c7)
    @test f.variables == [x]

    s =  MOI.get(instance, MOI.ConstraintSet(), c7)
    @test MOI.dimension(s) == 1

end
