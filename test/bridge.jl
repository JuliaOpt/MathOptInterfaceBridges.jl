MOIU.@model SimpleModel () (EqualTo, GreaterThan, LessThan) (Zeros, Nonnegatives, Nonpositives, RotatedSecondOrderCone, GeometricMeanCone, PositiveSemidefiniteConeTriangle, ExponentialCone) () (SingleVariable,) (ScalarAffineFunction,) (VectorOfVariables,) (VectorAffineFunction,)
MOIB.@bridge SplitInterval MOIB.SplitIntervalBridge () (Interval,) () () () (ScalarAffineFunction,) () ()

@testset "Copy test" begin
    mock = MOIU.MockOptimizer(SimpleModel{Float64}())
    bridgedmock = SplitInterval{Float64}(mock)
    MOIT.failcopytestc(bridgedmock)
    MOIT.failcopytestia(bridgedmock)
    MOIT.failcopytestva(bridgedmock)
    MOIT.failcopytestca(bridgedmock)
    MOIT.copytest(bridgedmock, SimpleModel{Float64}())
end

function test_noc(bridgedmock, F, S, n)
    @test MOI.canget(bridgedmock, MOI.NumberOfConstraints{F, S}())
    @test MOI.get(bridgedmock, MOI.NumberOfConstraints{F, S}()) == n
    @test MOI.canget(bridgedmock, MOI.ListOfConstraintIndices{F, S}())
    @test length(MOI.get(bridgedmock, MOI.ListOfConstraintIndices{F, S}())) == n
end

@testset "BridgeOptimizer" begin
    const model = SplitInterval{Int}(SimpleModel{Int}())

    @testset "Custom test" begin
        x, y = MOI.addvariables!(model, 2)
        @test MOI.get(model, MOI.NumberOfVariables()) == 2

        f1 = MOI.ScalarAffineFunction([x], [3], 7)
        c1 = MOI.addconstraint!(model, f1, MOI.Interval(-1, 1))

        @test MOI.canget(model, MOI.ListOfConstraints())
        @test MOI.get(model, MOI.ListOfConstraints()) == [(MOI.ScalarAffineFunction{Int},MOI.Interval{Int})]
        test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.GreaterThan{Int}, 0)
        test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}, 1)
        @test MOI.canget(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())
        @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())) == [c1]

        f2 = MOI.ScalarAffineFunction([x, y], [2, -1], 2)
        c2 = MOI.addconstraint!(model, f1, MOI.GreaterThan(-2))

        @test MOI.canget(model, MOI.ListOfConstraints())
        @test MOI.get(model, MOI.ListOfConstraints()) == [(MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}), (MOI.ScalarAffineFunction{Int},MOI.Interval{Int})]
        test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.GreaterThan{Int}, 1)
        test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}, 1)
        @test MOI.canget(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())
        @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())) == [c1]
        @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}}())) == [c2]

        @test MOI.isvalid(model, c2)
        @test MOI.candelete(model, c2)
        MOI.delete!(model, c2)

        @test MOI.canget(model, MOI.ListOfConstraints())
        @test MOI.get(model, MOI.ListOfConstraints()) == [(MOI.ScalarAffineFunction{Int},MOI.Interval{Int})]
        test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.GreaterThan{Int}, 0)
        test_noc(model, MOI.ScalarAffineFunction{Int}, MOI.Interval{Int}, 1)
        @test MOI.canget(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())
        @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())) == [c1]
    end

    mock = MOIU.MockOptimizer(SimpleModel{Float64}())

    @testset "Continuous Linear" begin
        MOIT.contlineartest(SplitInterval{Float64}(mock), MOIT.TestConfig(solve=false))
    end
end

MOIB.@bridge GeoMean MOIB.GeoMeanBridge () () (GeometricMeanCone,) () () () (VectorOfVariables,) (VectorAffineFunction,)
MOIB.@bridge SOCtoPSD MOIB.SOCtoPSDCBridge () () (SecondOrderCone,) () () () (VectorOfVariables,) (VectorAffineFunction,)
MOIB.@bridge RSOCtoPSD MOIB.RSOCtoPSDCBridge () () (RotatedSecondOrderCone,) () () () (VectorOfVariables,) (VectorAffineFunction,)
MOIB.@bridge LogDet MOIB.LogDetBridge () () (LogDetConeTriangle,) () () () (VectorOfVariables,) (VectorAffineFunction,)
MOIB.@bridge RootDet MOIB.RootDetBridge () () (RootDetConeTriangle,) () () () (VectorOfVariables,) (VectorAffineFunction,)

@testset "Bridge tests" begin
    mock = MOIU.MockOptimizer(SimpleModel{Float64}())
    config = MOIT.TestConfig()
    mock.evalobjective = true

    @testset "Interval" begin
        MOIU.set_mock_optimize!(mock,
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [5.0, 5.0],
                  (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [0],
                  (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [-1]),
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [2.5, 2.5],
                  (MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}) => [1],
                  (MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64})    => [0]),
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1.0]),
             (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [6.0, 6.0]))
        bridgedmock = SplitInterval{Float64}(mock)
        MOIT.linear10test(bridgedmock, config)
        ci = first(MOI.get(bridgedmock, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64}}()))
        @test MOI.canmodifyconstraint(bridgedmock, ci, MOI.ScalarAffineFunction{Float64})
        newf = MOI.ScalarAffineFunction(MOI.get(bridgedmock, MOI.ListOfVariableIndices()), [1., -1.], 0.)
        MOI.modifyconstraint!(bridgedmock, ci, newf)
        @test MOI.canget(bridgedmock, MOI.ConstraintFunction(), typeof(ci))
        @test MOI.get(bridgedmock, MOI.ConstraintFunction(), ci) ≈ newf
        # Test deletion
        @test MOI.get(bridgedmock, MOI.NumberOfVariables()) == 2
        test_noc(bridgedmock, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0)
        test_noc(bridgedmock, MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64},    1)
        test_noc(bridgedmock, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64},    0)
        @test MOI.isvalid(bridgedmock, ci)
        @test MOI.candelete(bridgedmock, ci)
        MOI.delete!(bridgedmock, ci)
        @test isempty(bridgedmock.bridges)
        @test MOI.get(bridgedmock, MOI.NumberOfVariables()) == 2
        test_noc(bridgedmock, MOI.ScalarAffineFunction{Float64}, MOI.GreaterThan{Float64}, 0)
        test_noc(bridgedmock, MOI.ScalarAffineFunction{Float64}, MOI.Interval{Float64},    0)
        test_noc(bridgedmock, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64},    0)
    end

    @testset "GeoMean" begin
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [ones(4); 2; √2; √2])
        bridgedmock = GeoMean{Float64}(mock)
        MOIT.geomean1vtest(bridgedmock, config)
        MOIT.geomean1ftest(bridgedmock, config)
        @test !MOI.canget(bridgedmock, MOI.ConstraintDual(), MOI.ConstraintIndex{MOI.VectorOfVariables, MOI.GeometricMeanCone})
        ci = first(MOI.get(bridgedmock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}()))
        @test !MOI.canmodifyconstraint(bridgedmock, ci, MOI.VectorAffineFunction{Float64})
        # Test deletion
        @test MOI.get(bridgedmock, MOI.NumberOfVariables()) == 4
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone,      1)
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0)
        test_noc(bridgedmock, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64},      1)
        @test MOI.isvalid(bridgedmock, ci)
        @test MOI.candelete(bridgedmock, ci)
        MOI.delete!(bridgedmock, ci)
        @test !MOI.isvalid(bridgedmock, ci)
        @test isempty(bridgedmock.bridges)
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone,      0)
        # As the bridge has been removed, if the constraints it has created where not removed, it wouldn't be there to decrease this counter anymore
        @test MOI.get(bridgedmock, MOI.NumberOfVariables()) == 4
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0)
        test_noc(bridgedmock, MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64},      1)
    end

    @testset "SOCtoPSD" begin
        bridgedmock = SOCtoPSD{Float64}(mock)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1.0, 1/√2, 1/√2],
                              (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [[√2/2, -1/2, √2/4, -1/2, √2/4, √2/4]],
                              (MOI.VectorAffineFunction{Float64}, MOI.Zeros)                            => [[-√2]])
        MOIT.soc1vtest(bridgedmock, config)
        MOIT.soc1ftest(bridgedmock, config)
        ci = first(MOI.get(bridgedmock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone}()))
        @test !MOI.canmodifyconstraint(bridgedmock, ci, MOI.VectorAffineFunction{Float64})
        @test MOI.get(bridgedmock, MOI.NumberOfVariables()) == 3
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone, 1)
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0)
        @test MOI.isvalid(bridgedmock, ci)
        @test MOI.candelete(bridgedmock, ci)
        MOI.delete!(bridgedmock, ci)
        @test !MOI.isvalid(bridgedmock, ci)
        @test isempty(bridgedmock.bridges)
        @test MOI.get(bridgedmock, MOI.NumberOfVariables()) == 3
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone, 0)
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0)
    end

    @testset "RSOCtoPSD" begin
        bridgedmock = RSOCtoPSD{Float64}(mock)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/√2, 1/√2, 0.5, 1.0],
                              (MOI.SingleVariable,                MOI.EqualTo{Float64})       => [-√2, -1/√2],
                              (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [[√2, -1/2, √2/8, -1/2, √2/8, √2/8]])
        MOIT.rotatedsoc1vtest(bridgedmock, config)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1/√2, 1/√2],
                              (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [[√2, -1/2, √2/8, -1/2, √2/8, √2/8]])
        MOIT.rotatedsoc1ftest(bridgedmock, config)
        ci = first(MOI.get(bridgedmock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone}()))
        @test !MOI.canmodifyconstraint(bridgedmock, ci, MOI.VectorAffineFunction{Float64})
        @test MOI.get(bridgedmock, MOI.NumberOfVariables()) == 2
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 1)
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0)
        @test MOI.isvalid(bridgedmock, ci)
        @test MOI.candelete(bridgedmock, ci)
        MOI.delete!(bridgedmock, ci)
        @test !MOI.isvalid(bridgedmock, ci)
        @test isempty(bridgedmock.bridges)
        @test MOI.get(bridgedmock, MOI.NumberOfVariables()) == 2
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0)
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0)
    end

    @testset "LogDet" begin
        bridgedmock = LogDet{Float64}(mock)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [0, 1, 0, 1, 1, 0, 1, 0, 0])
        MOIT.logdet1tvtest(bridgedmock, config)
        MOIT.logdet1tftest(bridgedmock, config)
        @test !MOI.canget(bridgedmock, MOI.ConstraintDual(), MOI.ConstraintIndex{MOI.VectorAffineFunction{Float64}, MOI.LogDetConeTriangle})
        ci = first(MOI.get(bridgedmock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.LogDetConeTriangle}()))
        @test !MOI.canmodifyconstraint(bridgedmock, ci, MOI.VectorAffineFunction{Float64})
        @test MOI.get(bridgedmock, MOI.NumberOfVariables()) == 4
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.LogDetConeTriangle, 1)
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone, 0)
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0)
        @test MOI.isvalid(bridgedmock, ci)
        @test MOI.candelete(bridgedmock, ci)
        MOI.delete!(bridgedmock, ci)
        @test !MOI.isvalid(bridgedmock, ci)
        @test isempty(bridgedmock.bridges)
        @test MOI.get(bridgedmock, MOI.NumberOfVariables()) == 4
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.LogDetConeTriangle, 0)
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.ExponentialCone, 0)
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0)
    end

    @testset "RootDet" begin
        bridgedmock = RootDet{Float64}(mock)
        mock.optimize! = (mock::MOIU.MockOptimizer) -> MOIU.mock_optimize!(mock, [1, 1, 0, 1, 1, 0, 1])
        MOIT.rootdet1tvtest(bridgedmock, config)
        MOIT.rootdet1tftest(bridgedmock, config)
        @test !MOI.canget(bridgedmock, MOI.ConstraintDual(), MOI.ConstraintIndex{MOI.VectorAffineFunction{Float64}, MOI.RootDetConeTriangle})
        ci = first(MOI.get(bridgedmock, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.RootDetConeTriangle}()))
        @test !MOI.canmodifyconstraint(bridgedmock, ci, MOI.VectorAffineFunction{Float64})
        @test MOI.get(bridgedmock, MOI.NumberOfVariables()) == 4
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.RootDetConeTriangle, 1)
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0)
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0)
        @test MOI.isvalid(bridgedmock, ci)
        @test MOI.candelete(bridgedmock, ci)
        MOI.delete!(bridgedmock, ci)
        @test !MOI.isvalid(bridgedmock, ci)
        @test isempty(bridgedmock.bridges)
        @test MOI.get(bridgedmock, MOI.NumberOfVariables()) == 4
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.RootDetConeTriangle, 0)
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone, 0)
        test_noc(bridgedmock, MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle, 0)
    end
end
