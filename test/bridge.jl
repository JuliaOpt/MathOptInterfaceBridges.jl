MOIU.@model SimpleModel () (EqualTo, GreaterThan, LessThan) (Zeros, Nonnegatives, Nonpositives, RotatedSecondOrderCone, PositiveSemidefiniteConeTriangle) () (SingleVariable,) (ScalarAffineFunction,) (VectorOfVariables,) (VectorAffineFunction,)
MOIB.@bridge SplitInterval MOIB.SplitIntervalBridge () (Interval,) () () () (ScalarAffineFunction,) () ()

@testset "Interval bridge" begin
    const model = SplitInterval{Int}(SimpleModel{Int}())

    x, y = MOI.addvariables!(model, 2)
    @test MOI.get(model, MOI.NumberOfVariables()) == 2

    f1 = MOI.ScalarAffineFunction([x], [3], 7)
    c1 = MOI.addconstraint!(model, f1, MOI.Interval(-1, 1))

    @test MOI.canget(model, MOI.ListOfConstraints())
    @test MOI.get(model, MOI.ListOfConstraints()) == [(MOI.ScalarAffineFunction{Int},MOI.Interval{Int})]
    @test MOI.canget(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}}())
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}}()) == 0
    @test MOI.canget(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}()) == 1
    @test MOI.canget(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())
    @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())) == [c1]

    f2 = MOI.ScalarAffineFunction([x, y], [2, -1], 2)
    c2 = MOI.addconstraint!(model, f1, MOI.GreaterThan(-2))

    @test MOI.canget(model, MOI.ListOfConstraints())
    @test MOI.get(model, MOI.ListOfConstraints()) == [(MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}), (MOI.ScalarAffineFunction{Int},MOI.Interval{Int})]
    @test MOI.canget(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}}())
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}}()) == 1
    @test MOI.canget(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}}())
    @test MOI.get(model, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}()) == 1
    @test MOI.canget(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())
    @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.Interval{Int}}())) == [c1]
    @test (@inferred MOI.get(model, MOI.ListOfConstraintIndices{MOI.ScalarAffineFunction{Int},MOI.GreaterThan{Int}}())) == [c2]
end

MOIB.@bridge GeoMean MOIB.GeoMeanBridge () () (GeometricMeanCone,) () () () (VectorOfVariables,) (VectorAffineFunction,)
MOIB.@bridge SOCtoPSD MOIB.SOCtoPSDCBridge () () (SecondOrderCone,) () () () (VectorOfVariables,) (VectorAffineFunction,)
MOIB.@bridge RSOCtoPSD MOIB.RSOCtoPSDCBridge () () (RotatedSecondOrderCone,) () () () (VectorOfVariables,) (VectorAffineFunction,)
MOIB.@bridge RootDet MOIB.RootDetBridge () () (RootDetConeTriangle,) () () () (VectorOfVariables,) (VectorAffineFunction,)

@testset "Bridge tests" begin
    optimizer = MOIU.MockOptimizer(SimpleModel{Float64}())
    config = MOIT.TestConfig()
    optimizer.evalobjective = true

    @testset "GeoMeanBridge" begin
        optimizer.optimize! = (optimizer::MOIU.MockOptimizer) -> MOIU.mock_optimize!(optimizer, [ones(4); 2; √2; √2])
        bridgedoptimizer = GeoMean{Float64}(optimizer)
        @test !MOI.canget(bridgedoptimizer, MOI.ConstraintDual(), MOI.ConstraintIndex{MOI.VectorOfVariables, MOI.GeometricMeanCone})
        MOIT.geomean1vtest(bridgedoptimizer, config)
        MOIT.geomean1ftest(bridgedoptimizer, config)
        ci = first(MOI.get(bridgedoptimizer, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}()))
        @test !MOI.canmodifyconstraint(bridgedoptimizer, ci, MOI.VectorOfVariables)
        # Test deletion
        @test MOI.get(bridgedoptimizer, MOI.NumberOfVariables()) == 4
        @test MOI.get(bridgedoptimizer, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}()) == 1
        @test MOI.get(bridgedoptimizer, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone}()) == 0
        @test isempty(MOI.get(bridgedoptimizer, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone}()))
        @test MOI.get(bridgedoptimizer, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}()) == 1
        @test MOI.isvalid(bridgedoptimizer, ci)
        MOI.delete!(bridgedoptimizer, ci)
        @test !MOI.isvalid(bridgedoptimizer, ci)
        @test isempty(bridgedoptimizer.bridges)
        @test MOI.get(bridgedoptimizer, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}()) == 0
        @test isempty(MOI.get(bridgedoptimizer, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.GeometricMeanCone}()))
        # As the bridge has been removed, if the constraints it has created where not removed, it wouldn't be there to decrease this counter anymore
        @test MOI.get(bridgedoptimizer, MOI.NumberOfVariables()) == 4
        @test MOI.get(bridgedoptimizer, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone}()) == 0
        @test isempty(MOI.get(bridgedoptimizer, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone}()))
        @test MOI.get(bridgedoptimizer, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64}, MOI.LessThan{Float64}}()) == 1
    end

    @testset "SOCtoPSDBridge" begin
        bridgedoptimizer = SOCtoPSD{Float64}(optimizer)
        optimizer.optimize! = (optimizer::MOIU.MockOptimizer) -> MOIU.mock_optimize!(optimizer, [1.0, 1/√2, 1/√2],
                              (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [[√2/2, -1/2, √2/4, -1/2, √2/4, √2/4]],
                              (MOI.VectorAffineFunction{Float64}, MOI.Zeros)                            => [[-√2]])
        MOIT.soc1vtest(bridgedoptimizer, config)
        MOIT.soc1ftest(bridgedoptimizer, config)
        ci = first(MOI.get(bridgedoptimizer, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone}()))
        @test MOI.get(bridgedoptimizer, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone}()) == 1
        @test MOI.get(bridgedoptimizer, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle}()) == 0
        @test isempty(MOI.get(bridgedoptimizer, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle}()))
        @test MOI.isvalid(bridgedoptimizer, ci)
        MOI.delete!(bridgedoptimizer, ci)
        @test !MOI.isvalid(bridgedoptimizer, ci)
        @test MOI.get(bridgedoptimizer, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.SecondOrderCone}()) == 0
        @test MOI.get(bridgedoptimizer, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle}()) == 0
        @test isempty(MOI.get(bridgedoptimizer, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle}()))
    end

    @testset "RSOCtoPSDBridge" begin
        bridgedoptimizer = RSOCtoPSD{Float64}(optimizer)
        optimizer.optimize! = (optimizer::MOIU.MockOptimizer) -> MOIU.mock_optimize!(optimizer, [1/√2, 1/√2, 0.5, 1.0],
                              (MOI.SingleVariable,                MOI.EqualTo{Float64})       => [-√2, -1/√2],
                              (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [[√2, -1/2, √2/8, -1/2, √2/8, √2/8]])
        MOIT.rotatedsoc1vtest(bridgedoptimizer, config)
        optimizer.optimize! = (optimizer::MOIU.MockOptimizer) -> MOIU.mock_optimize!(optimizer, [1/√2, 1/√2],
                              (MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle) => [[√2, -1/2, √2/8, -1/2, √2/8, √2/8]])
        MOIT.rotatedsoc1ftest(bridgedoptimizer, config)
        ci = first(MOI.get(bridgedoptimizer, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone}()))
        @test MOI.get(bridgedoptimizer, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone}()) == 1
        @test MOI.get(bridgedoptimizer, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle}()) == 0
        @test isempty(MOI.get(bridgedoptimizer, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle}()))
        @test MOI.isvalid(bridgedoptimizer, ci)
        MOI.delete!(bridgedoptimizer, ci)
        @test !MOI.isvalid(bridgedoptimizer, ci)
        @test MOI.get(bridgedoptimizer, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.RotatedSecondOrderCone}()) == 0
        @test MOI.get(bridgedoptimizer, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle}()) == 0
        @test isempty(MOI.get(bridgedoptimizer, MOI.ListOfConstraintIndices{MOI.VectorAffineFunction{Float64}, MOI.PositiveSemidefiniteConeTriangle}()))
    end

    config = MOIT.TestConfig(solve=false)

    @testset "RootDetBridge" begin
        MOIT.rootdet1tvtest(RootDet{Float64}(GeoMean{Float64}(SimpleModel{Float64}())), config)
        MOIT.rootdet1tftest(RootDet{Float64}(GeoMean{Float64}(SimpleModel{Float64}())), config)
    end
end
