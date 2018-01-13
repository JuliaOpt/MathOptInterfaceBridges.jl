
@MOIU.instance InstanceForManager (ZeroOne, Integer) (EqualTo, GreaterThan, LessThan, Interval) (Zeros, Nonnegatives, Nonpositives, SecondOrderCone) () (SingleVariable,) (ScalarAffineFunction,ScalarQuadraticFunction) (VectorOfVariables,) (VectorAffineFunction,)

@testset "InstanceManager Manual mode" begin
    m = MOIU.InstanceManager(InstanceForManager{Float64}(), MOIU.Manual)
    @test MOIU.state(m) == MOIU.NoSolver

    s = MOIU.MockSolverInstance(InstanceForMock{Float64}())
    @test MOI.isempty(s)
    MOIU.resetsolver!(m, s)
    @test MOIU.state(m) == MOIU.EmptySolver

    v = MOI.addvariable!(m)
    x = MOI.addvariables!(m, 2)
    saf = MOI.ScalarAffineFunction([v;x], [1.0,2.0,3.0], 0.0)
    @test MOI.canset(m, MOI.ObjectiveFunction())
    MOI.set!(m, MOI.ObjectiveFunction(), saf)
    @test MOI.get(m, MOIU.AttributeFromInstance(MOI.ObjectiveFunction())) ≈ saf

    @test_throws AssertionError MOI.optimize!(m)

    MOIU.attachsolver!(m)
    @test MOIU.state(m) == MOIU.AttachedSolver
    # This test is incorrect because ObjectiveFunction is returned in terms of
    # the solver's variable indices.
    #@test MOI.get(m, MOIU.AttributeFromSolver(MOI.ObjectiveFunction())) ≈ saf

    @test MOI.canset(m, MOI.ObjectiveSense())
    MOI.set!(m, MOI.ObjectiveSense(), MOI.MaxSense)
    @test MOI.canget(m, MOIU.AttributeFromInstance(MOI.ObjectiveSense()))
    @test MOI.canget(m, MOIU.AttributeFromSolver(MOI.ObjectiveSense()))
    @test MOI.get(m, MOIU.AttributeFromInstance(MOI.ObjectiveSense())) == MOI.MaxSense
    @test MOI.get(m, MOIU.AttributeFromSolver(MOI.ObjectiveSense())) == MOI.MaxSense

    @test !MOI.canset(m, MOI.NumberOfVariables())
    @test !MOI.canget(m, MOIU.AttributeFromInstance(MOIU.MockInstanceAttribute()))
    @test MOI.canget(m, MOIU.AttributeFromSolver((MOIU.MockInstanceAttribute())))

    @test MOI.canset(m, MOIU.AttributeFromSolver(MOIU.MockInstanceAttribute()))
    MOI.set!(m, MOIU.AttributeFromSolver(MOIU.MockInstanceAttribute()), 10)
    @test MOI.get(m, MOIU.AttributeFromSolver(MOIU.MockInstanceAttribute())) == 10

    MOI.set!(m, MOIU.AttributeFromSolver(MOI.ResultCount()), 1)
    @test MOI.canset(m, MOIU.AttributeFromSolver(MOI.VariablePrimal()), typeof(v))
    MOI.set!(m, MOIU.AttributeFromSolver(MOI.VariablePrimal()), v, 3.0)

    MOI.optimize!(m)

    @test MOI.canget(m, MOIU.AttributeFromSolver(MOI.VariablePrimal()), typeof(v))
    @test MOI.get(m, MOIU.AttributeFromSolver(MOI.VariablePrimal()), v) == 3.0

    # InstanceForMock doesn't support SecondOrderCone
    @test !MOI.canaddconstraint(m, MOI.VectorOfVariables([v]), MOI.SecondOrderCone(1))

    MOIU.dropsolver!(m)
    @test MOIU.state(m) == MOIU.NoSolver

end

@testset "InstanceManager Automatic mode" begin
    m = MOIU.InstanceManager(InstanceForManager{Float64}(), MOIU.Automatic)
    @test MOIU.state(m) == MOIU.NoSolver

    s = MOIU.MockSolverInstance(InstanceForMock{Float64}())
    @test MOI.isempty(s)
    MOIU.resetsolver!(m, s)
    @test MOIU.state(m) == MOIU.EmptySolver

    v = MOI.addvariable!(m)
    saf = MOI.ScalarAffineFunction([v], [1.0], 0.0)
    @test MOI.canset(m, MOI.ObjectiveFunction())
    MOI.set!(m, MOI.ObjectiveFunction(), saf)
    @test MOI.get(m, MOIU.AttributeFromInstance(MOI.ObjectiveFunction())) ≈ saf

    MOI.optimize!(m)
    @test MOIU.state(m) == MOIU.AttachedSolver
    @test MOI.get(m, MOIU.AttributeFromSolver(MOI.ResultCount())) == 0

    @test MOI.canset(m, MOI.ObjectiveSense())
    MOI.set!(m, MOI.ObjectiveSense(), MOI.MaxSense)
    @test MOI.canget(m, MOIU.AttributeFromInstance(MOI.ObjectiveSense()))
    @test MOI.canget(m, MOIU.AttributeFromSolver(MOI.ObjectiveSense()))
    @test MOI.get(m, MOIU.AttributeFromInstance(MOI.ObjectiveSense())) == MOI.MaxSense
    @test MOI.get(m, MOIU.AttributeFromSolver(MOI.ObjectiveSense())) == MOI.MaxSense

    @test !MOI.canget(m, MOIU.AttributeFromInstance(MOIU.MockInstanceAttribute()))
    @test MOI.canget(m, MOIU.AttributeFromSolver((MOIU.MockInstanceAttribute())))

    @test MOI.canset(m, MOIU.AttributeFromSolver(MOIU.MockInstanceAttribute()))
    MOI.set!(m, MOIU.AttributeFromSolver(MOIU.MockInstanceAttribute()), 10)
    @test MOI.get(m, MOIU.AttributeFromSolver(MOIU.MockInstanceAttribute())) == 10

    MOI.set!(m, MOIU.AttributeFromSolver(MOI.ResultCount()), 1)
    @test MOI.canset(m, MOIU.AttributeFromSolver(MOI.VariablePrimal()), typeof(v))
    MOI.set!(m, MOIU.AttributeFromSolver(MOI.VariablePrimal()), v, 3.0)

    MOI.optimize!(m)

    @test MOI.canget(m, MOIU.AttributeFromSolver(MOI.VariablePrimal()), typeof(v))
    @test MOI.get(m, MOIU.AttributeFromSolver(MOI.VariablePrimal()), v) == 3.0

    # InstanceForMock doesn't support SecondOrderCone
    MOI.addconstraint!(m, MOI.VectorOfVariables([v]), MOI.SecondOrderCone(1))
    @test MOIU.state(m) == MOIU.EmptySolver

end
