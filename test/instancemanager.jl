
@MOIU.instance InstanceForManager (ZeroOne, Integer) (EqualTo, GreaterThan, LessThan, Interval) (Zeros, Nonnegatives, Nonpositives, SecondOrderCone, RotatedSecondOrderCone, GeometricMeanCone, ExponentialCone, DualExponentialCone, PositiveSemidefiniteConeTriangle, RootDetConeTriangle, LogDetConeTriangle) () (SingleVariable,) (ScalarAffineFunction,ScalarQuadraticFunction) (VectorOfVariables,) (VectorAffineFunction,)

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
    @test MOI.canset(m, MOI.ObjectiveFunction{typeof(saf)}())
    MOI.set!(m, MOI.ObjectiveFunction{typeof(saf)}(), saf)
    @test MOI.get(m, MOIU.AttributeFromInstance(MOI.ObjectiveFunction{typeof(saf)}())) ≈ saf
    @test MOI.get(m, MOI.ObjectiveFunction{typeof(saf)}()) ≈ saf
    @test !MOI.canget(m, MOIU.AttributeFromSolver(MOI.ObjectiveSense()))

    @test_throws AssertionError MOI.optimize!(m)

    MOIU.attachsolver!(m)
    @test MOIU.state(m) == MOIU.AttachedSolver
    @test MOI.get(m, MOIU.AttributeFromSolver(MOI.ObjectiveFunction{typeof(saf)}())) ≈ saf

    @test MOI.canset(m, MOI.ObjectiveSense())
    MOI.set!(m, MOI.ObjectiveSense(), MOI.MaxSense)
    @test MOI.canget(m, MOI.ObjectiveSense())
    @test MOI.canget(m, MOIU.AttributeFromInstance(MOI.ObjectiveSense()))
    @test MOI.canget(m, MOIU.AttributeFromSolver(MOI.ObjectiveSense()))
    @test MOI.get(m, MOI.ObjectiveSense()) == MOI.MaxSense
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

    @test MOI.canget(m, MOI.VariablePrimal(), typeof(v))
    @test MOI.get(m, MOI.VariablePrimal(), v) == 3.0
    @test MOI.get(m, MOI.VariablePrimal(), [v]) == [3.0]
    @test MOI.canget(m, MOIU.AttributeFromSolver(MOI.VariablePrimal()), typeof(v))
    @test MOI.get(m, MOIU.AttributeFromSolver(MOI.VariablePrimal()), v) == 3.0

    # InstanceForMock doesn't support RotatedSecondOrderCone
    @test !MOI.canaddconstraint(m, MOI.VectorOfVariables, MOI.RotatedSecondOrderCone)

    @test MOI.canaddconstraint(m.instance, MOI.SingleVariable, MOI.LessThan{Float64})
    @test MOI.canaddconstraint(m.solver.instance, MOI.SingleVariable, MOI.LessThan{Float64})
    @test MOI.canaddconstraint(m.solver, MOI.SingleVariable, MOI.LessThan{Float64})
    @test MOI.canaddconstraint(m, MOI.SingleVariable, MOI.LessThan{Float64})
    lb = MOI.addconstraint!(m, MOI.SingleVariable(v), MOI.LessThan(10.0))
    @test MOI.canmodifyconstraint(m, lb, MOI.LessThan{Float64})
    MOI.modifyconstraint!(m, lb, MOI.LessThan(11.0))
    @test MOI.get(m, MOI.ConstraintSet(), lb) == MOI.LessThan(11.0)
    @test MOI.get(m, MOI.ConstraintFunction(), lb) == MOI.SingleVariable(v)

    MOIU.dropsolver!(m)
    @test MOIU.state(m) == MOIU.NoSolver

    @test MOI.canmodifyconstraint(m, lb, MOI.LessThan{Float64})
    MOI.modifyconstraint!(m, lb, MOI.LessThan(12.0))
    @test MOI.get(m, MOI.ConstraintSet(), lb) == MOI.LessThan(12.0)

    @test MOI.candelete(m, x[2])
    MOI.delete!(m, x[2])
    @test !MOI.isvalid(m, x[2])

    # TODO: test more constraint modifications


end

@testset "InstanceManager Automatic mode" begin
    m = MOIU.InstanceManager(InstanceForManager{Float64}(), MOIU.Automatic)
    @test MOIU.state(m) == MOIU.NoSolver

    v = MOI.addvariable!(m)
    @test MOI.canset(m, MOI.VariableName(), typeof(v))
    MOI.set!(m, MOI.VariableName(), v, "v")
    @test MOI.canget(m, MOI.VariableName(), typeof(v))
    @test MOI.get(m, MOI.VariableName(), v) == "v"

    s = MOIU.MockSolverInstance(InstanceForMock{Float64}())
    @test MOI.isempty(s)
    MOIU.resetsolver!(m, s)
    @test MOIU.state(m) == MOIU.EmptySolver

    saf = MOI.ScalarAffineFunction([v], [1.0], 0.0)
    @test MOI.canset(m, MOI.ObjectiveFunction{typeof(saf)}())
    MOI.set!(m, MOI.ObjectiveFunction{typeof(saf)}(), saf)
    @test MOI.get(m, MOIU.AttributeFromInstance(MOI.ObjectiveFunction{typeof(saf)}())) ≈ saf

    MOI.optimize!(m)
    @test MOIU.state(m) == MOIU.AttachedSolver
    @test MOI.get(m, MOIU.AttributeFromSolver(MOI.ResultCount())) == 0

    @test MOI.canget(m, MOI.VariableName(), typeof(v))
    @test MOI.get(m, MOI.VariableName(), v) == "v"
    @test MOI.canget(m, MOIU.AttributeFromInstance(MOI.VariableName()), typeof(v))
    @test MOI.get(m, MOIU.AttributeFromInstance(MOI.VariableName()), v) == "v"
    @test MOI.canget(m, MOIU.AttributeFromSolver(MOI.VariableName()), typeof(v))
    @test MOI.get(m, MOIU.AttributeFromSolver(MOI.VariableName()), v) == "v"

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

    # InstanceForMock doesn't support RotatedSecondOrderCone
    MOI.addconstraint!(m, MOI.VectorOfVariables([v]), MOI.RotatedSecondOrderCone(1))
    @test MOIU.state(m) == MOIU.EmptySolver

    # TODO: test modifyconstraint! with a change that forces the solver to be dropped

    MOI.empty!(m)
    @test MOIU.state(m) == MOIU.AttachedSolver

    m.solver.canaddvar = false # Simulate solver for which MOI.canaddvariable returns false
    MOI.addvariable!(m)
    @test MOIU.state(m) == MOIU.EmptySolver
    res = MOIU.attachsolver!(m)
    @test res.status == MOI.CopyOtherError
    @test MOIU.state(m) == MOIU.EmptySolver

    m.solver.canaddvar = true
    res = MOIU.attachsolver!(m)
    @test res.status == MOI.CopySuccess
    @test MOIU.state(m) == MOIU.AttachedSolver
    m.solver.canaddvar = false
    MOI.addvariables!(m, 2)
    @test MOIU.state(m) == MOIU.EmptySolver
end

@testset "InstanceManager constructor with solver" begin
    s = MOIU.MockSolverInstance(InstanceForMock{Float64}())
    m = MOIU.InstanceManager(InstanceForManager{Float64}(), s)
    @test MOI.isempty(m)
    @test MOIU.state(m) == MOIU.AttachedSolver
    @test MOIU.mode(m) == MOIU.Automatic
end

for state in (MOIU.NoSolver, MOIU.EmptySolver, MOIU.AttachedSolver)
    @testset "Optimization tests in state $state and mode $mode" for mode in (MOIU.Manual, MOIU.Automatic)
        m = MOIU.InstanceManager(InstanceForManager{Float64}(), mode)
        if state != MOIU.NoSolver
            s = MOIU.MockSolverInstance(InstanceForMock{Float64}())
            MOIU.resetsolver!(m, s)
            if state == MOIU.AttachedSolver
                MOIU.attachsolver!(m)
            end
        end
        @test MOIU.state(m) == state
        @test MOIU.mode(m) == mode
        config = MOIT.TestConfig(solve=false)
        MOIT.contlineartest(m, config)
    end
end
