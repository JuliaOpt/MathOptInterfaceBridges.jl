@testset "Mock solver continuous linear tests" begin
    instance = MOIU.MockSolverInstance(InstanceForMock{Float64}())
    config = MOIT.TestConfig(solve=false)
    MOIT.contlineartest(instance, config)
end

@testset "Mock solver instance attributes" begin
    instance = MOIU.MockSolverInstance(InstanceForMock{Float64}())
    @test MOI.canset(instance, MOIU.MockInstanceAttribute())
    MOI.set!(instance, MOIU.MockInstanceAttribute(), 10)
    @test MOI.canget(instance, MOIU.MockInstanceAttribute())
    @test MOI.get(instance, MOIU.MockInstanceAttribute()) == 10

    v1 = MOI.addvariable!(instance)
    @test MOI.canset(instance, MOIU.MockVariableAttribute(), typeof(v1))
    MOI.set!(instance, MOIU.MockVariableAttribute(), v1, 11)
    @test MOI.canget(instance, MOIU.MockVariableAttribute(), typeof(v1))
    @test MOI.get(instance, MOIU.MockVariableAttribute(), v1) == 11
    MOI.set!(instance, MOIU.MockVariableAttribute(), [v1], [-11])
    @test MOI.get(instance, MOIU.MockVariableAttribute(), [v1]) == [-11]

    @test MOI.canaddconstraint(instance, MOI.SingleVariable, MOI.GreaterThan{Float64})
    c1 = MOI.addconstraint!(instance, MOI.SingleVariable(v1), MOI.GreaterThan(1.0))
    @test MOI.canset(instance, MOIU.MockConstraintAttribute(), typeof(c1))
    MOI.set!(instance, MOIU.MockConstraintAttribute(), c1, 12)
    @test MOI.canget(instance, MOIU.MockConstraintAttribute(), typeof(c1))
    @test MOI.get(instance, MOIU.MockConstraintAttribute(), c1) == 12
    MOI.set!(instance, MOIU.MockConstraintAttribute(), [c1], [-12])
    @test MOI.get(instance, MOIU.MockConstraintAttribute(), [c1]) == [-12]
end

@testset "Mock solver instance solve no result" begin
    instance = MOIU.MockSolverInstance(InstanceForMock{Float64}())

    v1 = MOI.addvariable!(instance)

    # Load fake solution
    MOI.set!(instance, MOI.TerminationStatus(), MOI.InfeasibleNoResult)

    # Attributes are hidden until after optimize!()
    @test !MOI.canget(instance, MOI.TerminationStatus())
    @test !MOI.canget(instance, MOI.ResultCount())
    @test !MOI.canget(instance, MOI.VariablePrimal())

    MOI.optimize!(instance)
    @test MOI.canget(instance, MOI.TerminationStatus())
    @test MOI.canget(instance, MOI.ResultCount())
    @test !MOI.canget(instance, MOI.VariablePrimal(), typeof(v1))
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.InfeasibleNoResult
    @test MOI.get(instance, MOI.ResultCount()) == 0
end

@testset "Mock solver instance solve with result" begin
    instance = MOIU.MockSolverInstance(InstanceForMock{Float64}())

    v = MOI.addvariables!(instance, 2)
    c1 = MOI.addconstraint!(instance, MOI.SingleVariable(v[1]), MOI.GreaterThan(1.0))
    soc = MOI.addconstraint!(instance, MOI.VectorOfVariables(v), MOI.SecondOrderCone(2))


    # Load fake solution
    # TODO: Provide a more compact API for this.
    MOI.set!(instance, MOI.TerminationStatus(), MOI.Success)
    MOI.set!(instance, MOI.ObjectiveValue(), 1.0)
    MOI.set!(instance, MOI.ResultCount(), 1)
    MOI.set!(instance, MOI.PrimalStatus(), MOI.FeasiblePoint)
    MOI.set!(instance, MOI.DualStatus(), MOI.FeasiblePoint)
    MOI.set!(instance, MOI.VariablePrimal(), v, [1.0, 2.0])
    MOI.set!(instance, MOI.VariablePrimal(), v[1], 3.0)
    MOI.set!(instance, MOI.ConstraintDual(), c1, 5.9)
    MOI.set!(instance, MOI.ConstraintDual(), soc, [1.0,2.0])

    # Attributes are hidden until after optimize!()
    @test !MOI.canget(instance, MOI.TerminationStatus())
    @test !MOI.canget(instance, MOI.ResultCount())
    @test !MOI.canget(instance, MOI.VariablePrimal(), typeof(v[1]))
    @test !MOI.canget(instance, MOI.ConstraintDual(), typeof(c1))
    @test !MOI.canget(instance, MOI.ConstraintDual(), typeof(soc))

    MOI.optimize!(instance)
    @test MOI.canget(instance, MOI.TerminationStatus())
    @test MOI.canget(instance, MOI.ResultCount())
    @test MOI.canget(instance, MOI.ObjectiveValue())
    @test MOI.canget(instance, MOI.PrimalStatus())
    @test MOI.canget(instance, MOI.DualStatus())
    @test MOI.canget(instance, MOI.VariablePrimal(), typeof(v[1]))
    @test MOI.canget(instance, MOI.ConstraintDual(), typeof(c1))
    @test MOI.canget(instance, MOI.ConstraintDual(), typeof(soc))
    @test MOI.get(instance, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(instance, MOI.ResultCount()) == 1
    @test MOI.get(instance, MOI.ObjectiveValue()) == 1.0
    @test MOI.get(instance, MOI.PrimalStatus()) == MOI.FeasiblePoint
    @test MOI.get(instance, MOI.DualStatus()) == MOI.FeasiblePoint
    @test MOI.get(instance, MOI.VariablePrimal(), v) == [3.0, 2.0]
    @test MOI.get(instance, MOI.VariablePrimal(), v[1]) == 3.0
    @test MOI.get(instance, MOI.ConstraintDual(), c1) == 5.9
    @test MOI.get(instance, MOI.ConstraintDual(), soc) == [1.0,2.0]
end
