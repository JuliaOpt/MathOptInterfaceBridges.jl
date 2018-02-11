@testset "Mock optimizer continuous linear tests" begin
    optimizer = MOIU.MockOptimizer(ModelForMock{Float64}())
    config = MOIT.TestConfig(solve=false)
    MOIT.contlineartest(optimizer, config)
end

@testset "Mock optimizer optimizer attributes" begin
    optimizer = MOIU.MockOptimizer(ModelForMock{Float64}())
    @test MOI.canset(optimizer, MOIU.MockModelAttribute())
    MOI.set!(optimizer, MOIU.MockModelAttribute(), 10)
    @test MOI.canget(optimizer, MOIU.MockModelAttribute())
    @test MOI.get(optimizer, MOIU.MockModelAttribute()) == 10

    v1 = MOI.addvariable!(optimizer)
    @test MOI.canset(optimizer, MOIU.MockVariableAttribute(), typeof(v1))
    MOI.set!(optimizer, MOIU.MockVariableAttribute(), v1, 11)
    @test MOI.canget(optimizer, MOIU.MockVariableAttribute(), typeof(v1))
    @test MOI.get(optimizer, MOIU.MockVariableAttribute(), v1) == 11
    MOI.set!(optimizer, MOIU.MockVariableAttribute(), [v1], [-11])
    @test MOI.get(optimizer, MOIU.MockVariableAttribute(), [v1]) == [-11]

    @test MOI.canaddconstraint(optimizer, MOI.SingleVariable, MOI.GreaterThan{Float64})
    c1 = MOI.addconstraint!(optimizer, MOI.SingleVariable(v1), MOI.GreaterThan(1.0))
    @test MOI.canset(optimizer, MOIU.MockConstraintAttribute(), typeof(c1))
    MOI.set!(optimizer, MOIU.MockConstraintAttribute(), c1, 12)
    @test MOI.canget(optimizer, MOIU.MockConstraintAttribute(), typeof(c1))
    @test MOI.get(optimizer, MOIU.MockConstraintAttribute(), c1) == 12
    MOI.set!(optimizer, MOIU.MockConstraintAttribute(), [c1], [-12])
    @test MOI.get(optimizer, MOIU.MockConstraintAttribute(), [c1]) == [-12]
end

@testset "Mock optimizer optimizer solve no result" begin
    optimizer = MOIU.MockOptimizer(ModelForMock{Float64}())

    v1 = MOI.addvariable!(optimizer)

    # Load fake solution
    MOI.set!(optimizer, MOI.TerminationStatus(), MOI.InfeasibleNoResult)

    # Attributes are hidden until after optimize!()
    @test !MOI.canget(optimizer, MOI.TerminationStatus())
    @test !MOI.canget(optimizer, MOI.ResultCount())
    @test !MOI.canget(optimizer, MOI.VariablePrimal(), MOI.VariableIndex)

    MOI.optimize!(optimizer)
    @test MOI.canget(optimizer, MOI.TerminationStatus())
    @test MOI.canget(optimizer, MOI.ResultCount())
    @test !MOI.canget(optimizer, MOI.VariablePrimal(), typeof(v1))
    @test MOI.get(optimizer, MOI.TerminationStatus()) == MOI.InfeasibleNoResult
    @test MOI.get(optimizer, MOI.ResultCount()) == 0
end

@testset "Mock optimizer optimizer solve with result" begin
    optimizer = MOIU.MockOptimizer(ModelForMock{Float64}())

    v = MOI.addvariables!(optimizer, 2)
    c1 = MOI.addconstraint!(optimizer, MOI.SingleVariable(v[1]), MOI.GreaterThan(1.0))
    soc = MOI.addconstraint!(optimizer, MOI.VectorOfVariables(v), MOI.SecondOrderCone(2))


    # Load fake solution
    # TODO: Provide a more compact API for this.
    MOI.set!(optimizer, MOI.TerminationStatus(), MOI.Success)
    MOI.set!(optimizer, MOI.ObjectiveValue(), 1.0)
    MOI.set!(optimizer, MOI.ResultCount(), 1)
    MOI.set!(optimizer, MOI.PrimalStatus(), MOI.FeasiblePoint)
    MOI.set!(optimizer, MOI.DualStatus(), MOI.FeasiblePoint)
    MOI.set!(optimizer, MOI.VariablePrimal(), v, [1.0, 2.0])
    MOI.set!(optimizer, MOI.VariablePrimal(), v[1], 3.0)
    MOI.set!(optimizer, MOI.ConstraintDual(), c1, 5.9)
    MOI.set!(optimizer, MOI.ConstraintDual(), soc, [1.0,2.0])

    # Attributes are hidden until after optimize!()
    @test !MOI.canget(optimizer, MOI.TerminationStatus())
    @test !MOI.canget(optimizer, MOI.ResultCount())
    @test !MOI.canget(optimizer, MOI.VariablePrimal(), typeof(v[1]))
    @test !MOI.canget(optimizer, MOI.ConstraintDual(), typeof(c1))
    @test !MOI.canget(optimizer, MOI.ConstraintDual(), typeof(soc))

    MOI.optimize!(optimizer)
    @test MOI.canget(optimizer, MOI.TerminationStatus())
    @test MOI.canget(optimizer, MOI.ResultCount())
    @test MOI.canget(optimizer, MOI.ObjectiveValue())
    @test MOI.canget(optimizer, MOI.PrimalStatus())
    @test MOI.canget(optimizer, MOI.DualStatus())
    @test MOI.canget(optimizer, MOI.VariablePrimal(), typeof(v[1]))
    @test MOI.canget(optimizer, MOI.ConstraintDual(), typeof(c1))
    @test MOI.canget(optimizer, MOI.ConstraintDual(), typeof(soc))
    @test MOI.get(optimizer, MOI.TerminationStatus()) == MOI.Success
    @test MOI.get(optimizer, MOI.ResultCount()) == 1
    @test MOI.get(optimizer, MOI.ObjectiveValue()) == 1.0
    @test MOI.get(optimizer, MOI.PrimalStatus()) == MOI.FeasiblePoint
    @test MOI.get(optimizer, MOI.DualStatus()) == MOI.FeasiblePoint
    @test MOI.get(optimizer, MOI.VariablePrimal(), v) == [3.0, 2.0]
    @test MOI.get(optimizer, MOI.VariablePrimal(), v[1]) == 3.0
    @test MOI.get(optimizer, MOI.ConstraintDual(), c1) == 5.9
    @test MOI.get(optimizer, MOI.ConstraintDual(), soc) == [1.0,2.0]
end
