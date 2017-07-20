# Taken from MOI/test/contlinear.jl
@testset "Basic solve, query, resolve" begin
    # simple 2 variable, 1 constraint problem
    # min -x
    # st   x + y <= 1   (x + y - 1 ∈ Nonpositives)
    #       x, y >= 0   (x, y ∈ Nonnegatives)

    m = MOIU.Instance{Float64}()

    v = MOI.addvariables!(m, 2)
    @test MOI.getattribute(m, MOI.NumberOfVariables()) == 2

    cf = MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0)
    c = MOI.addconstraint!(m, cf, MOI.LessThan(1.0))
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

    vc1 = MOI.addconstraint!(m, MOI.SingleVariable(v[1]), MOI.GreaterThan(0.0))
    vc2 = MOI.addconstraint!(m, MOI.SingleVariable(v[2]), MOI.GreaterThan(0.0))
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    objf = MOI.ScalarAffineFunction(v, [-1.0,0.0], 0.0)
    MOI.setobjective!(m, MOI.MinSense, objf)

    @test MOI.getattribute(m, MOI.Sense()) == MOI.MinSense

    @test MOI.cangetattribute(m, MOI.ObjectiveFunction())
    @test objf ≈ MOI.getattribute(m, MOI.ObjectiveFunction())

    @test MOI.cangetattribute(m, MOI.ConstraintFunction(), c)
    @test cf ≈ MOI.getattribute(m, MOI.ConstraintFunction(), c)

    @test MOI.cangetattribute(m, MOI.ConstraintSet(), c)
    s = MOI.getattribute(m, MOI.ConstraintSet(), c)
    @test s == MOI.LessThan(1.0)

    @test MOI.cangetattribute(m, MOI.ConstraintSet(), vc1)
    s = MOI.getattribute(m, MOI.ConstraintSet(), vc1)
    @test s == MOI.GreaterThan(0.0)

    @test MOI.cangetattribute(m, MOI.ConstraintSet(), vc2)
    s = MOI.getattribute(m, MOI.ConstraintSet(), vc2)
    @test s == MOI.GreaterThan(0.0)

    # change objective to Max +x

    objf = MOI.ScalarAffineFunction(v, [1.0,0.0], 0.0)
    MOI.setobjective!(m, MOI.MaxSense, objf)

    @test MOI.cangetattribute(m, MOI.ObjectiveFunction())
    @test objf ≈ MOI.getattribute(m, MOI.ObjectiveFunction())

    @test MOI.getattribute(m, MOI.Sense()) == MOI.MaxSense

    # add new variable to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x,y,z >= 0

    z = MOI.addvariable!(m)
    v = [v; z]
    @test v[3] == z

    vc3 = MOI.addconstraint!(m, MOI.SingleVariable(v[3]), MOI.GreaterThan(0.0))
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 3

    MOI.modifyconstraint!(m, c, MOI.ScalarCoefficientChange{Float64}(z, 1.0))
    MOI.modifyobjective!(m, MOI.ScalarCoefficientChange{Float64}(z, 2.0))

    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 3

    # setting lb of x to -1 to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x >= -1
    # y,z >= 0

    MOI.modifyconstraint!(m, vc1, MOI.GreaterThan(-1.0))

    # put lb of x back to 0 and fix z to zero to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x, y >= 0, z = 0

    MOI.modifyconstraint!(m, vc1, MOI.GreaterThan(0.0))
    MOI.delete!(m, vc3)
    vc3 = MOI.addconstraint!(m, MOI.SingleVariable(v[3]), MOI.EqualTo(0.0))
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    # modify affine linear constraint set to be == 2 to get :
    # max x + 2z
    # s.t. x + y + z == 2
    # x,y >= 0, z = 0

    MOI.delete!(m, c)
    cf = MOI.ScalarAffineFunction(v, [1.0,1.0,1.0], 0.0)
    c = MOI.addconstraint!(m, cf, MOI.EqualTo(2.0))
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 0
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64}}()) == 1

    # modify objective function to x + 2y to get :
    # max x + 2y
    # s.t. x + y + z == 2
    # x,y >= 0, z = 0

    objf = MOI.ScalarAffineFunction(v, [1.0,2.0,0.0], 0.0)
    MOI.setobjective!(m, MOI.MaxSense, objf)

    # add constraint x - y >= 0 to get :
    # max x+2y
    # s.t. x + y + z == 2
    # x - y >= 0
    # x,y >= 0, z = 0

    cf2 = MOI.ScalarAffineFunction(v, [1.0, -1.0, 0.0], 0.0)
    c2 = MOI.addconstraint!(m, cf, MOI.GreaterThan(0.0))
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64}}()) == 1
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}}()) == 1
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 0
end
