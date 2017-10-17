MOIU.@instance LPInstance () (EqualTo, GreaterThan, LessThan, Interval) (Zeros, Nonnegatives, Nonpositives) () (SingleVariable,) (ScalarAffineFunction,) (VectorOfVariables,) (VectorAffineFunction,)

MOIU.@instance(Instance,
               (), # <- example of giving no set
               (EqualTo, GreaterThan, LessThan, Interval),
               (Reals, Zeros, Nonnegatives, Nonpositives, SecondOrderCone, RotatedSecondOrderCone, ExponentialCone, DualExponentialCone, PositiveSemidefiniteConeTriangle, PositiveSemidefiniteConeScaled),
               (PowerCone, DualPowerCone),
               (SingleVariable,), # <- example of giving only one set
               (ScalarAffineFunction, ScalarQuadraticFunction),
               (VectorOfVariables,),
               (VectorAffineFunction, VectorQuadraticFunction))

# Taken from MOI/test/contlinear.jl
@testset "Basic solve, query, resolve" begin
    # simple 2 variable, 1 constraint problem
    # min -x
    # st   x + y <= 1   (x + y - 1 ∈ Nonpositives)
    #       x, y >= 0   (x, y ∈ Nonnegatives)

    m = Instance{Float64}()

    v = MOI.addvariables!(m, 2)
    @test MOI.getattribute(m, MOI.NumberOfVariables()) == 2

    cf = MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0)
    c = MOI.addconstraint!(m, cf, MOI.LessThan(1.0))
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

    vc1 = MOI.addconstraint!(m, MOI.SingleVariable(v[1]), MOI.GreaterThan(0.0))
    vc2 = MOI.addconstraint!(m, MOI.SingleVariable(v[2]), MOI.GreaterThan(0.0))
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    objf = MOI.ScalarAffineFunction(v, [-1.0,0.0], 0.0)
    MOI.setattribute!(m, MOI.ObjectiveFunction(), objf)
    MOI.setattribute!(m, MOI.ObjectiveSense(), MOI.MinSense)

    @test MOI.getattribute(m, MOI.ObjectiveSense()) == MOI.MinSense

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
    MOI.setattribute!(m, MOI.ObjectiveFunction(), objf)
    MOI.setattribute!(m, MOI.ObjectiveSense(), MOI.MaxSense)

    @test MOI.cangetattribute(m, MOI.ObjectiveFunction())
    @test objf ≈ MOI.getattribute(m, MOI.ObjectiveFunction())

    @test MOI.getattribute(m, MOI.ObjectiveSense()) == MOI.MaxSense

    # add new variable to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x,y,z >= 0

    z = MOI.addvariable!(m)
    push!(v, z)
    @test v[3] == z
    @test cf.variables == v
    @test MOI.getattribute(m, MOI.ConstraintFunction(), c).variables == [v[1], v[2]]
    @test MOI.getattribute(m, MOI.ObjectiveFunction()).variables == [v[1], v[2]]

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
    @test MOI.isvalid(m, vc3)
    MOI.delete!(m, vc3)
    @test !MOI.isvalid(m, vc3)
    vc3 = MOI.addconstraint!(m, MOI.SingleVariable(v[3]), MOI.EqualTo(0.0))
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    # modify affine linear constraint set to be == 2 to get :
    # max x + 2z
    # s.t. x + y + z == 2
    # x,y >= 0, z = 0

    @test MOI.isvalid(m, c)
    MOI.delete!(m, c)
    @test !MOI.isvalid(m, c)
    cf = MOI.ScalarAffineFunction(v, [1.0,1.0,1.0], 0.0)
    c = MOI.addconstraint!(m, cf, MOI.EqualTo(2.0))
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 0
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64}}()) == 1

    # modify objective function to x + 2y to get :
    # max x + 2y
    # s.t. x + y + z == 2
    # x,y >= 0, z = 0

    objf = MOI.ScalarAffineFunction(v, [1.0,2.0,0.0], 0.0)
    MOI.setattribute!(m, MOI.ObjectiveFunction(), objf)
    MOI.setattribute!(m, MOI.ObjectiveSense(), MOI.MaxSense)

    # add constraint x - y >= 0 to get :
    # max x+2y
    # s.t. x + y + z == 2
    # x - y >= 0
    # x,y >= 0, z = 0

    cf2 = MOI.ScalarAffineFunction(v, [1.0, -1.0, 0.0], 0.0)
    c2 = MOI.addconstraint!(m, cf2, MOI.GreaterThan(0.0))
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64}}()) == 1
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}}()) == 1
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 0
    @test MOI.getattribute(m, MOI.ConstraintFunction(), c2) ≈ cf2

    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2
    MOI.delete!(m, v[1])
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 1

    f = MOI.getattribute(m, MOI.ConstraintFunction(), c2)
    @test f.variables == [v[2], z]
    @test f.coefficients == [-1.0, 0.0]

    @test MOI.getattribute(m, MOI.ObjectiveFunction()) ≈ MOI.ScalarAffineFunction([v[2], z], [2.0, 0.0], 0.0)

end

@testset "Quadratic functions" begin

    m = Instance{Int}()

    x, y = MOI.addvariables!(m, 2)
    @test MOI.getattribute(m, MOI.NumberOfVariables()) == 2

    f1 = MOI.ScalarQuadraticFunction([x], [3], [x, y, x], [x, y, y], [1, 2, 3], 7)
    c1 = MOI.addconstraint!(m, f1, MOI.Interval(-1, 1))

    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.ScalarQuadraticFunction{Int},MOI.Interval{Int}}()) == 1
    @test MOI.cangetattribute(m, MOI.ListOfConstraintReferences{MOI.ScalarQuadraticFunction{Int},MOI.Interval{Int}}())
    @test (@inferred MOI.getattribute(m, MOI.ListOfConstraintReferences{MOI.ScalarQuadraticFunction{Int},MOI.Interval{Int}}())) == [c1]

    f2 = MOI.VectorQuadraticFunction([1, 2, 2], [x, x, y], [3, 1, 2], [1, 1, 2], [x, y, x], [x, y, y], [1, 2, 3], [7, 3, 4])
    c2 = MOI.addconstraint!(m, f2, MOI.PositiveSemidefiniteConeTriangle(3))

    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle}()) == 1
    @test MOI.cangetattribute(m, MOI.ListOfConstraintReferences{MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle}())
    @test (@inferred MOI.getattribute(m, MOI.ListOfConstraintReferences{MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle}())) == [c2]

    @test MOI.cangetattribute(m, MOI.ListOfConstraints())
    loc = MOI.getattribute(m, MOI.ListOfConstraints())
    @test length(loc) == 2
    @test (MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle) in loc
    @test (MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle) in loc

    f3 = MOIU.modifyfunction(f1, MOI.ScalarConstantChange(9))
    f3 = MOIU.modifyfunction(f3, MOI.ScalarCoefficientChange(y, 2))

    @test !(MOI.getattribute(m, MOI.ConstraintFunction(), c1) ≈ f3)
    MOI.modifyconstraint!(m, c1, f3)
    @test MOI.getattribute(m, MOI.ConstraintFunction(), c1) ≈ f3

    f4 = MOI.VectorAffineFunction([1, 1, 2], [x, y, y], [2, 4, 3], [5, 7])
    c4 = MOI.addconstraint!(m, f4, MOI.SecondOrderCone(2))
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Int},MOI.SecondOrderCone}()) == 1

    f5 = MOI.VectorOfVariables([x])
    c5 = MOI.addconstraint!(m, f5, MOI.RotatedSecondOrderCone(1))
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.RotatedSecondOrderCone}()) == 1

    f6 = MOI.VectorAffineFunction([1, 2], [x, y], [2, 9], [6, 8])
    c6 = MOI.addconstraint!(m, f6, MOI.SecondOrderCone(2))
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Int},MOI.SecondOrderCone}()) == 2

    f7 = MOI.VectorOfVariables([x, y])
    c7 = MOI.addconstraint!(m, f7, MOI.Nonpositives(2))
    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorOfVariables,MOI.Nonpositives}()) == 1

    loc1 = MOI.getattribute(m, MOI.ListOfConstraints())
    loc2 = Vector{Tuple{DataType, DataType}}()
    function _pushloc{F, S}(constrs::Vector{MOIU.C{F, S}})
        if !isempty(constrs)
            push!(loc2, (F, S))
        end
    end
    MOIU.broadcastcall(_pushloc, m)
    for loc in (loc1, loc2)
        @test length(loc) == 5
        @test (MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle) in loc
        @test (MOI.VectorQuadraticFunction{Int},MOI.PositiveSemidefiniteConeTriangle) in loc
        @test (MOI.VectorOfVariables,MOI.RotatedSecondOrderCone) in loc
        @test (MOI.VectorAffineFunction{Int},MOI.SecondOrderCone) in loc
        @test (MOI.VectorOfVariables,MOI.Nonpositives) in loc
    end

    @test MOI.isvalid(m, c4)
    MOI.delete!(m, c4)
    @test !MOI.isvalid(m, c4)

    @test MOI.getattribute(m, MOI.NumberOfConstraints{MOI.VectorAffineFunction{Int},MOI.SecondOrderCone}()) == 1
    @test MOI.getattribute(m, MOI.ConstraintFunction(), c6).constant == f6.constant

    MOI.delete!(m, y)

    f = MOI.getattribute(m, MOI.ConstraintFunction(), c2)
    @test f.affine_outputindex == [1, 2]
    @test f.affine_variables == [x, x]
    @test f.affine_coefficients == [3, 1]
    @test f.quadratic_outputindex == [1]
    @test f.quadratic_rowvariables == [x]
    @test f.quadratic_colvariables == [x]
    @test f.quadratic_coefficients == [1]
    @test f.constant == [7, 3, 4]

    f =  MOI.getattribute(m, MOI.ConstraintFunction(), c6)
    @test f.outputindex == [1]
    @test f.variables == [x]
    @test f.coefficients == [2]
    @test f.constant == [6, 8]

    f =  MOI.getattribute(m, MOI.ConstraintFunction(), c7)
    @test f.variables == [x]

    s =  MOI.getattribute(m, MOI.ConstraintSet(), c7)
    @test MOI.dimension(s) == 1

end
