MOIU.@instance LPInstance () (EqualTo, GreaterThan, LessThan, Interval) (Zeros, Nonnegatives, Nonpositives) () (SingleVariable,) (ScalarAffineFunction,) (VectorOfVariables,) (VectorAffineFunction,)

MOIU.@instance(Instance,
               (), # <- example of giving no set
               (EqualTo, GreaterThan, LessThan, Interval),
               (Reals, Zeros, Nonnegatives, Nonpositives, SecondOrderCone, RotatedSecondOrderCone, ExponentialCone, DualExponentialCone, PositiveSemidefiniteConeTriangle),
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

# Taken from MOI/test/contlinear.jl
@testset "Basic solve, query, resolve" begin
    # simple 2 variable, 1 constraint problem
    # min -x
    # st   x + y <= 1   (x + y - 1 ∈ Nonpositives)
    #       x, y >= 0   (x, y ∈ Nonnegatives)

    instance = Instance{Float64}()

    v = MOI.addvariables!(instance, 2)
    @test MOI.get(instance, MOI.NumberOfVariables()) == 2
    @test MOI.canget(instance, MOI.ListOfVariableIndices())
    vrs = MOI.get(instance, MOI.ListOfVariableIndices())
    @test vrs == v || vrs == reverse(v)

    cf = MOI.ScalarAffineFunction(v, [1.0,1.0], 0.0)
    c = MOI.addconstraint!(instance, cf, MOI.LessThan(1.0))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1

    vc1 = MOI.addconstraint!(instance, MOI.SingleVariable(v[1]), MOI.GreaterThan(0.0))
    vc2 = MOI.addconstraint!(instance, v[2], MOI.GreaterThan(0.0))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    objf = MOI.ScalarAffineFunction(v, [-1.0,0.0], 0.0)
    MOI.set!(instance, MOI.ObjectiveFunction(), objf)
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MinSense)

    @test MOI.get(instance, MOI.ObjectiveSense()) == MOI.MinSense

    @test MOI.canget(instance, MOI.ObjectiveFunction())
    @test objf ≈ MOI.get(instance, MOI.ObjectiveFunction())

    @test MOI.canget(instance, MOI.ConstraintFunction(), c)
    @test cf ≈ MOI.get(instance, MOI.ConstraintFunction(), c)

    @test MOI.canget(instance, MOI.ConstraintSet(), c)
    s = MOI.get(instance, MOI.ConstraintSet(), c)
    @test s == MOI.LessThan(1.0)

    @test MOI.canget(instance, MOI.ConstraintSet(), vc1)
    s = MOI.get(instance, MOI.ConstraintSet(), vc1)
    @test s == MOI.GreaterThan(0.0)

    @test MOI.canget(instance, MOI.ConstraintSet(), vc2)
    s = MOI.get(instance, MOI.ConstraintSet(), vc2)
    @test s == MOI.GreaterThan(0.0)

    # change objective to Max +x

    objf = MOI.ScalarAffineFunction(v, [1.0,0.0], 0.0)
    MOI.set!(instance, MOI.ObjectiveFunction(), objf)
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MaxSense)

    @test MOI.canget(instance, MOI.ObjectiveFunction())
    @test objf ≈ MOI.get(instance, MOI.ObjectiveFunction())

    @test MOI.get(instance, MOI.ObjectiveSense()) == MOI.MaxSense

    # add new variable to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x,y,z >= 0

    z = MOI.addvariable!(instance)
    push!(v, z)
    @test v[3] == z
    @test cf.variables == v
    @test MOI.get(instance, MOI.ConstraintFunction(), c).variables == [v[1], v[2]]
    @test MOI.get(instance, MOI.ObjectiveFunction()).variables == [v[1], v[2]]

    vc3 = MOI.addconstraint!(instance, MOI.SingleVariable(v[3]), MOI.GreaterThan(0.0))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 3

    @test MOI.canmodifyconstraint(instance, c, MOI.ScalarCoefficientChange{Float64}(z, 1.0))
    MOI.modifyconstraint!(instance, c, MOI.ScalarCoefficientChange{Float64}(z, 1.0))

    @test MOI.canmodifyobjective(instance, MOI.ScalarCoefficientChange{Float64}(z, 2.0))
    MOI.modifyobjective!(instance, MOI.ScalarCoefficientChange{Float64}(z, 2.0))

    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 1
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 3

    # setting lb of x to -1 to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x >= -1
    # y,z >= 0

    MOI.modifyconstraint!(instance, vc1, MOI.GreaterThan(-1.0))

    # put lb of x back to 0 and fix z to zero to get :
    # max x + 2z
    # s.t. x + y + z <= 1
    # x, y >= 0, z = 0

    MOI.modifyconstraint!(instance, vc1, MOI.GreaterThan(0.0))
    @test MOI.isvalid(instance, vc3)
    MOI.delete!(instance, vc3)
    @test !MOI.isvalid(instance, vc3)
    vc3 = MOI.addconstraint!(instance, MOI.SingleVariable(v[3]), MOI.EqualTo(0.0))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2

    # modify affine linear constraint set to be == 2 to get :
    # max x + 2z
    # s.t. x + y + z == 2
    # x,y >= 0, z = 0

    @test MOI.isvalid(instance, c)
    MOI.delete!(instance, c)
    @test !MOI.isvalid(instance, c)
    cf = MOI.ScalarAffineFunction(v, [1.0,1.0,1.0], 0.0)
    c = MOI.addconstraint!(instance, cf, MOI.EqualTo(2.0))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 0
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64}}()) == 1

    # modify objective function to x + 2y to get :
    # max x + 2y
    # s.t. x + y + z == 2
    # x,y >= 0, z = 0

    objf = MOI.ScalarAffineFunction(v, [1.0,2.0,0.0], 0.0)
    MOI.set!(instance, MOI.ObjectiveFunction(), objf)
    MOI.set!(instance, MOI.ObjectiveSense(), MOI.MaxSense)

    # add constraint x - y >= 0 to get :
    # max x+2y
    # s.t. x + y + z == 2
    # x - y >= 0
    # x,y >= 0, z = 0

    cf2 = MOI.ScalarAffineFunction(v, [1.0, -1.0, 0.0], 0.0)
    c2 = MOI.addconstraint!(instance, cf2, MOI.GreaterThan(0.0))
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.EqualTo{Float64}}()) == 1
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.GreaterThan{Float64}}()) == 1
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.ScalarAffineFunction{Float64},MOI.LessThan{Float64}}()) == 0
    @test MOI.get(instance, MOI.ConstraintFunction(), c2) ≈ cf2

    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 2
    MOI.delete!(instance, v[1])
    @test MOI.get(instance, MOI.NumberOfConstraints{MOI.SingleVariable,MOI.GreaterThan{Float64}}()) == 1

    f = MOI.get(instance, MOI.ConstraintFunction(), c2)
    @test f.variables == [v[2], z]
    @test f.coefficients == [-1.0, 0.0]

    @test MOI.canget(instance, MOI.ListOfVariableIndices())
    vrs = MOI.get(instance, MOI.ListOfVariableIndices())
    @test vrs == [v[2], z] || vrs == [z, v[2]]
    @test MOI.get(instance, MOI.ObjectiveFunction()) ≈ MOI.ScalarAffineFunction([v[2], z], [2.0, 0.0], 0.0)

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
