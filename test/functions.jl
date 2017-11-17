@testset "Function tests" begin
    w = MOI.VariableReference(0)
    x = MOI.VariableReference(1)
    y = MOI.VariableReference(2)
    z = MOI.VariableReference(3)
    @testset "Conversion VectorOfVariables -> VectorAffineFunction" begin
        f = MOI.VectorAffineFunction{Int}(MOI.VectorOfVariables([z, x, y]))
        @test f isa MOI.VectorAffineFunction{Int}
        @test f.outputindex == collect(1:3)
        @test f.variables == [z, x, y]
        @test all(f.coefficients .== 1)
        @test all(iszero.(f.constant))
        f = MOI.VectorAffineFunction{Float64}(MOI.VectorOfVariables([x, w]))
        @test f isa MOI.VectorAffineFunction{Float64}
        @test f.outputindex == collect(1:2)
        @test f.variables == [x, w]
        @test all(f.coefficients .== 1)
        @test all(iszero.(f.constant))
    end
    @testset "Iteration and indexing on VectorOfVariables" begin
        f = MOI.VectorOfVariables([z, w, x, y])
        it = MOIU.eachscalar(f)
        @test length(it) == 4
        @test eltype(it) == MOI.SingleVariable
        @test collect(it) == [MOI.SingleVariable(z), MOI.SingleVariable(w), MOI.SingleVariable(x), MOI.SingleVariable(y)]
        @test it[2] == MOI.SingleVariable(w)
        @test it[end] == MOI.SingleVariable(y)
    end
    @testset "Indexing on VectorAffineFunction" begin
        f = MOI.VectorAffineFunction([2, 1, 3, 2, 2, 1, 3, 1, 2],
                                     [x, y, z, z, y, z, x, x, y],
                                     [1, 7, 2, 9, 3, 1, 6, 4, 1],
                                     [2, 7, 5])
        it = MOIU.eachscalar(f)
        @test length(it) == 3
        @test eltype(it) == MOI.ScalarAffineFunction{Int}
        g = it[2]
        @test g isa MOI.ScalarAffineFunction
        @test g.variables    == [x, z, y, y]
        @test g.coefficients == [1, 9, 3, 1]
        @test g.constant == 7
        g = it[1]
        @test g isa MOI.ScalarAffineFunction
        @test g.variables    == [y, z, x]
        @test g.coefficients == [7, 1, 4]
        @test g.constant == 2
        g = it[end]
        @test g isa MOI.ScalarAffineFunction
        @test g.variables    == [z, x]
        @test g.coefficients == [2, 6]
        @test g.constant == 5
        h = it[[3, 1]]
        @test h isa MOI.VectorAffineFunction
        @test h.outputindex  == [1, 1, 2, 2, 2]
        @test h.variables    == [z, x, y, z, x]
        @test h.coefficients == [2, 6, 7, 1, 4]
        @test h.constant == [5, 2]
    end
    @testset "Indexing on VectorQuadraticFunction" begin
        f = MOI.VectorQuadraticFunction([2, 1, 3, 2, 2],
                                        [x, y, z, z, y],
                                        [1, 7, 2, 9, 3],
                                        [2, 3, 1, 2],
                                        [z, x, x, y],
                                        [y, z, z, y],
                                        [1, 6, 4, 3],
                                        [2, 7, 5])
        it = MOIU.eachscalar(f)
        @test length(it) == 3
        @test eltype(it) == MOI.ScalarQuadraticFunction{Int}
        g = it[2]
        @test g isa MOI.ScalarQuadraticFunction
        @test g.affine_variables    == [x, z, y]
        @test g.affine_coefficients == [1, 9, 3]
        @test g.quadratic_rowvariables == [z, y]
        @test g.quadratic_colvariables == [y, y]
        @test g.quadratic_coefficients == [1, 3]
        @test g.constant == 7
        g = it[end]
        @test g isa MOI.ScalarQuadraticFunction
        @test g.affine_variables    == [z]
        @test g.affine_coefficients == [2]
        @test g.quadratic_rowvariables == [x]
        @test g.quadratic_colvariables == [z]
        @test g.quadratic_coefficients == [6]
        @test g.constant == 5
    end
    @testset "Variablewise constraint copy" begin
        f = MOI.SingleVariable(x)
        g = deepcopy(f)
        @test g.variable == x
        v = [x]
        h = deepcopy(MOI.VectorOfVariables([x]))
        push!(v, y)
        @test h.variables == [x]
    end
    @testset "Scalar" begin
        @testset "Affine" begin
            @test MOI.ScalarAffineFunction([x, z], [1, 1], 1) ≈ MOI.ScalarAffineFunction([x, y, z], [1, 1e-7, 1], 1.) atol=1e-6
            @test MOI.ScalarAffineFunction([x, y], [1, 1e-7], 1.) ≈ MOI.ScalarAffineFunction([x], [1], 1) atol=1e-6
            f = MOIU.canonical(MOI.ScalarAffineFunction([y, x, z, x, z], [2, 1, 3, -2, -3], 5))
            @test f.variables == [x, y]
            @test f.coefficients == [-1, 2]
            @test f.constant == 5
            f = MOIU.canonical(MOI.ScalarAffineFunction([w, y, w, x,  x, z,  y,  z,  w, x, y],
                                                        [1, 3, 1, 2, -3, 2, -1, -2, -2, 3, 2], 5))
            @test f.variables == [x, y]
            @test f.coefficients == [2, 4]
            @test f.constant == 5
            f = MOIU.modifyfunction(f, MOI.ScalarConstantChange(6))
            @test f.constant == 6
            g = deepcopy(f)
            @test g ≈ f
            f = MOIU.modifyfunction(f, MOI.ScalarCoefficientChange(y, 3))
            @test !(g ≈ f)
            @test g.coefficients == [2, 4]
            @test f.variables == [x, y]
            @test f.coefficients == [2, 3]
            f = MOIU.modifyfunction(f, MOI.ScalarCoefficientChange(x, 0))
            @test f.variables == [y]
            @test f.coefficients == [3]
        end
        @testset "Quadratic" begin
            f = MOI.ScalarQuadraticFunction([x], [3], [x, y, x], [x, y, y], [1, 2, 3], 7)
            f = MOIU.modifyfunction(f, MOI.ScalarConstantChange(9))
            @test f.constant == 9
            f = MOIU.modifyfunction(f, MOI.ScalarCoefficientChange(y, 0))
            @test f.affine_variables == [x]
            @test f.affine_coefficients == [3]
            g = deepcopy(f)
            @test f ≈ g
            f = MOIU.modifyfunction(f, MOI.ScalarCoefficientChange(y, 2))
            @test !(f ≈ g)
            @test g.affine_variables == [x]
            @test f.affine_variables == [x, y]
            @test f.affine_coefficients == [3, 2]
        end
    end
    @testset "Vector" begin
        @testset "Affine" begin
            f = MOIU.canonical(MOI.VectorAffineFunction([2, 1, 2,  1,  1,  2, 2,  2, 2, 1, 1,  2, 1,  2],
                                                        [x, x, z,  y,  y,  x, y,  z, x, y, y,  x, x,  z],
                                                        [3, 2, 3, -3, -1, -2, 3, -2, 1, 3, 5, -2, 0, -1], [5, 7]))
            @test f.outputindex == [1, 1, 2]
            @test f.variables == [x, y, y]
            @test f.coefficients == [2, 4, 3]
            @test f.constant == [5, 7]
            f = MOIU.modifyfunction(f, MOI.VectorConstantChange([6, 8]))
            @test f.constant == [6, 8]
            g = deepcopy(f)
            @test f ≈ g
            f = MOIU.modifyfunction(f, MOI.MultirowChange(y, [2], [9]))
            @test !(f ≈ g)
            @test f.outputindex == [1, 1, 2]
            @test f.variables == [x, y, y]
            @test g.coefficients == [2, 4, 3]
            @test f.coefficients == [2, 4, 9]
            f = MOIU.modifyfunction(f, MOI.MultirowChange(y, [1], [0]))
            @test f.outputindex == [1, 2]
            @test f.variables == [x, y]
            @test f.coefficients == [2, 9]
        end
        @testset "Quadratic" begin
            f = MOI.VectorQuadraticFunction([1, 2, 2], [x, x, y], [3, 1, 2], [1, 1, 2], [x, y, x], [x, y, y], [1, 2, 3], [7, 3, 4])
            f = MOIU.modifyfunction(f, MOI.VectorConstantChange([10, 11, 12]))
            @test f.constant == [10, 11, 12]
            f = MOIU.modifyfunction(f, MOI.MultirowChange(y, [2, 1], [0, 1]))
            @test f.affine_outputindex == [1, 2, 1]
            @test f.affine_variables == [x, x, y]
            @test f.affine_coefficients == [3, 1, 1]
            g = deepcopy(f)
            f = MOIU.modifyfunction(f, MOI.MultirowChange(x, [1, 3], [0, 4]))
            @test f.affine_outputindex == [2, 1, 3]
            @test g.affine_variables == [x, x, y]
            @test f.affine_variables == [x, y, x]
            @test g.affine_coefficients == [3, 1, 1]
            @test f.affine_coefficients == [1, 1, 4]
        end
    end
end
