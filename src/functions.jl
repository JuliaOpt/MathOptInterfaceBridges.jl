# Define copy of function
Base.deepcopy(f::SVF) = f
Base.deepcopy(f::VVF) = VVF(copy(f.variables))
Base.deepcopy(f::SAF) = SAF(copy(f.variables),
                            copy(f.coefficients),
                            f.constant)
Base.deepcopy(f::VAF) = VAF(copy(f.outputindex),
                            copy(f.variables),
                            copy(f.coefficients),
                            f.constant)
Base.deepcopy(f::SQF) = SQF(copy(f.affine_variables),
                            copy(f.affine_coefficients),
                            copy(f.quadratic_rowvariables),
                            copy(f.quadratic_colvariables),
                            copy(f.quadratic_coefficients),
                            f.constant)
Base.deepcopy(f::VQF) = VQF(copy(f.affine_outputindex),
                            copy(f.affine_variables),
                            copy(f.affine_coefficients),
                            copy(f.quadratic_outputindex),
                            copy(f.quadratic_rowvariables),
                            copy(f.quadratic_colvariables),
                            copy(f.quadratic_coefficients),
                            f.constant)



# Utilities for comparing functions
# Define isapprox so that we can use ≈ in tests

function Base.isapprox(f1::MOI.ScalarAffineFunction{T}, f2::MOI.ScalarAffineFunction{T}; nans = false, kwargs...) where {T}
    function canonicalize(f)
        d = Dict{MOI.VariableReference,T}()
        @assert length(f.variables) == length(f.coefficients)
        for k in 1:length(f.variables)
            d[f.variables[k]] = f.coefficients[k] + get(d, f.variables[k], zero(T))
        end
        return (d,f.constant)
    end
    d1, c1 = canonicalize(f1)
    d2, c2 = canonicalize(f2)
    for (var,coef) in d2
        d1[var] = get(d1,var,zero(T)) - coef
    end
    return isapprox([c2-c1;collect(values(d1))], zeros(T,length(d1)+1); nans=nans, kwargs...)
end

function Base.isapprox(f1::MOI.ScalarQuadraticFunction{T}, f2::MOI.ScalarQuadraticFunction{T}; nans = false, kwargs...) where {T}
    function canonicalize(f)
        affine_d = Dict{MOI.VariableReference,T}()
        @assert length(f.affine_variables) == length(f.affine_coefficients)
        for k in 1:length(f.affine_variables)
            affine_d[f.affine_variables[k]] = f.affine_coefficients[k] + get(affine_d, f.affine_variables[k], zero(T))
        end
        quadratic_d = Dict{Set{MOI.VariableReference},T}()
        @assert length(f.quadratic_rowvariables) == length(f.quadratic_coefficients)
        @assert length(f.quadratic_colvariables) == length(f.quadratic_coefficients)
        for k in 1:length(f.quadratic_rowvariables)
            quadratic_d[Set([f.quadratic_rowvariables[k],f.quadratic_colvariables[k]])] = f.quadratic_coefficients[k] + get(quadratic_d, Set([f.quadratic_rowvariables[k],f.quadratic_colvariables[k]]), zero(T))
        end
        return (quadratic_d,affine_d,f.constant)
    end
    quad_d1, aff_d1, c1 = canonicalize(f1)
    quad_d2, aff_d2, c2 = canonicalize(f2)
    for (var,coef) in aff_d2
        aff_d1[var] = get(aff_d1,var,zero(T)) - coef
    end
    for (vars,coef) in quad_d2
        quad_d1[vars] = get(quad_d1,vars,zero(T)) - coef
    end
    return isapprox([c2-c1;collect(values(aff_d1));collect(values(quad_d1))], zeros(T,length(quad_d1)+length(aff_d1)+1); nans=nans, kwargs...)
end


"""
    modifyfunction(f::AbstractFunction, change::AbstractFunctionModification)

Return a new function `f` modified according to `change`.
"""
function modifyfunction(f::MOI.ScalarAffineFunction, change::MOI.ScalarConstantChange)
    MOI.ScalarAffineFunction(f.variables, f.coefficients, change.new_constant)
end
function modifyfunction(f::MOI.ScalarQuadraticFunction, change::MOI.ScalarConstantChange)
    MOI.ScalarQuadraticFunction(f.affine_variables, f.affine_coefficients,
                         f.quadratic_rowvariables, f.quadratic_colvariables, f.quadratic_coefficients,
                         change.new_constant)
end

function modifyfunction(f::MOI.VectorAffineFunction, change::MOI.VectorConstantChange)
    MOI.VectorAffineFunction(f.outputindex, f.variables, f.coefficients, change.new_constant)
end
function modifyfunction(f::MOI.VectorQuadraticFunction, change::MOI.VectorConstantChange)
    MOI.VectorQuadraticFunction(f.affine_outputindex, f.affine_variables, f.affine_coefficients,
                            f.quadratic_outputindex, f.quadratic_rowvariables, f.quadratic_colvariables, f.quadratic_coefficients,
                            change.new_constant)
end

function _modifycoefficient(variables::Vector{MOI.VariableReference}, coefficients::Vector, variable::MOI.VariableReference, new_coefficient)
    variables = copy(variables)
    coefficients = copy(coefficients)
    i = findfirst(variables, variable)
    if i == 0
        # The variable was not already in the function
        if !iszero(new_coefficient)
            push!(variables, variable)
            push!(coefficients, new_coefficient)
        end
    else
        # The variable was already in the function
        if iszero(new_coefficient)
            deleteat!(variables, i)
            deleteat!(coefficients, i)
        else
            coefficients[i] = new_coefficient
        end
    end
    variables, coefficients
end
function modifyfunction(f::MOI.ScalarAffineFunction, change::MOI.ScalarCoefficientChange)
    MOI.ScalarAffineFunction(_modifycoefficient(f.variables, f.coefficients, change.variable, change.new_coefficient)..., f.constant)
end
function modifyfunction(f::MOI.ScalarQuadraticFunction, change::MOI.ScalarCoefficientChange)
    MOI.ScalarQuadraticFunction(_modifycoefficient(f.affine_variables, f.affine_coefficients, change.variable, change.new_coefficient)...,
                            f.quadratic_rowvariables, f.quadratic_colvariables, f.quadratic_coefficients,
                            f.constant)

end
function _modifycoefficients(n, outputindex, variables::Vector{MOI.VariableReference}, coefficients::Vector, variable::MOI.VariableReference, rows, new_coefficients)
    outputindex = copy(outputindex)
    variables = copy(variables)
    coefficients = copy(coefficients)
    rowmap = zeros(Int, n)
    rowmap[rows] = 1:length(rows)
    del = Int[]
    for i in 1:length(variables)
        if variables[i] == variable
            row = outputindex[i]
            j = rowmap[row]
            if !iszero(j)
                if iszero(new_coefficients[j])
                    push!(del, i)
                else
                    coefficients[i] =  new_coefficients[j]
                end
                rowmap[row] = 0
            end
        end
    end
    deleteat!(outputindex, del)
    deleteat!(variables, del)
    deleteat!(coefficients, del)
    for (row, j) in enumerate(rowmap)
        if !iszero(j)
            push!(outputindex, row)
            push!(variables, variable)
            push!(coefficients, new_coefficients[j])
        end
    end
    outputindex, variables, coefficients
end
function modifyfunction(f::MOI.VectorAffineFunction, change::MOI.MultirowChange)
    MOI.VectorAffineFunction(_modifycoefficients(length(f.constant), f.outputindex, f.variables, f.coefficients, change.variable, change.rows, change.new_coefficients)...,
                         f.constant)
end
function modifyfunction(f::MOI.VectorQuadraticFunction, change::MOI.MultirowChange)
    MOI.VectorQuadraticFunction(_modifycoefficients(length(f.constant), f.affine_outputindex, f.affine_variables, f.affine_coefficients, change.variable, change.rows, change.new_coefficients)...,
                            f.quadratic_outputindex, f.quadratic_rowvariables, f.quadratic_colvariables, f.quadratic_coefficients,
                            f.constant)
end
