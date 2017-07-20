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
