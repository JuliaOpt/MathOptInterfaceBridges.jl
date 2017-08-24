Base.show(io::IO, v::VR) = print(io, "v$(Int(v.value))")

Base.show(io::IO, f::MOI.SingleVariable) = show(io, f.variable)
Base.show(io::IO, f::MOI.VectorOfVariables) = show(io, f.variables)
function _printsign(io, coef)
    if coef < 0 || coef === -0.0
        print(io, " - ")
    else
        print(io, " + ")
    end
end
function _showaff(io, variables, coefficients, constant, filter)
    first = true
    for (i, (coef, var)) in enumerate(zip(coefficients, variables))
        if filter(i)
            if first
                first = false
            else
                _printsign(io, coef)
                coef = abs(coef)
            end
            print(io, "$coef * $var")
        end
    end
    if first || !iszero(constant)
        if !first
            _printsign(io, constant)
            constant = abs(constant)
        end
        print(io, constant)
    end
end
function Base.show(io::IO, f::MOI.ScalarAffineFunction)
    _showaff(io, f.variables, f.coefficients, f.constant, i -> true)
end
function Base.show{T}(io::IO, f::MOI.VectorAffineFunction{T})
    println(io, "$(length(f.constant))-element VectorAffineFunction{$T}:")
    for (i, constant) in enumerate(f.constant)
        print(io, " ")
        _showaff(io, f.variables, f.coefficients, constant, j -> f.outputindex[j] == i)
        println(io)
    end
end
