using Base.Test

mapvariables(varmap, f::MOI.SingleVariable) = MOI.SingleVariable(varmap[f.variable])
mapvariables(varmap, f::MOI.VectorOfVariables) = MOI.VectorOfVariables(getindex.(varmap, f.variables))
mapvariables(varmap, f::MOI.ScalarAffineFunction) = MOI.ScalarAffineFunction(getindex.(varmap, f.variables), f.coefficients, f.constant)
mapvariables(varmap, f::MOI.VectorAffineFunction) = MOI.VectorAffineFunction(f.outputindex, getindex.(varmap, f.variables), f.coefficients, f.constant)
mapvariables(varmap, f::MOI.ScalarQuadraticFunction) = MOI.ScalarQuadraticFunction(getindex.(varmap, f.affine_variables), f.affine_coefficients, getindex.(varmap, f.quadratic_rowvariables), getindex.(varmap, f.quadratic_colvariables), f.quadratic_coefficients, f.constant)
mapvariables(varmap, f::MOI.VectorQuadraticFunction) = MOI.VectorQuadraticFunction(f.affine_outputindex, getindex.(varmap, f.affine_variables), f.affine_coefficients, f.quadratic_outputindex, getindex.(varmap, f.quadratic_rowvariables), getindex.(varmap, f.quadratic_colvariables), f.quadratic_coefficients, f.constant)

# Cat for MOI sets
affineoutputindex(f::MOI.ScalarAffineFunction) = ones(Int, length(f.variables))
affineoutputindex(f::MOI.VectorAffineFunction) = f.outputindex
moilength(f::MOI.ScalarAffineFunction) = 1
moilength(f::MOI.VectorAffineFunction) = length(f.constant)
constant(f::MOI.ScalarAffineFunction) = [f.constant]
constant(f::MOI.VectorAffineFunction) = f.constant
function moivcat(f::Union{MOI.ScalarAffineFunction, MOI.VectorAffineFunction}...)
    n = length(f)
    offsets = cumsum(collect(moilength.(f)))
    offsets = [0; offsets[1:(n-1)]]
    outputindex = vcat((affineoutputindex.(f) .+ offsets)...)
    variables = vcat((f -> f.variables).(f)...)
    coefficients = vcat((f -> f.coefficients).(f)...)
    cst = vcat(constant.(f)...)
    MOI.VectorAffineFunction(outputindex, variables, coefficients, cst)
end

# Define conversion SingleVariable -> ScalarAffineFunction and VectorOfVariable -> VectorAffineFunction{T}
function MOI.ScalarAffineFunction{T}(f::MOI.SingleVariable) where T
    MOI.ScalarAffineFunction([f.variable], ones(T, 1), zero(T))
end
function MOI.VectorAffineFunction{T}(f::MOI.VectorOfVariables) where T
    n = length(f.variables)
    MOI.VectorAffineFunction(collect(1:n), f.variables, ones(T, n), zeros(T, n))
end

# Implements iterator interface
struct ScalarFunctionIterator{F<:MOI.AbstractVectorFunction}
    f::F
end
eachscalar(f::MOI.AbstractVectorFunction) = ScalarFunctionIterator(f)
Base.start(it::ScalarFunctionIterator) = 1
Base.done(it::ScalarFunctionIterator, state) = state > length(it)
Base.next(it::ScalarFunctionIterator, state) = (it[state], state+1)
Base.length(it::ScalarFunctionIterator{MOI.VectorOfVariables}) = length(it.f.variables)
Base.length(it::ScalarFunctionIterator{<:Union{MOI.VectorAffineFunction, MOI.VectorQuadraticFunction}}) = length(it.f.constant)
Base.eltype(it::ScalarFunctionIterator{MOI.VectorOfVariables}) = MOI.SingleVariable
Base.eltype(it::ScalarFunctionIterator{MOI.VectorAffineFunction{T}}) where T = MOI.ScalarAffineFunction{T}
Base.eltype(it::ScalarFunctionIterator{MOI.VectorQuadraticFunction{T}}) where T = MOI.ScalarQuadraticFunction{T}
Base.endof(it::ScalarFunctionIterator) = length(it)

# Define getindex for Vector functions

Base.getindex(it::ScalarFunctionIterator{MOI.VectorOfVariables}, i::Integer) = MOI.SingleVariable(it.f.variables[i])
function Base.getindex(it::ScalarFunctionIterator{<:MOI.VectorAffineFunction}, i::Integer)
    f = it.f
    I = find(oi -> oi == i, f.outputindex)
    MOI.ScalarAffineFunction(f.variables[I], f.coefficients[I], f.constant[i])
end
function Base.getindex(it::ScalarFunctionIterator{<:MOI.VectorQuadraticFunction}, i::Integer)
    f = it.f
    aI = find(oi -> oi == i, f.affine_outputindex)
    qI = find(oi -> oi == i, f.quadratic_outputindex)
    MOI.ScalarQuadraticFunction(f.affine_variables[aI], f.affine_coefficients[aI],
                                f.quadratic_rowvariables[qI], f.quadratic_colvariables[qI], f.quadratic_coefficients[qI],
                                f.constant[i])
end

function Base.getindex(it::ScalarFunctionIterator{MOI.VectorAffineFunction{T}}, I::AbstractVector) where T
    outputindex = Int[]
    variables = VI[]
    coefficients = T[]
    constant = Vector{T}(length(I))
    for (i, j) in enumerate(I)
        g = it[j]
        append!(outputindex, repmat(i:i, length(g.variables)))
        append!(variables, g.variables)
        append!(coefficients, g.coefficients)
        constant[i] = g.constant
    end
    MOI.VectorAffineFunction(outputindex, variables, coefficients, constant)
end

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

# Utilities for getting a canonical representation of a function
Base.isless(v1::VI, v2::VI) = isless(v1.value, v2.value)
"""
    canonical(f::AbstractFunction)

Returns the funcion in a canonical form, i.e.
* A term appear only once.
* The coefficients are nonzero.
* The terms appear in increasing order of variable where there the order of the variables is the order of their value.
* For a `AbstractVectorFunction`, the terms are sorted in ascending order of output index.

### Examples
If `x` (resp. `y`, `z`) is `VariableIndex(1)` (resp. 2, 3).
The canonical representation of `ScalarAffineFunction([y, x, z, x, z], [2, 1, 3, -2, -3], 5)` is `ScalarAffineFunction([x, y], [-1, 2], 5)`.

"""
function canonical{T}(f::SAF{T})
    σ = sortperm(f.variables)
    outputindex = Int[]
    variables = VI[]
    coefficients = T[]
    prev = 0
    for i in σ
        if !isempty(variables) && f.variables[i] == last(variables)
            coefficients[end] += f.coefficients[i]
        elseif !iszero(f.coefficients[i])
            if !isempty(variables) && iszero(last(coefficients))
                variables[end] = f.variables[i]
                coefficients[end] = f.coefficients[i]
            else
                push!(variables, f.variables[i])
                push!(coefficients, f.coefficients[i])
            end
        end
    end
    if !isempty(variables) && iszero(last(coefficients))
        pop!(variables)
        pop!(coefficients)
    end
    SAF{T}(variables, coefficients, f.constant)
end
function canonical{T}(f::VAF{T})
    σ = sortperm(1:length(f.variables), by = i -> (f.outputindex[i], f.variables[i]))
    outputindex = Int[]
    variables = VI[]
    coefficients = T[]
    prev = 0
    for i in σ
        if !isempty(variables) && f.outputindex[i] == last(outputindex) && f.variables[i] == last(variables)
            coefficients[end] += f.coefficients[i]
        elseif !iszero(f.coefficients[i])
            if !isempty(variables) && iszero(last(coefficients))
                outputindex[end] = f.outputindex[i]
                variables[end] = f.variables[i]
                coefficients[end] = f.coefficients[i]
            else
                push!(outputindex, f.outputindex[i])
                push!(variables, f.variables[i])
                push!(coefficients, f.coefficients[i])
            end
        end
    end
    if !isempty(variables) && iszero(last(coefficients))
        pop!(outputindex)
        pop!(variables)
        pop!(coefficients)
    end
    VAF{T}(outputindex, variables, coefficients, f.constant)
end

# Utilities for comparing functions
# Define isapprox so that we can use ≈ in tests

function _isapprox(vars1, coeffs1, vars2, coeffs2; kwargs...)
    m = length(vars1)
    n = length(vars2)
    i = 1
    j = 1
    while i <= m || j <= n
        if i <= m && j <= n && vars1[i] == vars2[j]
            isapprox(coeffs1[i], coeffs2[j]; kwargs...) || return false
            i += 1
            j += 1
        elseif j > n || (i <= m && vars1[i] < vars2[j])
            isapprox(coeffs1[i], 0.0; kwargs...) || return false
            i += 1
        else
            isapprox(0.0, coeffs2[j]; kwargs...) || return false
            j += 1
        end
    end
    return true
end

Base.isapprox(f1::MOI.SingleVariable, f2::MOI.SingleVariable; kwargs...) = f1.variable == f2.variable
Base.isapprox(f1::MOI.VectorOfVariables, f2::MOI.VectorOfVariables; kwargs...) = f1.variables == f2.variables

function Base.isapprox(f1::MOI.VectorAffineFunction, f2::MOI.VectorAffineFunction; kwargs...)
    f1 = canonical(f1)
    f2 = canonical(f2)
    _isapprox(collect(zip(f1.outputindex, f1.variables)), f1.coefficients, collect(zip(f2.outputindex, f2.variables)), f2.coefficients; kwargs...)
end

function Base.isapprox(f1::MOI.ScalarAffineFunction, f2::MOI.ScalarAffineFunction; kwargs...)
    f1 = canonical(f1)
    f2 = canonical(f2)
    _isapprox(f1.variables, f1.coefficients, f2.variables, f2.coefficients; kwargs...)
end

function Base.isapprox(f1::MOI.ScalarAffineFunction{T}, f2::MOI.ScalarAffineFunction{T}; kwargs...) where {T}
    function canonicalize(f)
        d = Dict{MOI.VariableIndex,T}()
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
    return isapprox([c2-c1;collect(values(d1))], zeros(T,length(d1)+1); kwargs...)
end

function Base.isapprox(f1::MOI.ScalarQuadraticFunction{T}, f2::MOI.ScalarQuadraticFunction{T}; kwargs...) where {T}
    function canonicalize(f)
        affine_d = Dict{MOI.VariableIndex,T}()
        @assert length(f.affine_variables) == length(f.affine_coefficients)
        for k in 1:length(f.affine_variables)
            affine_d[f.affine_variables[k]] = f.affine_coefficients[k] + get(affine_d, f.affine_variables[k], zero(T))
        end
        quadratic_d = Dict{Set{MOI.VariableIndex},T}()
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
    return isapprox([c2-c1;collect(values(aff_d1));collect(values(quad_d1))], zeros(T,length(quad_d1)+length(aff_d1)+1); kwargs...)
end

function test_variablenames_equal(instance, variablenames)
    seen_name = Dict(name => false for name in variablenames)
    for index in MOI.get(instance, MOI.ListOfVariableIndices())
        vname = MOI.get(instance, MOI.VariableName(), index)
        if !haskey(seen_name, vname)
            error("Variable with name $vname present in instance but not expected list of variable names.")
        end
        if seen_name[vname]
            error("Variable with name $vname present twice in instance (shouldn't happen!)")
        end
        seen_name[vname] = true
    end
    for (vname,seen) in seen_name
        if !seen
            error("Did not find variable with name $vname in intance.")
        end
    end
end
function test_constraintnames_equal(instance, constraintnames)
    seen_name = Dict(name => false for name in constraintnames)
    for (F,S) in MOI.get(instance, MOI.ListOfConstraints())
        for index in MOI.get(instance, MOI.ListOfConstraintIndices{F,S}())
            cname = MOI.get(instance, MOI.ConstraintName(), index)
            if !haskey(seen_name, cname)
                error("Constraint with name $cname present in instance but not expected list of constraint names.")
            end
            if seen_name[cname]
                error("Constraint with name $cname present twice in instance (shouldn't happen!)")
            end
            seen_name[cname] = true
        end
    end
    for (cname,seen) in seen_name
        if !seen
            error("Did not find constraint with name $cname in intance.")
        end
    end
end

map_variables(f::Vector{MOI.VariableIndex}, variablemap::Dict{MOI.VariableIndex,MOI.VariableIndex}) = map(v -> variablemap[v], f)
map_variables(f, variablemap) = f


for moiname in [MOI.ScalarAffineFunction,MOI.VectorAffineFunction,
                 MOI.ScalarQuadraticFunction,MOI.VectorQuadraticFunction,
                 MOI.SingleVariable,MOI.VectorOfVariables]
    fields = fieldnames(moiname)
    constructor = Expr(:call, moiname, [Expr(:call,:map_variables,Expr(:.,:f,Base.Meta.quot(field)),:variablemap) for field in fields]...)
    @eval map_variables(f::$moiname, variablemap::Dict{MOI.VariableIndex,MOI.VariableIndex}) = $constructor
end

"""
    test_instances_equal(instance1::AbstractInstance, instance2::AbstractInstance, variablenames::Vector{String}, constraintnames::Vector{String})

Test that `instance1` and `instance2` are identical using `variablenames` as as keys for the variable names and `constraintnames` as keys for the constraint names. Uses `Base.Test` macros.
"""
function test_instances_equal(instance1::MOI.AbstractInstance, instance2::MOI.AbstractInstance, variablenames::Vector{String}, constraintnames::Vector{String})
    # TODO: give test-friendly feedback instead of errors?
    test_variablenames_equal(instance1, variablenames)
    test_variablenames_equal(instance2, variablenames)
    test_constraintnames_equal(instance1, constraintnames)
    test_constraintnames_equal(instance2, constraintnames)

    variablemap_2to1 = Dict{MOI.VariableIndex,MOI.VariableIndex}()
    for vname in variablenames
        index1 = MOI.get(instance1, MOI.VariableIndex, vname)
        index2 = MOI.get(instance2, MOI.VariableIndex, vname)
        variablemap_2to1[index2] = index1
    end

    for cname in constraintnames
        index1 = MOI.get(instance1, MOI.ConstraintIndex, cname)
        index2 = MOI.get(instance2, MOI.ConstraintIndex, cname)
        f1 = MOI.get(instance1, MOI.ConstraintFunction(), index1)
        f2 = MOI.get(instance2, MOI.ConstraintFunction(), index2)
        s1 = MOI.get(instance1, MOI.ConstraintSet(), index1)
        s2 = MOI.get(instance2, MOI.ConstraintSet(), index2)
        @test isapprox(f1, map_variables(f2, variablemap_2to1))
        @test s1 == s2
    end

    for src in (instance1, instance2)
        for attr in MOI.get(src, MOI.ListOfInstanceAttributesSet())
            @test MOI.canget(instance1, attr)
            value1 = MOI.get(instance1, attr)
            @test MOI.canget(instance2, attr)
            value2 = MOI.get(instance2, attr)
            if value1 isa MOI.AbstractFunction
                @test value2 isa MOI.AbstractFunction
                @test isapprox(value1, attribute_value_map(variablemap_2to1, value2))
            else
                @test !(value2 isa MOI.AbstractFunction)
                @test value1 == value2
            end
        end
    end
end


function _rmvar(vis::Vector{MOI.VariableIndex}, vi::MOI.VariableIndex)
    find(v -> v != vi, vis)
end
function _rmvar(vis1::Vector{MOI.VariableIndex}, vis2::Vector{MOI.VariableIndex}, vi::MOI.VariableIndex)
    @assert eachindex(vis1) == eachindex(vis2)
    find(i -> vis1[i] != vi && vis2[i] != vi, eachindex(vis1))
end

"""
    removevariable(f::AbstractFunction, vi::VariableIndex)

Return a new function `f` with the variable vi removed.
"""
function removevariable(f::MOI.VectorOfVariables, vi)
    MOI.VectorOfVariables(f.variables[_rmvar(f.variables, vi)])
end
function removevariable(f::MOI.ScalarAffineFunction, vi)
    I = _rmvar(f.variables, vi)
    MOI.ScalarAffineFunction(f.variables[I], f.coefficients[I], f.constant)
end
function removevariable(f::MOI.ScalarQuadraticFunction, vi)
    I = _rmvar(f.affine_variables, vi)
    J = _rmvar(f.quadratic_rowvariables, f.quadratic_colvariables, vi)
    MOI.ScalarQuadraticFunction(f.affine_variables[I], f.affine_coefficients[I],
                                f.quadratic_rowvariables[J], f.quadratic_colvariables[J], f.quadratic_coefficients[J],
                                f.constant)
end
function removevariable(f::MOI.VectorAffineFunction, vi)
    I = _rmvar(f.variables, vi)
    MOI.VectorAffineFunction(f.outputindex[I], f.variables[I], f.coefficients[I], f.constant)
end
function removevariable(f::MOI.VectorQuadraticFunction, vi)
    I = _rmvar(f.affine_variables, vi)
    J = _rmvar(f.quadratic_rowvariables, f.quadratic_colvariables, vi)
    MOI.VectorQuadraticFunction(f.affine_outputindex[I], f.affine_variables[I], f.affine_coefficients[I],
                                f.quadratic_outputindex[J], f.quadratic_rowvariables[J], f.quadratic_colvariables[J], f.quadratic_coefficients[J],
                                f.constant)
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

function _modifycoefficient(variables::Vector{MOI.VariableIndex}, coefficients::Vector, variable::MOI.VariableIndex, new_coefficient)
    variables = copy(variables)
    coefficients = copy(coefficients)
    i = findfirst(variables, variable)
    if iszero(i) || i === nothing # returns 0 in Julia v0.6 and nothing in Julia v0.7
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
function _modifycoefficients(n, outputindex, variables::Vector{MOI.VariableIndex}, coefficients::Vector, variable::MOI.VariableIndex, rows, new_coefficients)
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
