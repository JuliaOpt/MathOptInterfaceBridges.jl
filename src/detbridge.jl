function trimap(i::Integer, j::Integer)
    if i < j
        trimap(j, i)
    else
        div((i-1)*i, 2) + j
    end
end

"""
    RootDetBridge{T}

The `RootDetConeTriangle` is representable by a `PositiveSemidefiniteConeTriangle` and an `GeometricMeanCone` constraints; see [1, p. 149].
Indeed, ``t \\le \\det(X)^(1/n)`` if and only if there exists a lower triangular matrix ``Δ`` such that
```math
\\begin{align*}
  \\begin{pmatrix}
    X & Δ\\\\
    Δ^\\top & \\mathrm{Diag}(Δ)
  \\end{pmatrix} & \\succeq 0\\\\
  t & \\le (Δ_{11} Δ_{22} \\cdots Δ_{nn})^{1/n}
\\end{align*}
```

[1] Ben-Tal, Aharon, and Arkadi Nemirovski. *Lectures on modern convex optimization: analysis, algorithms, and engineering applications*. Society for Industrial and Applied Mathematics, 2001.
"""
struct RootDetBridge{T} <: AbstractBridge
    Δ::Vector{VR}
    sdref::CR{MOI.VectorAffineFunction{T}, MOI.PositiveSemidefiniteConeTriangle}
    gmref::CR{MOI.VectorAffineFunction{T}, MOI.GeometricMeanCone}
end
function RootDetBridge{T}(instance, f::MOI.VectorAffineFunction{T}, s::MOI.RootDetConeTriangle) where T
    d = s.dimension
    n = trimap(d, d)
    N = trimap(2d, 2d)

    Δ = MOI.addvariables!(instance, n)

    X = eachscalar(f)[2:(n+1)]
    m = length(X.outputindex)
    M = m + n + d

    outputindex = Vector{Int}(M); outputindex[1:m] = X.outputindex
    variables = Vector{VR}(M); variables[1:m] = X.variables
    coefficients = Vector{T}(M); coefficients[1:m] = X.coefficients
    constant = zeros(T, N); constant[1:n] = X.constant

    cur = m
    for j in 1:d
        for i in j:d
            cur += 1
            outputindex[cur] = trimap(i, d+j)
            variables[cur] = Δ[trimap(i, j)]
            coefficients[cur] = one(T)
        end
        cur += 1
        outputindex[cur] = trimap(d+j, d+j)
        variables[cur] = Δ[trimap(j, j)]
        coefficients[cur] = one(T)
    end
    @assert cur == M
    Y = MOI.VectorAffineFunction(outputindex, variables, coefficients, constant)
    sdref = MOI.addconstraint!(instance, Y, MOI.PositiveSemidefiniteConeTriangle(2d))

    t = eachscalar(f)[1]
    diagidx = trimap.(1:d, 1:d)
    D = MOI.VectorAffineFunction(diagidx, Δ[diagidx], ones(T, d), zeros(T, d))
    gmref = MOI.addconstraint!(instance, moivcat(t, D), MOI.GeometricMeanCone(d+1))

    RootDetBridge(Δ, sdref, gmref)
end

# Attributes, Bridge acting as an instance
MOI.get(b::RootDetBridge, ::MOI.NumberOfVariables) = length(b.Δ)
MOI.get(b::RootDetBridge{T}, ::MOI.NumberOfConstraints{MOI.VectorAffineFunction{T}, MOI.PositiveSemidefiniteConeTriangle}) where T = 1
MOI.get(b::RootDetBridge{T}, ::MOI.NumberOfConstraints{MOI.VectorAffineFunction{T}, MOI.GeometricMeanCone}) where T = 1
MOI.get(b::RootDetBridge{T}, ::MOI.ListOfConstraintReferences{MOI.VectorAffineFunction{T}, MOI.PositiveSemidefiniteConeTriangle}) where T = [b.sdref]
MOI.get(b::RootDetBridge{T}, ::MOI.ListOfConstraintReferences{MOI.VectorAffineFunction{T}, MOI.GeometricMeanCone}) where T = [b.gmref]

# References
function MOI.delete!(instance::MOI.AbstractInstance, c::RootDetBridge)
    MOI.delete!(instance, c.Δ)
    MOI.delete!(instance, c.sdref)
    MOI.delete!(instance, c.gmref)
end

# Attributes, Bridge acting as a constraint reference
MOI.canget(instance::MOI.AbstractInstance, a::MOI.ConstraintPrimal, c::RootDetBridge) = true
function MOI.get(instance::MOI.AbstractInstance, a::MOI.ConstraintPrimal, c::RootDetBridge)
    t = MOI.get(instance, MOI.ConstraintPrimal(), c.gmref)[1]
    x = MOI.get(instance, MOI.ConstraintPrimal(), c.sdref)[1:length(c.Δ)]
    [t; x]
end
MOI.canget(instance::MOI.AbstractInstance, a::MOI.ConstraintDual, c::RootDetBridge) = false

# Constraints
MOI.canmodifyconstraint(instance::MOI.AbstractInstance, c::RootDetBridge, change) = false
