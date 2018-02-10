struct IndexMap
    varmap::Dict{MOI.VariableIndex, MOI.VariableIndex}
    conmap::Dict{MOI.ConstraintIndex, MOI.ConstraintIndex}
end
IndexMap() = IndexMap(Dict{MOI.VariableIndex, MOI.VariableIndex}(),
                 Dict{MOI.ConstraintIndex, MOI.ConstraintIndex}())
Base.getindex(idxmap::IndexMap, vi::MOI.VariableIndex) = idxmap.varmap[vi]
function Base.getindex(idxmap::IndexMap, ci::MOI.ConstraintIndex{F, S}) where {F, S}
    idxmap.conmap[ci]::MOI.ConstraintIndex{F, S}
end

Base.setindex!(idxmap::IndexMap, vi1::MOI.VariableIndex, vi2::MOI.VariableIndex) = Base.setindex!(idxmap.varmap, vi1, vi2)
function Base.setindex!(idxmap::IndexMap, ci1::MOI.ConstraintIndex{F, S}, ci2::MOI.ConstraintIndex{F, S}) where {F, S}
    Base.setindex!(idxmap.conmap, ci1, ci2)
end

Base.delete!(idxmap::IndexMap, vi::MOI.VariableIndex) = delete!(idxmap.varmap, vi)
Base.delete!(idxmap::IndexMap, ci::MOI.ConstraintIndex) = delete!(idxmap.conmap, ci)

Base.keys(idxmap::IndexMap) = Iterators.flatten((keys(idxmap.varmap), keys(idxmap.conmap)))

"""
    copyattributes!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance, idxmap::IndexMap, cancopyattr::Function=MOI.canset, copyattr!::Function=MOI.set!)

Copy the instance attributes from the instance `src` the instance `dest` using `cancopyattr` to check if the attribute can be copied and `copyattr!` to copy the attribute.

    copyattributes!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance, idxmap::IndexMap, vis_src::Vector{MOI.VariableIndex}, cancopyattr::Function=MOI.canset, copyattr!::Function=MOI.set!)

Copy the variable attributes from the instance `src` the instance `dest` using `cancopyattr` to check if the attribute can be copied and `copyattr!` to copy the attribute.

    copyattributes!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance, idxmap::IndexMap, cis_src::Vector{MOI.ConstraintIndex{F, S}}, cancopyattr::Function=MOI.canset, copyattr!::Function=MOI.set!) where {F, S}

Copy the constraint attributes of `F`-in-`S` constraints from the instance `src` the instance `dest` using `cancopyattr` to check if the attribute can be copied and `copyattr!` to copy the attribute.
"""
function copyattributes! end

function copyattributes!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance, idxmap::IndexMap, cancopyattr::Function=MOI.canset, copyattr!::Function=MOI.set!)
    # Copy instance attributes
    attrs = MOI.get(src, MOI.ListOfInstanceAttributesSet())
    _copyattributes!(dest, src, idxmap, attrs, tuple(), tuple(), tuple(), cancopyattr, copyattr!)
end
function copyattributes!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance, idxmap::IndexMap, vis_src::Vector{VI}, cancopyattr::Function=MOI.canset, copyattr!::Function=MOI.set!)
    # Copy variable attributes
    attrs = MOI.get(src, MOI.ListOfVariableAttributesSet())
    vis_dest = map(vi -> idxmap[vi], vis_src)
    _copyattributes!(dest, src, idxmap, attrs, (VI,), (vis_src,), (vis_dest,), cancopyattr, copyattr!)
end
function copyattributes!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance, idxmap::IndexMap, cis_src::Vector{CI{F, S}}, cancopyattr::Function=MOI.canset, copyattr!::Function=MOI.set!) where {F, S}
    # Copy constraint attributes
    attrs = MOI.get(src, MOI.ListOfConstraintAttributesSet{F, S}())
    cis_dest = map(ci -> idxmap[ci], cis_src)
    _copyattributes!(dest, src, idxmap, attrs, (CI{F, S},), (cis_src,), (cis_dest,), cancopyattr, copyattr!)
end

function _copyattributes!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance, idxmap::IndexMap, attrs, canargs, getargs, setargs, cancopyattr::Function=MOI.canset, copyattr!::Function=MOI.set!)
    for attr in attrs
        if MOI.canget(src, attr, canargs...)
            if !cancopyattr(dest, attr, canargs...)
                return MOI.CopyResult(MOI.CopyUnsupportedAttribute, "Unsupported attribute $attr", idxmap)
            end
            copyattr!(dest, attr, setargs..., attribute_value_map(idxmap, MOI.get(src, attr, getargs...)))
        end
    end
    return MOI.CopyResult(MOI.CopySuccess, "", idxmap)
end

"""
    copyconstraints!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance, idxmap::IndexMap, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}

Copy the constraints of type `F`-in-`S` from the instance `src` the instance `dest` and fill `idxmap` accordingly.
"""
function copyconstraints!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance, idxmap::IndexMap, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    # Copy constraints
    cis_src = MOI.get(src, MOI.ListOfConstraintIndices{F, S}())
    for ci_src in cis_src
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        f_dest = mapvariables(idxmap, f_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        if MOI.canaddconstraint(dest, typeof(f_dest), typeof(s))
            ci_dest = MOI.addconstraint!(dest, f_dest, s)
            idxmap.conmap[ci_src] = ci_dest
        else
            return MOI.CopyResult(MOI.CopyUnsupportedConstraint, "Unsupported $F-in-$S constraint", idxmap)
        end
    end

    return copyattributes!(dest, src, idxmap, cis_src)
end

attribute_value_map(idxmap, f::MOI.AbstractFunction) = mapvariables(idxmap, f)
attribute_value_map(idxmap, attribute_value) = attribute_value
function defaultcopy!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance)
    MOI.empty!(dest)

    idxmap = IndexMap()

    # Copy variables
    vis_src = MOI.get(src, MOI.ListOfVariableIndices())
    if !MOI.canaddvariable(dest)
        return MOI.CopyResult(MOI.CopyOtherError, "Adding variables is not supported", idxmap)
    end
    for vi in vis_src
        idxmap.varmap[vi] = MOI.addvariable!(dest)
    end

    # Copy variable attributes
    res = copyattributes!(dest, src, idxmap, vis_src)
    res.status == MOI.CopySuccess || return res

    # Copy instance attributes
    res = copyattributes!(dest, src, idxmap)
    res.status == MOI.CopySuccess || return res

    # Copy constraints
    for (F, S) in MOI.get(src, MOI.ListOfConstraints())
        # do the rest in copyconstraints! which is type stable
        res = copyconstraints!(dest, src, idxmap, F, S)
        res.status == MOI.CopySuccess || return res
    end

    return MOI.CopyResult(MOI.CopySuccess, "", idxmap)
end

# Allocate-Load Interface: 2-pass copy of a MathOptInterface instance
# Some solver wrappers (e.g. SCS, ECOS, SDOI) do not supporting copying an optimization instance using `MOI.addconstraints!`, `MOI.addvariables` and `MOI.set!`
# as they first need to figure out some information about a model before being able to pass the problem data to the solver.
#
# During the first pass (called allocate) : the instance collects the relevant information about the problem so that
# on the second pass (called load), the constraints can be loaded directly to the solver (in case of SDOI) or written directly into the matrix of constraints (in case of SCS and ECOS).

# To support `MOI.copy!` using this 2-pass mechanism, implement the allocate-load interface defined below and do:
# MOI.copy!(dest::InstanceType, src::MOI.AbstractInstance) = MOIU.allocateload!(dest, src)
# In the implementation of the allocate-load interface, it can be assumed that the different functions will the called in the following order:
# 1) `allocatevariables!`
# 2) `allocate!` and `allocateconstraint!`
# 3) `loadvariables!` and `allocateconstraint!`
# 4) `load!` and `loadconstraint!`
# The interface is not meant to be used to create new constraints with `allocateconstraint!` followed by `loadconstraint!` after a solve, it is only meant for being used in this order to implement `MOI.copy!`.

"""
    needsallocateload(instance::MOI.AbstractInstance)::Bool

Return a `Bool` indicating whether `instance` does not support `addvariables!`/`addconstraint!`/`set!` but supports `allocatevariables!`/`allocateconstraint!`/`allocate!`/`loadvariables!`/`loadconstraint!`/`load!`.
That is, the allocate-load interface need to be used to copy an instance to `instance`.
"""
function needsallocateload end
needsallocateload(::MOI.AbstractInstance) = false

"""
    allocatevariables!(instance::MOI.AbstractInstance, nvars::Integer)

Creates `nvars` variables and returns a vector of `nvars` variable indices.
"""
function allocatevariables! end

"""
    allocate!(instance::AbstractInstance, attr::AbstractInstanceAttribute, value)
    allocate!(instance::AbstractInstance, attr::AbstractVariableAttribute, v::VariableIndex, value)
    allocate!(instance::AbstractInstance, attr::AbstractConstraintAttribute, c::ConstraintIndex, value)

Informs `instance` that `load!` will be called with the same arguments after `loadvariables!` is called.
"""
function allocate! end

"""
    canallocate(instance::AbstractInstance, attr::AbstractInstanceAttribute)::Bool
    canallocate(instance::AbstractInstance, attr::AbstractVariableAttribute, R::Type{VariableIndex})::Bool
    canallocate(instance::AbstractInstance, attr::AbstractConstraintAttribute, R::Type{ConstraintIndex{F,S})::Bool

Return a `Bool` indicating whether it is possible to allocate attribute `attr` applied to the index type `R` in the instance `instance`.
"""
function canallocate end
canallocate(::MOI.AbstractInstance, ::MOI.AnyAttribute) = false
canallocate(::MOI.AbstractInstance, ::MOI.AnyAttribute, ::Type{<:MOI.Index}) = false

"""
    allocateconstraint!(instance::MOI.AbstractInstance, f::MOI.AbstractFunction, s::MOI.AbstractSet)

Returns the index for the constraint to be used in `loadconstraint!` that will be called after `loadvariables!` is called.
"""
function allocateconstraint! end

"""
    canallocateconstraint(instance::AbstractInstance, F::Type{<:AbstractFunction}, S::Type{<:AbstractSet})::Bool

Return a `Bool` indicating whether it is possible to allocate a constraint ``f(x) \\in \\mathcal{S}`` where ``f`` is of type `F`, and ``\\mathcal{S}`` is of type `S`.
"""
canallocateconstraint(instance::MOI.AbstractInstance, ::Type{<:MOI.AbstractFunction}, ::Type{<:MOI.AbstractSet}) = false

"""
    loadvariables!(instance::MOI.AbstractInstance, nvars::Integer)

Prepares the `instance` for `loadobjective!` and `loadconstraint!`.
"""
function loadvariables! end

"""
    load!(instance::AbstractInstance, attr::AbstractInstanceAttribute, value)
    load!(instance::AbstractInstance, attr::AbstractVariableAttribute, v::VariableIndex, value)
    load!(instance::AbstractInstance, attr::AbstractConstraintAttribute, c::ConstraintIndex, value)

This has the same effect that `set!` with the same arguments except that `allocate!` should be called first before `loadvariables!`.
"""
function load! end

"""
    canload(instance::AbstractInstance, attr::AbstractInstanceAttribute)::Bool
    canload(instance::AbstractInstance, attr::AbstractVariableAttribute, R::Type{VariableIndex})::Bool
    canload(instance::AbstractInstance, attr::AbstractConstraintAttribute, R::Type{ConstraintIndex{F,S})::Bool

Return a `Bool` indicating whether it is possible to load attribute `attr` applied to the index type `R` in the instance `instance`.
"""
function canload end
canload(::MOI.AbstractInstance, ::MOI.AnyAttribute) = false
canload(::MOI.AbstractInstance, ::MOI.AnyAttribute, ::Type{<:MOI.Index}) = false

"""
    loadconstraint!(instance::MOI.AbstractInstance, ci::MOI.ConstraintIndex, f::MOI.AbstractFunction, s::MOI.AbstractSet)

Sets the constraint function and set for the constraint of index `ci`.
"""
function loadconstraint! end

"""
    canloadconstraint(instance::AbstractInstance, F::Type{<:AbstractFunction}, S::Type{<:AbstractSet})::Bool

Return a `Bool` indicating whether it is possible to load a constraint ``f(x) \\in \\mathcal{S}`` where ``f`` is of type `F`, and ``\\mathcal{S}`` is of type `S`.
"""
canloadconstraint(instance::MOI.AbstractInstance, ::Type{<:MOI.AbstractFunction}, ::Type{<:MOI.AbstractSet}) = false

function allocateconstraints!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance, idxmap::IndexMap, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    # Allocate constraints
    if !canallocateconstraint(dest, F, S)
        return MOI.CopyResult(MOI.CopyUnsupportedConstraint, "Unsupported $F-in-$S constraint", idxmap)
    end
    cis_src = MOI.get(src, MOI.ListOfConstraintIndices{F, S}())
    for ci_src in cis_src
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        f_dest = mapvariables(idxmap, f_src)
        ci_dest = allocateconstraint!(dest, f_dest, s)
        idxmap.conmap[ci_src] = ci_dest
    end

    return copyattributes!(dest, src, idxmap, cis_src, canallocate, allocate!)
end

function loadconstraints!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance, idxmap::IndexMap, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    # Load constraints
    if !canloadconstraint(dest, F, S)
        return MOI.CopyResult(MOI.CopyUnsupportedConstraint, "Unsupported $F-in-$S constraint", idxmap)
    end
    cis_src = MOI.get(src, MOI.ListOfConstraintIndices{F, S}())
    for ci_src in cis_src
        ci_dest = idxmap[ci_src]
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        f_dest = mapvariables(idxmap, f_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        loadconstraint!(dest, ci_dest, f_dest, s)
    end

    return copyattributes!(dest, src, idxmap, cis_src, canload, load!)
end

"""
    allocateload!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance)

Implements `MOI.copy!(dest, src)` using the allocate-load interface.
"""
function allocateload!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance)
    MOI.empty!(dest)

    idxmap = IndexMap()

    # Allocate variables
    nvars = MOI.get(src, MOI.NumberOfVariables())
    vis_src = MOI.get(src, MOI.ListOfVariableIndices())
    vis_dest = allocatevariables!(dest, nvars)
    for (var_src, var_dest) in zip(vis_src, vis_dest)
        idxmap.varmap[var_src] = var_dest
    end

    # Allocate variable attributes
    res = copyattributes!(dest, src, idxmap, vis_src, canallocate, allocate!)
    res.status == MOI.CopySuccess || return res

    # Allocate instance attributes
    res = copyattributes!(dest, src, idxmap, canallocate, allocate!)
    res.status == MOI.CopySuccess || return res

    # Allocate constraints
    for (F, S) in MOI.get(src, MOI.ListOfConstraints())
        # do the rest in copyconstraints! which is type stable
        res = allocateconstraints!(dest, src, idxmap, F, S)
        res.status == MOI.CopySuccess || return res
    end

    # Load variables
    loadvariables!(dest, nvars)

    # Load variable attributes
    res = copyattributes!(dest, src, idxmap, vis_src, canload, load!)
    res.status == MOI.CopySuccess || return res

    # Load instance attributes
    res = copyattributes!(dest, src, idxmap, canload, load!)
    res.status == MOI.CopySuccess || return res

    # Copy constraints
    for (F, S) in MOI.get(src, MOI.ListOfConstraints())
        # do the rest in copyconstraints! which is type stable
        res = loadconstraints!(dest, src, idxmap, F, S)
        res.status == MOI.CopySuccess || return res
    end

    return MOI.CopyResult(MOI.CopySuccess, "", idxmap)
end
