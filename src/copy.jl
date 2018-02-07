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

    # Copy constraint attributes
    cis_dest = map(ci -> idxmap[ci], cis_src)
    for attr in MOI.get(src, MOI.ListOfConstraintAttributesSet{F, S}())
        if MOI.canget(src, attr, CI{F, S})
            if !MOI.canset(dest, attr, CI{F, S})
                return MOI.CopyResult(MOI.CopyUnsupportedAttribute, "Unsupported attribute $attr", idxmap)
            end
            vector_of_values = MOI.get(src, attr, cis_src)
            MOI.set!(dest, attr, cis_dest, vector_of_values)
        end
    end

    return MOI.CopyResult(MOI.CopySuccess, "", idxmap)
end

attribute_value_map(idxmap, f::MOI.AbstractFunction) = mapvariables(idxmap, f)
attribute_value_map(idxmap, attribute_value) = attribute_value
function defaultcopy!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance)
    MOI.empty!(dest)

    idxmap = IndexMap()

    # Copy variables
    vis_src = MOI.get(src, MOI.ListOfVariableIndices())
    for vi in vis_src
        idxmap.varmap[vi] = MOI.addvariable!(dest)
    end

    # Copy variable attributes
    vis_dest = map(vi -> idxmap[vi], vis_src)
    for attr in MOI.get(src, MOI.ListOfVariableAttributesSet())
        if MOI.canget(src, attr, VI)
            if !MOI.canset(dest, attr, VI)
                return MOI.CopyResult(MOI.CopyUnsupportedAttribute, "Unsupported attribute $attr", idxmap)
            end
            vector_of_values = MOI.get(src, attr, vis_src)
            MOI.set!(dest, attr, vis_dest, vector_of_values)
        end
    end

    # Copy instance attributes
    for attr in MOI.get(src, MOI.ListOfInstanceAttributesSet())
        if MOI.canget(src, attr)
            if !MOI.canset(dest, attr)
                return MOI.CopyResult(MOI.CopyUnsupportedAttribute, "Unsupported attribute $attr", idxmap)
            end
            MOI.set!(dest, attr, attribute_value_map(idxmap, MOI.get(src, attr)))
        end
    end

    # Copy constraints
    for (F, S) in MOI.get(src, MOI.ListOfConstraints())
        # do the rest in copyconstraints! which is type stable
        res = copyconstraints!(dest, src, idxmap, F, S)
        if res.status != MOI.CopySuccess
            return res
        end
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
    canallocateconstraint(instance::AbstractInstance, func::AbstractFunction, set::AbstractSet)::Bool

Return a `Bool` indicating whether it is possible to allocate the constraint ``f(x) \\in \\mathcal{S}`` where ``f`` is defined by `func`, and ``\\mathcal{S}`` is defined by `set`.
"""
canallocateconstraint(instance::MOI.AbstractInstance, func::MOI.AbstractFunction, set::MOI.AbstractSet) = false

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
    canloadconstraint(instance::AbstractInstance, func::AbstractFunction, set::AbstractSet)::Bool

Return a `Bool` indicating whether it is possible to load the constraint ``f(x) \\in \\mathcal{S}`` where ``f`` is defined by `func`, and ``\\mathcal{S}`` is defined by `set`.
"""
canloadconstraint(instance::MOI.AbstractInstance, func::MOI.AbstractFunction, set::MOI.AbstractSet) = false

function allocateconstraints!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance, idxmap::IndexMap, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    # Allocate constraints
    cis_src = MOI.get(src, MOI.ListOfConstraintIndices{F, S}())
    for ci_src in cis_src
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        f_dest = mapvariables(idxmap, f_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        if canallocateconstraint(dest, f_dest, s)
            ci_dest = allocateconstraint!(dest, f_dest, s)
            idxmap.conmap[ci_src] = ci_dest
        else
            return MOI.CopyResult(MOI.CopyUnsupportedConstraint, "Unsupported $F-in-$S constraint", idxmap)
        end
    end

    # Allocate constraint attributes
    cis_dest = map(ci -> idxmap[ci], cis_src)
    for attr in MOI.get(src, MOI.ListOfConstraintAttributesSet{F, S}())
        if MOI.canget(src, attr, CI{F, S})
            if !canallocate(dest, attr, CI{F, S})
                return MOI.CopyResult(MOI.CopyUnsupportedAttribute, "Unsupported attribute $attr", idxmap)
            end
            vector_of_values = MOI.get(src, attr, cis_src)
            allocate!(dest, attr, cis_dest, vector_of_values)
        end
    end

    return MOI.CopyResult(MOI.CopySuccess, "", idxmap)
end

function loadconstraints!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance, idxmap::IndexMap, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    # Load constraints
    cis_src = MOI.get(src, MOI.ListOfConstraintIndices{F, S}())
    for ci_src in cis_src
        ci_dest = idxmap[ci_src]
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        f_dest = mapvariables(idxmap, f_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        if canloadconstraint(dest, f_dest, s)
            loadconstraint!(dest, ci_dest, f_dest, s)
        else
            return MOI.CopyResult(MOI.CopyUnsupportedConstraint, "Unsupported $F-in-$S constraint", idxmap)
        end
    end

    # Load constraint attributes
    cis_dest = map(ci -> idxmap[ci], cis_src)
    for attr in MOI.get(src, MOI.ListOfConstraintAttributesSet{F, S}())
        if MOI.canget(src, attr, CI{F, S})
            if !canload(dest, attr, CI{F, S})
                return MOI.CopyResult(MOI.CopyUnsupportedAttribute, "Unsupported attribute $attr", idxmap)
            end
            vector_of_values = MOI.get(src, attr, cis_src)
            load!(dest, attr, cis_dest, vector_of_values)
        end
    end

    return MOI.CopyResult(MOI.CopySuccess, "", idxmap)
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
    for attr in MOI.get(src, MOI.ListOfVariableAttributesSet())
        if MOI.canget(src, attr, VI)
            if !canallocate(dest, attr, VI)
                return MOI.CopyResult(MOI.CopyUnsupportedAttribute, "Unsupported attribute $attr", idxmap)
            end
            vector_of_values = MOI.get(src, attr, vis_src)
            allocate!(dest, attr, vis_dest, vector_of_values)
        end
    end

    # Allocate instance attributes
    for attr in MOI.get(src, MOI.ListOfInstanceAttributesSet())
        if MOI.canget(src, attr)
            if !canallocate(dest, attr)
                return MOI.CopyResult(MOI.CopyUnsupportedAttribute, "Unsupported attribute $attr", idxmap)
            end
            allocate!(dest, attr, attribute_value_map(idxmap, MOI.get(src, attr)))
        end
    end

    # Allocate constraints
    for (F, S) in MOI.get(src, MOI.ListOfConstraints())
        # do the rest in copyconstraints! which is type stable
        allocateconstraints!(dest, src, idxmap, F, S)
    end

    # Load variables
    loadvariables!(dest, nvars)

    # Load variable attributes
    vis_dest = map(vi -> idxmap[vi], vis_src)
    for attr in MOI.get(src, MOI.ListOfVariableAttributesSet())
        if MOI.canget(src, attr, VI)
            if !canload(dest, attr, VI)
                return MOI.CopyResult(MOI.CopyUnsupportedAttribute, "Unsupported attribute $attr", idxmap)
            end
            vector_of_values = MOI.get(src, attr, vis_src)
            load!(dest, attr, vis_dest, vector_of_values)
        end
    end

    # Load instance attributes
    for attr in MOI.get(src, MOI.ListOfInstanceAttributesSet())
        if MOI.canget(src, attr)
            if !canload(dest, attr)
                return MOI.CopyResult(MOI.CopyUnsupportedAttribute, "Unsupported attribute $attr", idxmap)
            end
            load!(dest, attr, attribute_value_map(idxmap, MOI.get(src, attr)))
        end
    end

    # Copy constraints
    for (F, S) in MOI.get(src, MOI.ListOfConstraints())
        # do the rest in copyconstraints! which is type stable
        res = loadconstraints!(dest, src, idxmap, F, S)
        if res.status != MOI.CopySuccess
            return res
        end
    end

    return MOI.CopyResult(MOI.CopySuccess, "", idxmap)
end
