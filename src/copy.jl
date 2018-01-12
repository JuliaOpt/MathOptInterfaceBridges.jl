struct IndexMap
    varmap::Dict{MOI.VariableIndex, MOI.VariableIndex}
    conmap::Dict{MOI.ConstraintIndex, MOI.ConstraintIndex}
    IndexMap() = new(Dict{MOI.VariableIndex, MOI.VariableIndex}(),
                     Dict{MOI.ConstraintIndex, MOI.ConstraintIndex}())
end
Base.getindex(idxmap::IndexMap, ci::MOI.VariableIndex) = idxmap.varmap[ci]
function Base.getindex(idxmap::IndexMap, ci::MOI.ConstraintIndex{F, S}) where {F, S}
    idxmap.conmap[ci]::MOI.ConstraintIndex{F, S}
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
        if MOI.canaddconstraint(dest, f_dest, s)
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
            if !MOI.canset(src, attr, CI{F, S})
                @show attr
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
            if !MOI.canset(src, attr, VI)
                @show attr
                return MOI.CopyResult(MOI.CopyUnsupportedAttribute, "Unsupported attribute $attr", idxmap)
            end
            vector_of_values = MOI.get(src, attr, vis_src)
            MOI.set!(dest, attr, vis_dest, vector_of_values)
        end
    end

    # Copy instance attributes
    for attr in MOI.get(src, MOI.ListOfInstanceAttributesSet())
        if MOI.canget(src, attr)
            if !MOI.canset(src, attr)
                @show attr
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
