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
    for ci_src in MOI.get(src, MOI.ListOfConstraintIndices{F, S}())
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        f_dest = mapvariables(idxmap, f_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        ci_dest = MOI.addconstraint!(dest, f_dest, s)
        idxmap.conmap[ci_src] = ci_dest
    end
end

function defaultcopy!(dest::MOI.AbstractInstance, src::MOI.AbstractInstance)
    MOI.empty!(dest)

    idxmap = IndexMap()

    for vi in MOI.get(src, MOI.ListOfVariableIndices())
        idxmap.varmap[vi] = MOI.addvariable!(dest)
    end

    MOI.set!(dest, MOI.ObjectiveSense(), MOI.get(src, MOI.ObjectiveSense()))
    obj_src = MOI.get(src, MOI.ObjectiveFunction())
    obj_dest = mapvariables(idxmap, obj_src)
    MOI.set!(dest, MOI.ObjectiveFunction(), obj_dest)

    for (F, S) in MOI.get(src, MOI.ListOfConstraints())
        # do the rest in copyconstraints! which is type stable
        copyconstraints!(dest, src, idxmap, F, S)
    end

    idxmap
end
