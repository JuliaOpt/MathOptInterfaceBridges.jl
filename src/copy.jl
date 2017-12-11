struct IndexMap
    varmap::Dict{VariableIndex, VariableIndex}
    conmap::Dict{ConstraintIndex, ConstraintIndex}
    IndexMap() = new(Dict{VariableIndex, VariableIndex}(),
                     Dict{ConstraintIndex, ConstraintIndex}())
end
Base.getindex(idxmap::IndexMap, ci::VariableIndex) = idxmap.varmap[ci]
function Base.getindex(idxmap::IndexMap, ci::ConstraintIndex{F, S}) where {F, S}
    idxmap.conmap[ci]::ConstraintIndex{F, S}
end

"""
    copyconstraints!(dest::AbstractInstance, src::AbstractInstance, idxmap::IndexMap, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}

Copy the constraints of type `F`-in-`S` from the instance `src` the instance `dest` and fill `idxmap` accordingly.
"""
function copyconstraints!(dest::AbstractInstance, src::AbstractInstance, idxmap::IndexMap, ::Type{F}, ::Type{S}) where {F<:MOI.AbstractFunction, S<:MOI.AbstractSet}
    for ci_src in MOI.get(src, MOI.ListOfConstraintIndices{F, S}())
        f_src = MOI.get(src, MOI.ConstraintFunction(), ci_src)
        f_dest = mapvariables(idxmap, f_src)
        s = MOI.get(src, MOI.ConstraintSet(), ci_src)
        ci_dest = MOI.addconstraint!(dest, f, s)
        idxmap.conmap[ci_src] = ci_dest
    end
end

function copy!(dest::AbstractInstance, src::AbstractInstance)
    empty!(dest)

    idxmap = IndexMap()

    for vi in MOI.get(src, MOI.ListOfVariableIndices())
        idxmap.varmap[vi] = MOI.addvariable!(dest)
    end

    MOI.set!(dest, MOI.ObjectiveSense(), MOI.get(src, MOI.ObjectiveSense()))
    obj_src = MOI.get(src, MOI.ObjectiveFunction())
    obj_dest = mapvariables(idxmap, obj_src)
    MOI.set!(dest, MOI.ObjectiveFunction(), obj_dest)

    for (F, S) in MOI.get(m.instance, MOI.ListOfConstraints())
        # do the rest in copyconstraints! which is type stable
        copyconstraints!(dest, src, idxmap, F, S)
    end
end
