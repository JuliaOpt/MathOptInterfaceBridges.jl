abstract type Constraints{F} end

struct ScalarConstraints{T, F<:MOI.AbstractScalarFunction} <: Constraints{F}
    eq::Vector{Tuple{CR{F, MOI.EqualTo{T}}, F, MOI.EqualTo{T}}}
    ge::Vector{Tuple{CR{F, MOI.GreaterThan{T}}, F, MOI.GreaterThan{T}}}
    le::Vector{Tuple{CR{F, MOI.LessThan{T}}, F, MOI.LessThan{T}}}
    # TODO add more sets
    function ScalarConstraints{T, F}() where {T, F}
        new{T, F}(Tuple{CR{F, MOI.EqualTo{T}}, F, MOI.EqualTo{T}}[], Tuple{CR{F, MOI.GreaterThan{T}}, F, MOI.GreaterThan{T}}[], Tuple{CR{F, MOI.LessThan{T}}, F, MOI.LessThan{T}}[])
    end
end

struct VectorConstraints{F<:MOI.AbstractVectorFunction} <: Constraints{F}
    eq::Vector{Tuple{CR{F, MOI.Zeros}, F, MOI.Zeros}}
    ge::Vector{Tuple{CR{F, MOI.Nonnegatives}, F, MOI.Nonnegatives}}
    le::Vector{Tuple{CR{F, MOI.Nonpositives}, F, MOI.Nonpositives}}
    sd::Vector{Tuple{CR{F, MOI.PositiveSemidefiniteConeTriangle}, F, MOI.PositiveSemidefiniteConeTriangle}}
    # TODO add more sets
    function VectorConstraints{F}() where F
        new{F}(Tuple{CR{F, MOI.Zeros}, F, MOI.Zeros}[], Tuple{CR{F, MOI.Nonnegatives}, F, MOI.Nonnegatives}[], Tuple{CR{F, MOI.Nonpositives}, F, MOI.Nonpositives}[], Tuple{CR{F, MOI.PositiveSemidefiniteConeTriangle}, F, MOI.PositiveSemidefiniteConeTriangle}[])
    end
end

const ZS = Union{MOI.EqualTo, MOI.Zeros}
const NS = Union{MOI.GreaterThan, MOI.Nonnegatives}
const PS = Union{MOI.LessThan, MOI.Nonpositives}
const DS = MOI.PositiveSemidefiniteConeTriangle

const SVF = MOI.SingleVariable
const VVF = MOI.VectorOfVariables
const SAF{T} = MOI.ScalarAffineFunction{T}
const VAF{T} = MOI.VectorAffineFunction{T}
const SQF{T} = MOI.ScalarQuadraticFunction{T}
const VQF{T} = MOI.VectorQuadraticFunction{T}

_getnoc(m::Constraints, noc::MOI.NumberOfConstraints{<:Any, <:ZS}) = _getnoc(m.eq, noc)
_getnoc(m::Constraints, noc::MOI.NumberOfConstraints{<:Any, <:NS}) = _getnoc(m.ge, noc)
_getnoc(m::Constraints, noc::MOI.NumberOfConstraints{<:Any, <:PS}) = _getnoc(m.le, noc)
_getnoc(m::Constraints, noc::MOI.NumberOfConstraints{<:Any, <:DS}) = _getnoc(m.sd, noc)

function _addconstraint!{F, S}(constrs::Vector{Tuple{CR{F, S}, F, S}}, cr::CR, f::F, s::S)
    push!(constrs, (cr, f, s))
    length(constrs)
end

function _delete!(constrs::Vector, cr::CR, i::Int)
    deleteat!(constrs, i)
    @view constrs[i:end] # will need to shift it in constrmap
end

_getfun(cr::CR, f::MOI.AbstractFunction, s::MOI.AbstractSet) = f
function _getfunction(constrs::Vector, cr::CR, i::Int)
    @assert cr.value == constrs[i][1].value
    _getfun(constrs[i]...)
end

_gets(cr::CR, f::MOI.AbstractFunction, s::MOI.AbstractSet) = s
function _getset(constrs::Vector, cr::CR, i::Int)
    @assert cr.value == constrs[i][1].value
    _gets(constrs[i]...)
end

_modifyconstr{F, S}(cr::CR{F, S}, f::F, s::S, change::F) = (cr, change, s)
_modifyconstr{F, S}(cr::CR{F, S}, f::F, s::S, change::S) = (cr, f, change)
_modifyconstr{F, S}(cr::CR{F, S}, f::F, s::S, change::MOI.AbstractFunctionModification) = (cr, modifyfunction(f, change), s)
function _modifyconstraint!{F, S}(constrs::Vector{Tuple{CR{F, S}, F, S}}, cr::CR{F}, i::Int, change)
    _modifyconstr(constrs[i]..., change)
end

function _getloc{F, S}(constrs::Vector{Tuple{CR{F, S}, F, S}})::Vector{Tuple{MOI.AbstractFunction, MOI.AbstractSet}}
    isempty(constrs) ? [] : [(F, S)]
end

function _getloc(m::ScalarConstraints)
    [_getnoc(m.eq); _getnoc(m.ge); _plist(m.le)]
end
function _getloc(m::VectorConstraints)
    [_getnoc(m.eq); _getnoc(m.ge); _plist(m.le); _dlist(m.sd)]
end

# We define this function instead of doing length directly in the functions below to have MethodError in case of e.g. NOC{scalar function, vector set}
_getnoc{F, S}(constrs::Vector{Tuple{CR{F, S}, F, S}}, noc::MOI.NumberOfConstraints{F, S}) = length(constrs)

mutable struct Instance{T}
    sense::MOI.OptimizationSense
    objective::SAF{T}
    sv::ScalarConstraints{T, SVF}
    sa::ScalarConstraints{T, SAF{T}}
    sq::ScalarConstraints{T, SQF{T}}
    vv::VectorConstraints{VVF}
    va::VectorConstraints{VAF{T}}
    vq::VectorConstraints{VQF{T}}
    nvars::UInt64
    nconstrs::UInt64
    constrmap::Vector{Int} # Constraint Reference value ci -> index in array in Constraints
    function Instance{T}() where T
        new{T}(MOI.FeasibilitySense, SAF{T}(MOI.VariableReference[], T[], zero(T)),
               ScalarConstraints{T, SVF}(), ScalarConstraints{T, SAF{T}}(), ScalarConstraints{T, SQF{T}}(), VectorConstraints{VVF}(), VectorConstraints{VAF{T}}(), VectorConstraints{VQF{T}}(),
               0, 0, Int[])
    end
end

getconstrloc(m::Instance, cr::CR) = m.constrmap[cr.value]

# Variables
MOI.getattribute(m::Instance, ::MOI.NumberOfVariables) = m.nvars
MOI.addvariable!(m::Instance) = MOI.VariableReference(m.nvars += 1)
function MOI.addvariables!(m::Instance, n::Integer)
    [MOI.addvariable!(m) for i in 1:n]
end

# Objective
MOI.getattribute(m::Instance, ::MOI.Sense) = m.sense
MOI.getattribute(m::Instance, ::MOI.ObjectiveFunction) = m.objective

function MOI.setobjective!(m::Instance, sense::MOI.OptimizationSense, f::MOI.AbstractFunction)
    m.sense = sense
    m.objective = f
end

function MOI.modifyobjective!(m::Instance, change::MOI.AbstractFunctionModification)
    m.objective = modifyfunction(m.objective, change)
end

# Constraints
function MOI.addconstraint!{F, S}(m::Instance, f::F, s::S)
    cr = CR{F, S}(m.nconstrs += 1)
    push!(m.constrmap, _addconstraint!(m, cr, f, s))
    cr
end

function MOI.delete!(m::Instance, cr::CR)
    for (ci, _, _) in _delete!(m, cr, getconstrloc(m, cr))
        m.constrmap[ci] -= 1
    end
    m.constrmap[cr.value] = 0
end

function MOI.modifyconstraint!(m::Instance, cr::CR, change)
    _modifyconstraint!(m, cr, getconstrloc(m, cr), change)
end

function MOI.getattribute(m::Instance, loc::MOI.ListOfConstraints)
    [_getloc(m.sv, loc); _getloc(m.sa, loc); _getloc(m.vv, loc); _getloc(m.va, loc)]
end

MOI.getattribute(m::Instance, noc::MOI.NumberOfConstraints) = _getnoc(m, noc)

MOI.cangetattribute(m::Instance, ::Union{MOI.NumberOfVariables,
                                         MOI.NumberOfConstraints,
                                         MOI.ListOfConstraints,
                                         MOI.ObjectiveFunction,
                                         MOI.Sense}) = true

MOI.cangetattribute(m::Instance, ::Union{MOI.ConstraintFunction,
                                         MOI.ConstraintSet}, ref) = true

function MOI.getattribute(m::Instance, ::MOI.ConstraintFunction, cr::CR)
    _getfunction(m, cr, getconstrloc(m, cr))
end

function MOI.getattribute(m::Instance, ::MOI.ConstraintSet, cr::CR)
    _getset(m, cr, getconstrloc(m, cr))
end

# Passing functions

for (fun, T) in ((:_addconstraint!, CR), (:_modifyconstraint!, CR), (:_delete!, CR), (:_getfunction, CR), (:_getset, CR), (:_getnoc, MOI.NumberOfConstraints))
    @eval begin
        $fun{F}(m::Constraints, cr::$T{F, <:ZS}, args...) = $fun(m.eq, cr, args...)
        $fun{F}(m::Constraints, cr::$T{F, <:NS}, args...) = $fun(m.ge, cr, args...)
        $fun{F}(m::Constraints, cr::$T{F, <:PS}, args...) = $fun(m.le, cr, args...)
        $fun{F}(m::Constraints, cr::$T{F, <:DS}, args...) = $fun(m.sd, cr, args...)

        $fun(m::Instance, cr::$T{<:SVF}, args...) = $fun(m.sv, cr, args...)
        $fun(m::Instance, cr::$T{<:SAF}, args...) = $fun(m.sa, cr, args...)
        $fun(m::Instance, cr::$T{<:SQF}, args...) = $fun(m.sq, cr, args...)
        $fun(m::Instance, cr::$T{<:VVF}, args...) = $fun(m.vv, cr, args...)
        $fun(m::Instance, cr::$T{<:VAF}, args...) = $fun(m.va, cr, args...)
        $fun(m::Instance, cr::$T{<:VQF}, args...) = $fun(m.vq, cr, args...)
    end
end
