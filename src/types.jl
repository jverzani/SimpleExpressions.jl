## ---- types ----
##
abstract type AbstractSymbolic <: Function end
Base.broadcastable(x::AbstractSymbolic) = Ref(x)

# By design we *typically* have at most a single variable and a single parameter
struct SymbolicVariable{X, T <: StaticVariable{X}} <: AbstractSymbolic
    u::T
    SymbolicVariable(u::T) where {X, T <: StaticVariable{X}} = new{X,T}(u)
end

SymbolicVariable(x::SymbolicVariable) = x
SymbolicVariable(x::Symbol) = SymbolicVariable(StaticVariable{x}())
SymbolicVariable(x::AbstractString) = SymbolicVariable(Symbol(x))
SymbolicVariable(x::Tuple) = SymbolicVariable.(x)

# parameter
struct SymbolicParameter{T <: DynamicVariable} <: AbstractSymbolic
    u::T
    SymbolicParameter(u::T) where {T <: DynamicVariable} = new{T}(u)
end
SymbolicParameter(p::SymbolicParameter) = p
SymbolicParameter(p::Symbol) = SymbolicParameter(DynamicVariable(p))
SymbolicParameter(p::AbstractString) = SymbolicParameter(Symbol(p))

# wrap numbers
struct SymbolicNumber{T <: DynamicConstant} <: AbstractSymbolic
    u::T
end
SymbolicNumber(c::SymbolicNumber) = c
function SymbolicNumber(c::S) where {S <: Number}
    SymbolicNumber(DynamicConstant(c))
end

Base.zero(::AbstractSymbolic) = SymbolicNumber(0)
Base.zero(::Type{<:AbstractSymbolic}) = SymbolicNumber(0)
Base.one(::AbstractSymbolic)  = SymbolicNumber(1)
Base.one(::Type{<:AbstractSymbolic}) = SymbolicNumber(1)



# Expressions
struct SymbolicExpression{T <: StaticExpression} <: AbstractSymbolic
    u::T
end
SymbolicExpression(u) = SymbolicExpression(u)
function SymbolicExpression(op, children)
    u = StaticExpression(map(↓,children), op)
    SymbolicExpression(u)
end

## ----------
# @symbolic_variables has room for functions and guarded functions
# for symbolic functions, different call
struct SymbolicFunction{X, T <: StaticVariable{X}} <: AbstractSymbolic
    u::T
    SymbolicFunction(u::T) where {X, T <: StaticVariable{X}} = new{X,T}(u)
end
SymbolicFunction(x::Symbol) = SymbolicFunction(StaticVariable{x}())
SymbolicFunction(x::AbstractString) = SymbolicFunction(Symbol(x))
(f::SymbolicFunction)(xs...) = SymbolicExpression(f, xs)

## Guarded variables can be created by `@symbolic_variables`
## This just stores the passed value by key, nothing more.
const GuardedVariables = IdDict()
function GuardedSymbolicVariable(u; kwargs...)
    𝑢 = SymbolicVariable(u)
    GuardedVariables[𝑢] = kwargs
    𝑢
end


## ----------

# conveniences
𝑉 = Union{SymbolicVariable, SymbolicParameter}
𝐿 = Union{𝑉, SymbolicNumber} # not constant expressions though

## ----- promotion/conversion
Base.promote_rule(::Type{<:AbstractSymbolic}, x::Type{T}) where {T <: Number} = AbstractSymbolic

Base.convert(::Type{<:AbstractSymbolic}, x::Number) = SymbolicNumber(DynamicConstant(x))
Base.convert(::Type{<:AbstractSymbolic}, x::SymbolicVariable) = x
Base.convert(::Type{<:AbstractSymbolic}, x::SymbolicParameter) = x


## ---

## --- CallableExpressions --> SimpleExpression
# convert to symbolic; ↑ is an alias
assymbolic(x) = x
assymbolic(x::AbstractSymbolic) = x
assymbolic(x::Symbol) = SymbolicVariable(x)
assymbolic(x::Number) = SymbolicNumber(x)

assymbolic(u::DynamicConstant) = SymbolicNumber(u)
assymbolic(u::StaticVariable) = SymbolicVariable(u)
assymbolic(u::DynamicVariable) = SymbolicParameter(u)

assymbolic(u::StaticExpression) = SymbolicExpression(u)

# convert from Expression to SimpleExpression
# all variables become `𝑥` except `p` becomes `𝑝`, a parameter
assymbolic(x::Expr) = eval(_assymbolic(x))
function _assymbolic(x)
    if !iscall(x)
        # convert :p --> parameter, other symbol to variable :𝑥
        isa(x, Symbol) && return x == :p ? :(SymbolicParameter(:𝑝)) : :(SymbolicVariable(:𝑥))
        return x
    end

    op = operation(x)
    args = arguments(x)
    Expr(:call, op, _assymbolic.(args)...)
end


# ↑ \uparrow[tab]; returns SimpleExpression
↑ = assymbolic

## ---- SimpleExpressions --> CallableExpressions

# ↓ \downarrow[tab] returns something in `CallableExpressions.jl` language
↓(x::AbstractSymbolic) = x.u
↓(x::Number) = DynamicConstant(x)
↓(x::ExpressionTypeAliases.ExpressionLoosely) = x
↓(x) = DynamicConstant(x)
