## ---- types ----
abstract type AbstractSymbolic <: Function end
Base.broadcastable(x::AbstractSymbolic) = Ref(x)

# By design we have at most a single variable and a single parameter
struct SymbolicVariable{X, T <: StaticVariable{X}} <: AbstractSymbolic
    u::T
    SymbolicVariable(u::T) where {X, T <: StaticVariable{X}} = new{X,T}(u)
end

SymbolicVariable(x::SymbolicVariable) = x
SymbolicVariable(x::Symbol) = SymbolicVariable(StaticVariable{x}())
SymbolicVariable(x::AbstractString) = SymbolicVariable(Symbol(x))

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
Base.one(::AbstractSymbolic)  = SymbolicNumber(1)

# conveniences
𝑉 = Union{SymbolicVariable, SymbolicParameter}
𝐿 = Union{𝑉, SymbolicNumber}



# Expressions
const Δ = :nothing # flag for missing symbols 𝑥, 𝑝
struct SymbolicExpression{T <: StaticExpression} <: AbstractSymbolic
    u::T
end
SymbolicExpression(u) = SymbolicExpression(u)
function SymbolicExpression(op, children)
    u = StaticExpression(map(↓,children), op)
    SymbolicExpression(u)
end



## ----- CallableExpressions
_Variable = CallableExpressions.ExpressionTypeAliases.Variable


## ----- promotion/conversion


Base.promote_rule(::Type{<:AbstractSymbolic}, x::Type{T}) where {T <: Number} = AbstractSymbolic

Base.convert(::Type{<:AbstractSymbolic}, x::Number) = SymbolicNumber(DynamicConstant(x))
Base.convert(::Type{<:AbstractSymbolic}, x::SymbolicVariable) = x
Base.convert(::Type{<:AbstractSymbolic}, x::SymbolicParameter) = x
