## ---- comparison, sorting

# only used for domain restrictions
Base.ifelse(p::AbstractSymbolic, a, b) = SymbolicExpression(ifelse, (p,a,b))

## utils?
Base.hash(x::AbstractSymbolic) = hash(↓(x))
Base.isequal(x::AbstractSymbolic, y::AbstractSymbolic) = hash(x) == hash(y)
Base.isequal(x::AbstractSymbolic, y::Real) = hash(x) == hash(y)
Base.isequal(x::Real, y::AbstractSymbolic) = hash(x) == hash(y)

# isless for sorting
# Number < SymbolicNumber < SymbolicParameter < SymbolicVariable < SymbolicExpression
Base.isless(::Number, ::AbstractSymbolic) = true
Base.isless(::AbstractSymbolic, ::Number) = false

Base.isless(::SymbolicNumber,     ::SymbolicVariable) = true
Base.isless(::SymbolicNumber,     ::SymbolicParameter) = true
Base.isless(::SymbolicNumber,     ::SymbolicExpression) = true

Base.isless(::SymbolicParameter,  ::SymbolicNumber) = false
Base.isless(::SymbolicParameter,  ::SymbolicVariable) = true
Base.isless(::SymbolicParameter,  ::SymbolicExpression) = true

Base.isless(::SymbolicVariable,   ::SymbolicNumber) = false
Base.isless(::SymbolicVariable,   ::SymbolicParameter) = false
Base.isless(::SymbolicVariable,   ::SymbolicExpression) = true

Base.isless(::SymbolicExpression, ::SymbolicNumber) = false
Base.isless(::SymbolicExpression, ::SymbolicVariable) = false
Base.isless(::SymbolicExpression, ::SymbolicParameter) = false

Base.isless(x::SymbolicNumber,   y::SymbolicNumber) =
    isless(x(), y())
Base.isless(x::SymbolicVariable, y::SymbolicVariable) =
    isless(Symbol(x), Symbol(y))
Base.isless(x::SymbolicParameter, y::SymbolicParameter)  =
    isless(Symbol(x), Symbol(y))

op_val(f) = Base.operator_precedence(Symbol(f))
function Base.isless(x::SymbolicExpression, y::SymbolicExpression)
    xo, yo = op_val(operation(x)), op_val(operation(y))
    isless(xo,yo) && return true
    isless(yo, xo) && return false
    xc, yc = children(x), children(y)
    isless(length(xc), length(yc)) && return true
    isless(length(yc), length(xc)) && return false
    for (cx, cy) ∈ zip(xc, yc)
        isless(cx, cy) && return true
        isless(cy, cx) && return false
    end
    false
end
