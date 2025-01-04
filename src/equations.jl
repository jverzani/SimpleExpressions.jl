
## ----
"""
   a ~ b

Create a SymbolicEquation.

The equation has a left and right-hand side, which can be found by tuple destructing; calling `first` and `last`; by index; or field access using `.lhs` and `.rhs`.

Symbolic equations can be evaluated, in which case the value of `a-b` is returned.

When a symbolic equation is passed as an argument to a symbolic expression, the pair `a => b` is passed to `replace`.

The `D` function differentiates both sides. The `solve` function tries to move `x` terms to the left-hand side; and non-`x` terms to the right-hand side.
"""
struct SymbolicEquation{T,S}
    lhs::T
    rhs::S
end
Base.:~(a::AbstractSymbolic, b::AbstractSymbolic) = SymbolicEquation(a,b)
Base.:~(a::AbstractSymbolic, b::Number) = SymbolicEquation(a, SymbolicNumber(b))
Base.:~(a::Number, b::AbstractSymbolic) = SymbolicEquation(SymbolicNumber(a),b)

tilde(a::Number, b::Number) = a - b
tilde(a, b) = a ~ b

(X::SymbolicEquation)(x) =   tilde(X.lhs(x), X.rhs(x))
(X::SymbolicEquation)(x,p) = tilde(X.lhs(x, p),  X.rhs(x, p))
(X::SymbolicEquation)(args::Pair...) = tilde(X.lhs(args...), X.rhs(args...))


function Base.iterate(X::SymbolicEquation, state=nothing)
    isnothing(state) && return (X.lhs, 0)
    iszero(state) && return (X.rhs, 1)
    return nothing
end
Base.getindex(X::SymbolicEquation, i::Int) =
    i == 1 ? X.lhs : i == 2 ? X.rhs : nothing
Base.lastindex(X::SymbolicEquation) = 2
Base.length(X::SymbolicEquation) = 2

