module SimpleExpressionsTermInterfaceExt

using SimpleExpressions

import SimpleExpressions: AbstractSymbolic, Symbolic, SymbolicParameter, SymbolicExpression, SymbolicEquation

using TermInterface

TermInterface.istree(::Symbolic) = true
TermInterface.istree(::SymbolicParameter) = true
TermInterface.istree(::AbstractSymbolic) = true

TermInterface.operation(X::SymbolicExpression) = X.op
TermInterface.operation(X::AbstractSymbolic) = () -> X

TermInterface.arguments(X::SymbolicExpression) = collect(X.arguments)
TermInterface.arguments(X::AbstractSymbolic) = ()

TermInterface.exprhead(X::AbstractSymbolic) = :call
TermInterface.exprhead(X::SymbolicExpression) = _exprhead(X.op, X)
_exprhead(::typeof(getindex), X) = :ref
_exprhead(::Any, X) = :call



TermInterface.similarterm(X::Symbolic, f, args, as...; kwargs...) = X
TermInterface.similarterm(X::SymbolicEquation, f, args, as...; kwargs...) = error(f)

function TermInterface.similarterm(X::SymbolicExpression, f, args, as...; kwargs...)
    f(args...)
end

TermInterface.symtype(::T) where {T<:AbstractSymbolic} = T

TermInterface.arity(::AbstractSymbolic) = 0
TermInterface.arity(ex::SymbolicExpression) = length(ex.arguments)

TermInterface.metadata(::AbstractSymbolic) = nothing


# convert from Expression to SimpleExpression
# all variables become `𝑥` except `p` becomes `𝑝`, a parameter
function SimpleExpressions.assymbolic(x::Expr)
    body = _assymbolic(x)
    eval(body)
end

function _assymbolic(x)
    if !TermInterface.istree(x)
        isa(x, Symbol) && return x == :p ? :(SymbolicParameter(:𝑝)) : :(Symbolic(:𝑥))
        return x
    end

    op = TermInterface.operation(x)
    arguments = TermInterface.arguments(x)
    Expr(:call, op, _assymbolic.(arguments)...)
end

end
