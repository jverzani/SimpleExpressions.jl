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
end
