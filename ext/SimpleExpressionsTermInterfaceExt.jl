module SimpleExpressionsTermInterfaceExt

using SimpleExpressions

import SimpleExpressions: AbstractSymbolic, Symbolic, SymbolicParameter, SymbolicExpression, SymbolicEquation

using TermInterface

TermInterface.istree(::Symbolic) = true
TermInterface.istree(::SymbolicParameter) = true
TermInterface.istree(::AbstractSymbolic) = true

TermInterface.operation(X::SymbolicExpression) = X.op
TermInterface.operation(X::AbstractSymbolic) = () -> nothing
TermInterface.arguments(X::SymbolicExpression) = X.arguments
TermInterface.arguments(X::AbstractSymbolic) = ()

TermInterface.exprhead(X::SymbolicExpression) = _exprhead(X.op, X)
_exprhead(::typeof(getindex), X) = :ref
_exprhead(::Any, X) = :call

TermInterface.similarterm(X::SymbolicExpression, f, args, as...; kwargs...) = f(args...)

end
