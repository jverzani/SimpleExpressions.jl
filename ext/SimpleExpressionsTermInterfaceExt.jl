module SimpleSymbolicsTermInterfaceExt

using SimpleSymbolics

import SimpleSymbolics: AbstractSymbolic, Symbolic, SymbolicParameter, SymbolicExpression, SymbolicEquation

using TermInterface

TermInterface.istree(::Symbolic) = true
TermInterface.istree(::SymbolicParameter) = true
TermInterface.istree(::AbstractSymbolic) = true


TermInterface.operation(X::SymbolicExpression) = X.op
TermInterface.arguments(X::SymbolicExpression) = X.arguments

TermInterface.exprhead(X::SymbolicExpression) = _exprhead(X.op, X)
_exprhead(::typeof(getindex), X) = :ref
_exprhead(::Any, X) = :call

TermInterface.similarterm(X::SymbolicExpression, f, args, as...; kwargs...) = f(args...)

end
