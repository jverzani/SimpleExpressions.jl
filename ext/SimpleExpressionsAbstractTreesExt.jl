module SimpleExpressionsAbstractTreesExt

using SimpleExpressions
import SimpleExpressions: AbstractSymbolic,
    SymbolicNumber,
    SymbolicExpression, SymbolicEquation
import TermInterface
import AbstractTrees

# use fallback of () for others
AbstractTrees.children(x::SymbolicExpression) = TermInterface.children(x)
AbstractTrees.children(x::SymbolicEquation) = MethodError(AbstractTrees.children, SymbolicExpression)

AbstractTrees.nodevalue(n::SymbolicNumber) = n()
AbstractTrees.nodevalue(n::SymbolicExpression) = TermInterface.operation(n)
AbstractTrees.nodevalue(::SymbolicEquation) = MethodError(AbstractTrees.nodevalue, SymbolicExpression)




end
