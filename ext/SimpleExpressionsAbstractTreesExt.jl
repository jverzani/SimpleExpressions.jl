module SimpleExpressionsAbstractTreesExt

using SimpleExpressions
import SimpleExpressions: AbstractSymbolic,
    SymbolicNumber,
    SymbolicExpression, SymbolicEquation


import AbstractTrees

# use fallback of () for others
AbstractTrees.children(x::SimpleExpressions.SymbolicCall) = SimpleExpressions.children(x)
AbstractTrees.children(x::SymbolicEquation) = MethodError(AbstractTrees.children, SymbolicExpression)

AbstractTrees.nodevalue(n::SymbolicNumber) = n()
AbstractTrees.nodevalue(n::SimpleExpressions.SymbolicCall) = SimpleExpressions.operation(n)
AbstractTrees.nodevalue(::SymbolicEquation) = MethodError(AbstractTrees.nodevalue, SymbolicExpression)




end
