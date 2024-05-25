module SimpleExpressionsAbstractTreesExt

using SimpleExpressions
import SimpleExpressions: AbstractSymbolic,
    SymbolicNumber,
    SymbolicExpression, SymbolicEquation

import AbstractTrees

# use fallback of () for others
AbstractTrees.children(x::SymbolicExpression) = x.arguments
AbstractTrees.children(x::SymbolicEquation) = MethodError(AbstractTrees.children, SymbolicExpression)

AbstractTrees.nodevalue(n::SymbolicNumber) = n.x
AbstractTrees.nodevalue(n::SymbolicExpression) = n.op
AbstractTrees.nodevalue(::SymbolicEquation) = MethodError(AbstractTrees.nodevalue, SymbolicExpression)




end
