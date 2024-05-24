module SimpleExpressionsAbstractTreesExt

using SimpleExpressions
import SimpleExpressions: AbstractSymbolic,
    Symbolic, SymbolicParameter, SymbolicNumber,
    SymbolicExpression, SymbolicEquation

import AbstractTrees

# use fallback of () for others
AbstractTrees.children(x::SymbolicExpression) = x.arguments
AbstractTrees.children(x::SymbolicEquation) = MethodError(AbstractTrees.children, SymbolicExpression)

AbstractTrees.nodevalue(n::Symbolic) = n.x
AbstractTrees.nodevalue(n::SymbolicParameter) = n.p
AbstractTrees.nodevalue(n::SymbolicNumber) = n.x
AbstractTrees.nodevalue(x::SymbolicEquation) = MethodError(AbstractTrees.nodevalue, SymbolicExpression)




end
