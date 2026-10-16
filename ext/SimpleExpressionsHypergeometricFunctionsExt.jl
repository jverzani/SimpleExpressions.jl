module SimpleExpressionsHypergeometricFunctionsExt

import SimpleExpressions
import SimpleExpressions: D
using HypergeometricFunctions

# the parameters come first; the argument z is last
HypergeometricFunctions._₁F₁(a, b, z::SimpleExpressions.AbstractSymbolic) =
    SimpleExpressions.SymbolicExpression(_₁F₁, (a, b, z))
HypergeometricFunctions._₂F₁(a, b, c, z::SimpleExpressions.AbstractSymbolic) =
    SimpleExpressions.SymbolicExpression(_₂F₁, (a, b, c, z))
HypergeometricFunctions._₃F₂(a₁, a₂, a₃, b₁, b₂, z::SimpleExpressions.AbstractSymbolic) =
    SimpleExpressions.SymbolicExpression(_₃F₂, (a₁, a₂, a₃, b₁, b₂, z))

## derivatives in z
# d/dz ₁F₁(a; b; z) = (a / b) ₁F₁(a + 1; b + 1; z)
D(::typeof(_₁F₁), args, x) = ((a, b, 𝑧) = args; D(𝑧, x) * a / b * _₁F₁(a + 1, b + 1, 𝑧))
# d/dz ₂F₁(a, b; c; z) = (a b / c) ₂F₁(a + 1, b + 1; c + 1; z)
D(::typeof(_₂F₁), args, x) = ((a, b, c, 𝑧) = args; D(𝑧, x) * a * b / c * _₂F₁(a + 1, b + 1, c + 1, 𝑧))

end
