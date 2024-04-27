module SimpleExpressionsRecipesBaseExt

using SimpleExpressions
using RecipesBase

@recipe f(::Type{T}, v::T) where {T<:SimpleExpressions.SymbolicEquation} = [v.lhs, v.rhs]

end
