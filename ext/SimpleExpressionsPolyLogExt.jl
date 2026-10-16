module SimpleExpressionsPolyLogExt

import SimpleExpressions
import SimpleExpressions: D
using PolyLog

# special_funcs
for fn ∈ (:reli1, :reli2, :reli3, :reli4,
          :li0, :li1, :li2, :li3, :li4, :li5, :li6)
    @eval begin
        PolyLog.$fn(x::SimpleExpressions.AbstractSymbolic, as...) = SimpleExpressions.SymbolicExpression($fn, (x, as...))
    end
end

# reli(n, x), li(n, x): the order comes first
for fn ∈ (:reli, :li)
    @eval begin
        PolyLog.$fn(n, x::SimpleExpressions.AbstractSymbolic) = SimpleExpressions.SymbolicExpression($fn, (n, x))
    end
end

## derivatives
# d/dx Liₙ(x) = Liₙ₋₁(x) / x
D(::typeof(reli), args, x) = ((n, 𝑥) = args; D(𝑥, x) * reli(n - 1, 𝑥) / 𝑥)
D(::typeof(li), args, x)   = ((n, 𝑥) = args; D(𝑥, x) * li(n - 1, 𝑥) / 𝑥)

end
