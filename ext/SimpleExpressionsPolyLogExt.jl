module SimpleExpressionsPolyLogExt

import SimpleExpressions
using PolyLog

# special_funcs
for fn ∈ (:reli1, :reli2, :reli3, :reli4, :reli,
          :li0, :li1, :li2, :li3, :li4, :li5, :li6, :li)
    @eval begin
        PolyLog.$fn(x::SimpleExpressions.AbstractSymbolic, as...) = SimpleExpressions.SymbolicExpression(PolyLog.$fn, (x, as...))
    end
end




end
