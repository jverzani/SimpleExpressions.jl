module SimpleExpressionsLatexifyExt

import SimpleExpressions
import Latexify

Latexify.@latexrecipe function f(x::SimpleExpressions.AbstractSymbolic)
    return string(x)
end

end
