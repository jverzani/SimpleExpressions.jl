module SimpleSymbolicsSpecialFunctionsExt

import SimpleSymbolics
import SpecialFunctions

# special_funcs
for fn ∈ (:airy,
          :airyai, :airyaiprime,
          :airybi, :airybiprime,
          :airyaix, :airyaiprimex,
          :airybix, :airybiprimex,
          :besselh, :besselhx,
          :besseli, :besselix,
          :besselj, :besselj0, :besselj1, :besseljx,
          :besselk, :besselkx,
          :bessely, :bessely0, :bessely1, :besselyx,
          :dawson,
          :erf, :erfc, :erfcinv, :erfcx, :erfi, :erfinv, :eta,
          :digamma, :invdigamma, :polygamma, :trigamma,
          :hankelh1, :hankelh1x, :hankelh2, :hankelh2x,
          :zeta)
    @eval begin
        SpecialFunctions.$fn(x::SimpleSymbolics.AbstractSymbolic, as...) = SimpleSymbolics.SymbolicExpression(SpecialFunctions.$fn, (x, as...))
    end
end

end
