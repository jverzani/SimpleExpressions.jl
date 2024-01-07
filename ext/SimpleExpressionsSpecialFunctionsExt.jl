module SimpleExpressionsSpecialFunctionsExt

import SimpleExpressions
using SpecialFunctions

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
        SpecialFunctions.$fn(x::SimpleExpressions.AbstractSymbolic, as...) = SimpleExpressions.SymbolicExpression(SpecialFunctions.$fn, (x, as...))
    end
end

## derivatives
import SimpleExpressions: D

# cf Calculus.jl/src/differentiate.jl
D(::typeof(erf), args)   = (𝑥 = only(args); D(𝑥) * 2/sqrt(pi) * exp(-𝑥^2))
D(::typeof(erfinv), args)   = (𝑥 = only(args); D(𝑥) * (1/2) * sqrt(pi) * exp(erfinv(𝑥) * erfinv(𝑥)))
D(::typeof(erfc), args)   = (𝑥 = only(args); D(𝑥) * -2 * exp(-𝑥*𝑥) / sqrt(pi)   )
D(::typeof(erfcinv), args)   = (𝑥 = only(args); D(𝑥) * 0.5 * sqrt(pi) * exp(erfinv(𝑥) * erfinv(𝑥)))
D(::typeof(erfi), args)   = (𝑥 = only(args); D(𝑥) * 2 * exp(𝑥*𝑥) / sqrt(pi) )
D(::typeof(gamma), args)   = (𝑥 = only(args); D(𝑥) * digamma(𝑥) * gamma(𝑥) )
D(::typeof(lgamma), args)   = (𝑥 = only(args); D(𝑥) * digamma(𝑥)  )
D(::typeof(digamma), args)   = (𝑥 = only(args); D(𝑥) * trigamma(𝑥) )
D(::typeof(invdigamma), args)   = (𝑥 = only(args); D(𝑥) * 1 /trigamma(invdigamma(𝑥)))
D(::typeof(airyai), args)   = (𝑥 = only(args); D(𝑥) * airyaiprime(𝑥))
D(::typeof(airybi), args)   = (𝑥 = only(args); D(𝑥) * airybiprime(𝑥))
D(::typeof(airyaiprime), args)   = (𝑥 = only(args); D(𝑥) * 𝑥 * airyai(𝑥))
D(::typeof(airybiprime), args)   = (𝑥 = only(args); D(𝑥) * 𝑥 * airybi(𝑥) )
D(::typeof(besselj0), args)   = (𝑥 = only(args); D(𝑥) * -besselj1(𝑥))
D(::typeof(besselj1), args)   = (𝑥 = only(args); D(𝑥) * (besselj0(𝑥) - besselj(2, 𝑥)) / 2 )
D(::typeof(bessely0), args)   = (𝑥 = only(args); D(𝑥) * -bessely1(𝑥) )
D(::typeof(bessely1), args)   = (𝑥 = only(args); D(𝑥) * (bessely0(𝑥) - bessely(2, 𝑥)) / 2 )
D(::typeof(erfcx), args)   = (𝑥 = only(args); D(𝑥) * (2 * 𝑥 * erfcx(𝑥) - 2 / sqrt(pi)) )
D(::typeof(dawson), args)   = (𝑥 = only(args); D(𝑥) * (1 - 2𝑥 * dawson(𝑥))  )





end
