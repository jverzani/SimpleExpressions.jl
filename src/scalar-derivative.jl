"""
    D(::AbstractSymbolic)

Finds derivative of symbolic expression.

* *assumes* a symbolic value is a scalar and takes derivative with respect to that
* There is no simplification, so the output is not necessarily friendly
* limited to a select set of functions

# Example

```julia
julia> @symbolic x p
(x, p)

julia> D(exp(sin(x)))
(1 * cos(x)) * exp(sin(x))

julia> D(sin(x)*cos(x-p)) # no simplification!
((1 * cos(x)) * cos(x - p)) + (sin(x) * (((-(0)) + 1) * (-(sin(x - p)))))
```

```
"""
D(::Any) = 0
D(::Symbolic) = 1
D(::SymbolicParameter) = 0

D(ex::SymbolicExpression) = D(ex.op, ex.arguments)

D(::typeof(+), args) = SymbolicExpression(+, D.(args))
D(::typeof(sum), args) = D(SymbolicExpression(+, args))

function D(::typeof(-), args)
    u = SymbolicExpression(-, (D(last(args)),))
    if length(args) > 1
        u = u + D(first(args))
    end
end

function D(::typeof(*), args)
    length(args) == 1 && return D(only(args))
    a, b... = args
    bb = length(b) == 1 ? only(b) : SymbolicExpression(*, b)
    D(a) * bb + a * D(bb)
end
D(::typeof(prod), args) = D(SymbolicExpression(*, args))

function D(::typeof(/), args)
    u,v = args
    (u * D(v) - D(u)*v) / v^2
end

function D(::typeof(^), args)
    a,b = args
    if !isa(b, AbstractSymbolic)
        return b*a^(b-1) * D(a)
    end
    return D(exp(b * log(a)))
end
D(::typeof(sqrt), args) = (𝑥 = only(args); D(𝑥) / sqrt(𝑥) * (1//2))
D(::typeof(cbrt), args) = (𝑥 = only(args); D(𝑥) / cbrt(𝑥)^2 * (1//3))

# idiosyncratic, x is a scalar for D
function D(::typeof(Base.broadcasted), args)
    op, as... = args
    D(SymbolicExpression(op, as))
end

D(::typeof(abs2), args)   = (𝑥 = only(args); D(𝑥) * 2𝑥)
D(::typeof(deg2rad), args)   = (𝑥 = only(args); D(𝑥) * pi / 180)
D(::typeof(rad2deg), args)   = (𝑥 = only(args); D(𝑥) * 180 / pi)


D(::typeof(exp), args)   = (𝑥 = only(args); D(𝑥) * exp(𝑥))
D(::typeof(exp2), args)  = (𝑥 = only(args); D(𝑥) * exp2(𝑥) * log(2))
D(::typeof(exp10), args) = (𝑥 = only(args); D(𝑥) * exp10(𝑥) * log(10))
D(::typeof(expm1), args) = (𝑥 = only(args); D(𝑥) * exp(𝑥))
D(::typeof(log), args)   = (𝑥 = only(args); D(𝑥) * 1/𝑥)
D(::typeof(log2), args)  = (𝑥 = only(args); D(𝑥) * 1/𝑥/log(2))
D(::typeof(log10), args) = (𝑥 = only(args); D(𝑥) * 1/𝑥/log(10))
D(::typeof(log1p), args) = (𝑥 = only(args); D(𝑥) * 1/(1 + 𝑥))

D(::typeof(sin), args) = (𝑥 = only(args); D(𝑥) *  cos(𝑥))
D(::typeof(cos), args) = (𝑥 = only(args); D(𝑥) * -sin(𝑥))
D(::typeof(tan), args) = (𝑥 = only(args); D(𝑥) *  sec(𝑥)^2)
D(::typeof(sec), args) = (𝑥 = only(args); D(𝑥) *  sec(𝑥) * tan(𝑥))
D(::typeof(csc), args) = (𝑥 = only(args); D(𝑥) * -csc(𝑥) * cot(𝑥))
D(::typeof(cot), args) = (𝑥 = only(args); D(𝑥) * -csc(𝑥)^2)

D(::typeof(asin), args) = (𝑥 = only(args); D(𝑥) / sqrt(1 - 𝑥^2))
D(::typeof(acos), args) = (𝑥 = only(args); D(𝑥) / (-sqrt(1 - 𝑥^2)))
D(::typeof(atan), args) = (𝑥 = only(args); D(𝑥) / (1 + 𝑥^2))
D(::typeof(asec), args) = (𝑥 = only(args); D(𝑥) / (abs(𝑥) * sqrt(𝑥^2 - 1)))
D(::typeof(acsc), args) = (𝑥 = only(args); D(𝑥) * (abs(𝑥) * sqrt(𝑥^2 - 1)) * (-1))
D(::typeof(acot), args) = (𝑥 = only(args); D(𝑥) / (1 + 𝑥^2) * (-1))

D(::typeof(sinh), args) = (𝑥 = only(args); D(𝑥) *  cosh(𝑥))
D(::typeof(cosh), args) = (𝑥 = only(args); D(𝑥) *  sinh(𝑥))
D(::typeof(tanh), args) = (𝑥 = only(args); D(𝑥) *  sech(𝑥)^2)
D(::typeof(sech), args) = (𝑥 = only(args); D(𝑥) * -sech(𝑥) * tanh(𝑥))
D(::typeof(csch), args) = (𝑥 = only(args); D(𝑥) * -csch(𝑥) * coth(𝑥))
D(::typeof(coth), args) = (𝑥 = only(args); D(𝑥) * -csch(𝑥)^2)

D(::typeof(sinpi), args) = (𝑥 = only(args); D(𝑥) *  π * cospi(𝑥))
D(::typeof(cospi), args) = (𝑥 = only(args); D(𝑥) * -π * sinpi(𝑥))
