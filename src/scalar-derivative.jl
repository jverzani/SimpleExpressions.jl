"""
    D(::AbstractSymbolic)

Finds derivative of symbolic expression.

* *assumes* a symbolic value is a scalar and takes derivative with respect to that; symbolic parameters are assumed to be constants
* There is no simplification, so the output is not necessarily friendly
* limited to a select set of functions

# Example

```julia
julia> @symbolic x p
(x, p)

julia> D(exp(sin(x)))
(1 * cos(x)) * exp(sin(x))

julia> D(D(sin(x))) + sin(x) # no simplification!
(-(sin(x))) + sin(x)
```

```
"""
D(::Any) = 0
D(::Symbolic) = 1
D(::SymbolicParameter) = 0

D(ex::SymbolicExpression) = D(ex.op, ex.arguments)


# slight simplifications here
Base.iszero(::AbstractSymbolic) = false
Base.isone(::AbstractSymbolic) = false

function ⊕(x,y)
    iszero(x) && return y
    iszero(y) && return x
    return x + y
end

function ⊖(x,y)
    iszero(x) && return -y
    iszero(y) && return x
    return x - y
end

function ⊗(x,y)
    iszero(x) && return x
    iszero(y) && return y
    isone(x) && return y
    isone(y) && return x
    return x * y
end

function D(::typeof(+), args)
    a, b= args
    D(a) ⊕ D(b)
end
D(::typeof(sum), args) = SymbolicExpression(+, D.(args))


function D(::typeof(-), args)
    ∂b = D(last(args))
    length(args) == 1 && return ∂b

    ∂a = D(first(args))
    iszero(∂a) && return -∂b
    iszero(∂b) && return ∂a
    return ∂a - ∂b
end

function D(::typeof(*), args)
    length(args) == 1 && return D(only(args))
    a, b... = args
    bb = length(b) == 1 ? only(b) : SymbolicExpression(*, b)
    ∂a = D(a)
    ∂bb = D(bb)
    (a ⊗ ∂bb) ⊕ (∂a ⊗ bb)
end
D(::typeof(prod), args) = D(SymbolicExpression(*, args))

function D(::typeof(/), args)
    u,v = args
    ((D(u) ⊗ v) ⊖ (u ⊗ D(v))) / v^2
end

function D(::typeof(^), args)
    a,b = args
    if !isa(b, AbstractSymbolic)
        isone(b) && return D(a) ⊗ a
        isone(b-1) && return D(a) ⊗ (2*a)
        return  D(a) ⊗ (b*a^(b-1))
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

# (prefer NaN over error for technical reasons)
D(::typeof(inv), args)     = (𝑥 = only(args); D(𝑥) ⊗ -abs2(inv(𝑥)) ⊗ ifelse(𝑥==0, NaN, 1))
D(::typeof(abs), args)     = (𝑥 = only(args); D(𝑥) ⊗ ifelse(𝑥==0, NaN, sign(𝑥)))
D(::typeof(abs2), args)    = (𝑥 = only(args); D(𝑥) ⊗ 2𝑥)
D(::typeof(deg2rad), args) = (𝑥 = only(args); D(𝑥) ⊗ pi / 180)
D(::typeof(rad2deg), args) = (𝑥 = only(args); D(𝑥) ⊗ 180 / pi)


D(::typeof(exp), args)   = (𝑥 = only(args); D(𝑥) ⊗ exp(𝑥))
D(::typeof(exp2), args)  = (𝑥 = only(args); D(𝑥) ⊗ exp2(𝑥) ⊗ log(2))
D(::typeof(exp10), args) = (𝑥 = only(args); D(𝑥) ⊗ exp10(𝑥) ⊗ log(10))
D(::typeof(expm1), args) = (𝑥 = only(args); D(𝑥) ⊗ exp(𝑥))
D(::typeof(log), args)   = (𝑥 = only(args); D(𝑥) ⊗ 1/𝑥 * ifelse(𝑥>0, 1, NaN))
D(::typeof(log2), args)  = (𝑥 = only(args); D(𝑥) ⊗ 1/𝑥/log(2) * ifelse(𝑥>0, 1, NaN))
D(::typeof(log10), args) = (𝑥 = only(args); D(𝑥) ⊗ 1/𝑥/log(10) * ifelse(𝑥>0, 1, NaN))
D(::typeof(log1p), args) = (𝑥 = only(args); D(𝑥) ⊗ 1/(1 + 𝑥))

D(::typeof(sin), args) = (𝑥 = only(args); D(𝑥) ⊗  cos(𝑥))
D(::typeof(cos), args) = (𝑥 = only(args); D(𝑥) ⊗ -sin(𝑥))
D(::typeof(tan), args) = (𝑥 = only(args); D(𝑥) ⊗  sec(𝑥)^2)
D(::typeof(sec), args) = (𝑥 = only(args); D(𝑥) ⊗  sec(𝑥) ⊗ tan(𝑥))
D(::typeof(csc), args) = (𝑥 = only(args); D(𝑥) ⊗ -csc(𝑥) ⊗ cot(𝑥))
D(::typeof(cot), args) = (𝑥 = only(args); D(𝑥) ⊗ -csc(𝑥)^2)

D(::typeof(asin), args) = (𝑥 = only(args); D(𝑥) / sqrt(1 - 𝑥^2))
D(::typeof(acos), args) = (𝑥 = only(args); D(𝑥) / (-sqrt(1 - 𝑥^2)))
D(::typeof(atan), args) = (𝑥 = only(args); D(𝑥) / (1 + 𝑥^2))
D(::typeof(asec), args) = (𝑥 = only(args); D(𝑥) / (abs(𝑥) ⊗ sqrt(𝑥^2 - 1)))
D(::typeof(acsc), args) = (𝑥 = only(args); D(𝑥) ⊗ (abs(𝑥) ⊗ sqrt(𝑥^2 - 1)) ⊗ (-1))
D(::typeof(acot), args) = (𝑥 = only(args); D(𝑥) / (1 + 𝑥^2) ⊗ (-1))

D(::typeof(sinh), args) = (𝑥 = only(args); D(𝑥) ⊗  cosh(𝑥))
D(::typeof(cosh), args) = (𝑥 = only(args); D(𝑥) ⊗  sinh(𝑥))
D(::typeof(tanh), args) = (𝑥 = only(args); D(𝑥) ⊗  sech(𝑥)^2)
D(::typeof(sech), args) = (𝑥 = only(args); D(𝑥) ⊗ -sech(𝑥) ⊗ tanh(𝑥))
D(::typeof(csch), args) = (𝑥 = only(args); D(𝑥) ⊗ -csch(𝑥) ⊗ coth(𝑥))
D(::typeof(coth), args) = (𝑥 = only(args); D(𝑥) ⊗ -csch(𝑥)^2)

D(::typeof(sinpi), args) = (𝑥 = only(args); D(𝑥) ⊗  π ⊗ cospi(𝑥))
D(::typeof(cospi), args) = (𝑥 = only(args); D(𝑥) ⊗ -π ⊗ sinpi(𝑥))
