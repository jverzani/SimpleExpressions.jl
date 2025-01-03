"""
    D(::AbstractSymbolic, [x])

Finds derivative of a symbolic expression with respect to a symbolic variable or parameter.

* Specify a variable to differentiate by, otherwise the lone symbolic variable (if present) will be used
* There is no simplification, so the output is not necessarily friendly
* limited to a select set of functions

# Example

```julia
julia> @symbolic x p
(x, p)

julia> D(exp(sin(x)), x)
(1 * cos(x)) * exp(sin(x))

julia> D(D(sin(x))) + sin(x) # no simplification!
(-(sin(x))) + sin(x)
```

```
"""
D(𝑥::SymbolicNumber, x) = 0
D(𝑥::SymbolicVariable, x) = 𝑥 == x ? 1 : 0
D(𝑥::SymbolicParameter, x) = 𝑥 == x ? 1 : 0
D(ex::SymbolicExpression, x) = D(TermInterface.operation(ex), TermInterface.children(ex), x)
D(ex::SymbolicEquation, x) = D(ex.lhs, x) ~ D(ex.rhs, x)

function D(ex::Any)
    𝑥, 𝑝 = free_symbols(ex)
    isempty(𝑥) && isempty(𝑝) && return 0
    isempty(𝑥) && throw(ArgumentError("No symbol specified or inferred"))
    length(𝑥) >= 2 && throw(ArgumentError("Too many symbolic variables to infer one. Specify a specific one"))
    x = SymbolicVariable(only(𝑥))
    D(ex, x)
end

D(ex::SymbolicEquation) = D(ex.lhs) ~ D(ex.rhs)


# plus
⊕(x::SymbolicNumber,y::SymbolicNumber) = SymbolicNumber(x() + y())
function ⊕(x,y)
    iszero(x) && return y
    iszero(y) && return x
    return x + y
end

# minus
⊖(x::SymbolicNumber,y::SymbolicNumber) = SymbolicNumber(x() - y())
function ⊖(x,y)
    iszero(x) && return -y
    iszero(y) && return x
    return x - y
end

# times
⊗(x::SymbolicNumber,y::SymbolicNumber) = SymbolicNumber(x() * y())
function ⊗(x,y)
    isone(x) && return y
    isone(y) && return x
    iszero(x) && return zero(x)
    iszero(y) && return zero(y)
    return x * y
end

# div
function ⨸(x::SymbolicNumber,y::SymbolicNumber)
    n, d = x(), y()
    # keep as rational?
    isa(n, Integer) && isa(d, Integer) && return SymbolicNumber(n // d)
    SymbolicNumber(x() / y())
end

function ⨸(x,y)
    x == y    && return one(x)
    isone(y)  && return x
    iszero(x) && return zero(x)
    !isinf(x) && isinf(y) && return zero(x)

    
    
    # can cancel?
    if is_operation(/)(y)
        a, b = children(y)
        return (x ⊗ b) ⨸ a
    end
    
    if is_operation(*)(x)
        if contains(x, y) # cancel y in x; return
            out = one(x)
            for c ∈ sort(children(x))
                c == y && continue
                out = out ⊗ c
            end
            return out
        end
    end
    
    return x / y
end

function D(::typeof(+), args, x)
    reduce(⊕, D.(args, x); init=zero(x))
end
D(::typeof(sum), args, x) = SymbolicExpression(+, D.(args), x)

function D(::typeof(-), args, x)
    return reduce(⊖, D.(args, x); init=zero(x))
end

function D(::typeof(*), args, x)
    args′ = D.(args, x)
    tot = zero(x)
    aa = Any[ai for ai in args] 
    for (i,ai′) ∈ enumerate(args′)
        copy!(aa, args)
        aa[i] = ai′
        tot = tot ⊕ reduce(⊗, aa)
    end
    return tot
end
D(::typeof(prod), args, x) = D(SymbolicExpression(*, args), x)

function D(::typeof(/), args, x)
    u,v = args
    u′, v′ = D(u,x), D(v,x)
    ((u′ ⊗ v) ⊖ (u ⊗ v′)) ⨸ (v⊗v)
end

function D(::typeof(^), args,x)
    a,b = args

    if !contains(b, x)
        iszero(b) && return zero(x)
        isone(b) && return D(a,x) ⊗ a
        isone(b-1) && return D(a,x) ⊗ (2*a)
        return  D(a,x) ⊗ (b*a^(b()-1))
    else
        return D(exp(b * log(a)),x)
    end
end

D(::typeof(sqrt), args,x) = (𝑥 = only(args); D(𝑥,x) / sqrt(𝑥) * (1//2))

D(::typeof(cbrt), args,x) = (𝑥 = only(args); D(𝑥,x) / cbrt(𝑥)^2 * (1//3))

# idiosyncratic, x is a scalar for D
function D(::typeof(Base.broadcasted), args,x)
    op, as... = args
    D(SymbolicExpression(op, as),x)
end

# idiosyncratic, ifelse only used for domain restrictions
# expected to be multiplied by other expressions
# use `&` or `|` to combine deferred logical expressions
𝕀(pred::AbstractSymbolic) = ifelse(pred, 1, NaN)

# ifelse is *assumed* to be a step function (1 or NaN, so has derivative 0)
# which works as 𝕀 is expected to be *multiplied* so
# (u ⋅ 𝕀)' = (u' ⋅ 𝕀 ) + U ⋅ 0 = u′ ⋅ 𝕀 which is what is desired.
D(::typeof(ifelse), args, x) = zero(x)

# (prefer NaN over error for technical reasons)
D(::typeof(inv), args,x)     = (𝑥 = only(args); D(𝑥,x) ⊗ -1/𝑥^2 ⊗ 𝕀(Ne(𝑥,0)))
D(::typeof(abs), args,x)     = (𝑥 = only(args); D(𝑥,x) ⊗ sign(𝑥) ⊗ 𝕀(Ne(𝑥, 0)))
D(::typeof(sign), args,x)    = (𝑥 = only(args); 0 ⊗ 𝕀(𝑥 != 0))
D(::typeof(abs2), args,x)    = (𝑥 = only(args); D(𝑥,x) ⊗ 2𝑥)
D(::typeof(deg2rad), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗ (pi / 180))
D(::typeof(rad2deg), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗ (180 / pi))

D(::typeof(exp), args,x)   = (𝑥 = only(args); D(𝑥,x) ⊗ exp(𝑥))
D(::typeof(exp2), args,x)  = (𝑥 = only(args); D(𝑥,x) ⊗ exp2(𝑥) ⊗ log(2))
D(::typeof(exp10), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗ exp10(𝑥) ⊗ log(10))
D(::typeof(expm1), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗ exp(𝑥))
D(::typeof(log), args,x)   = (𝑥 = only(args); D(𝑥,x) ⊗ (1/𝑥) ⊗ 𝕀(Ge(𝑥,0)))
D(::typeof(log2), args,x)  = (𝑥 = only(args); D(𝑥,x) ⊗ (1/𝑥/log(2)) ⊗ 𝕀(Ge(𝑥, 0)))
D(::typeof(log10), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗ (1/𝑥/log(10)) ⊗ 𝕀(Ge(𝑥, 0)))
D(::typeof(log1p), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗ 1/(1 + 𝑥))


D(::typeof(sin), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗  cos(𝑥))
D(::typeof(cos), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗ -sin(𝑥))
D(::typeof(tan), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗  sec(𝑥)^2)
D(::typeof(sec), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗  sec(𝑥) ⊗ tan(𝑥))
D(::typeof(csc), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗ -csc(𝑥) ⊗ cot(𝑥))
D(::typeof(cot), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗ -csc(𝑥)^2)

D(::typeof(asin), args,x) = (𝑥 = only(args); D(𝑥,x) / sqrt(1 - 𝑥^2))
D(::typeof(acos), args,x) = (𝑥 = only(args); D(𝑥,x) / (-sqrt(1 - 𝑥^2)))
D(::typeof(atan), args,x) = (𝑥 = only(args); D(𝑥,x) / (1 + 𝑥^2))
D(::typeof(asec), args,x) = (𝑥 = only(args); D(𝑥,x) / (abs(𝑥) ⊗ sqrt(𝑥^2 - 1)))
D(::typeof(acsc), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗ (abs(𝑥) ⊗ sqrt(𝑥^2 - 1)) ⊗ (-1))
D(::typeof(acot), args,x) = (𝑥 = only(args); D(𝑥,x) / (1 + 𝑥^2) ⊗ (-1))

D(::typeof(sinh), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗  cosh(𝑥))
D(::typeof(cosh), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗  sinh(𝑥))
D(::typeof(tanh), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗  sech(𝑥)^2)
D(::typeof(sech), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗ -sech(𝑥) ⊗ tanh(𝑥))
D(::typeof(csch), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗ -csch(𝑥) ⊗ coth(𝑥))
D(::typeof(coth), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗ -csch(𝑥)^2)

D(::typeof(sinpi), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗  π ⊗ cospi(𝑥))
D(::typeof(cospi), args,x) = (𝑥 = only(args); D(𝑥,x) ⊗ -π ⊗ sinpi(𝑥))
