# quickish method to combine terms in an expression

# ax + bx -> (a+b)x
# x^n*x^m -> x^(n+m)
"""
    combine(ex)

Lightly simplify symbolic expressions.


## Example

```@repl combine
julia> using SimpleExpressions: @symbolic, combine

julia> @symbolic x
(x,)

julia> ex = 1 + x + 2x + 3x
1 + x + (2 * x) + (3 * x)

julia> combine(ex)
1 + (6 * x)

julia> ex = 1 + x^2 + 2x^2 + 3x*x + x^4/x
1 + (x ^ 2) + (2 * (x ^ 2)) + (3 * x * x) + ((x ^ 4) / x)

julia> combine(ex)
1 + (x ^ 3) + (6 * (x ^ 2))

```

Not exported.

"""
function combine(ex::AbstractSymbolic, n=5)
    for _ in 1:n
        ex′ = _combine(ex)
        ex′ == ex && return ex
        ex = ex′
    end
    ex
end

function _combine(ex::AbstractSymbolic)
    m = ATERM(ex)
    u, v = _from_aterm(m)
    u + v
end

combine(x::Number) = x
## ---- experimental
## SymEngine, Symbolics, ... use this structure to add
## c + (c₁,T₁) + (c₂,T₂) + ⋯
## uses a dict to store Tᵢ => cᵢ
## TERM should have + or * types (powers or coefficients?)
## Term has constant, terms
abstract type Term end
struct ATerm <: Term
    constant
    terms
end
function Base.:+(a::ATerm, b::ATerm)
    ca, cd = a
    ba, bd = b
    d = copy(cd)
    for (k,v) ∈ bd
        d[k] = get(d,k,0) + v
    end
    ATerm(ca + ba, d)
end


struct MTerm <: Term
    constant
    terms
end

function Base.:*(a::MTerm, b::MTerm)
    ca, cd = a
    ba, bd = b
    d = copy(cd)
    for (k,v) ∈ bd
        d[k] = get(d,k,0) + v
    end
    MTerm(ca * ba, d)
end

function Base.iterate(t::Term, state=nothing)
    isnothing(state) && return t.constant, 1
    state == 1 && return t.terms, 2
    nothing
end

## -- we have ATERM and MTERM

# ATERM stores c + a₁*ex₁ + a₂*ex₂ as (c, Dict(ex₁=>a₁, ex₂ => a₂,...))
function _from_aterm(a)
    c,d = a
    c, sum(v*k for (k,v) ∈ d; init=SymbolicNumber(0))
end

ATERM(ex::Number, d=IdDict()) = ATerm( SymbolicNumber(0),d)
ATERM(ex::SymbolicNumber, d=IdDict()) = ATerm( ex, d)
function ATERM(x::𝑉, d=IdDict())
    d[x] = get(d, x, 0) + 1
    ATerm( SymbolicNumber(0), d)
end

ATERM(x::SymbolicExpression, d=IdDict()) = ATERM(operation(x), x, d)

function ATERM(::typeof(+), x::SymbolicExpression, d)
    b = SymbolicNumber(0)
    for a ∈ arguments(x)
        if isnumeric(a)
            b += a
        else
            # add MTERM to d
            m = MTERM(a)
            c, k = _from_mterm(m)
            d[k] = get(d, k, 0) + c
        end
    end
    ATerm( b, d)
end

# fallback
function ATERM(::Any, x::SymbolicExpression, d)
    m = MTERM(x)
    c, k = _from_mterm(m)
    d[k] = get(d, k, 0) + c
    ATerm( SymbolicNumber(0), d)
end



## --- multiplicative terms simplified
# MTERM stores c*a₁^b₁*a₂^b+^2 as (c, Dict(a₁=>b₁, a₂ => b₂,...))
# -> c*prod
_abs(x::Number) = abs(x)
_abs(x::AbstractSymbolic) = abs(x())
function _from_mterm(m)
    c, d = m
    k = __from_mterm(d)
    c, k
end
function __from_mterm(d) # just from the dictionary
    den = num = SymbolicNumber(1)
    for (k,v) ∈ d
        iszero(v) && continue
        if isnegative(v)
            v = _abs(v)
            den *= isone(v) ? k : k^v
        else
            num *= isone(v) ? k : k^v
        end
    end

    num / den
end

MTERM(x::SymbolicNumber, d= IdDict()) = MTerm( x, d)
function MTERM(x::SymbolicVariable, d = IdDict())
    d[x] = get(d, x, 0) + 1
    MTerm( SymbolicNumber(1), d)
end
function MTERM(x::SymbolicParameter, d=IdDict())
    d[x] = get(d, x, 0) + 1
    MTerm( SymbolicNumber(1), d)
end

MTERM(x::SymbolicExpression, d=IdDict()) = MTERM(operation(x), x, d)

function MTERM(::Any, x::SymbolicExpression, d)
    d[x] = get(d, x, 0) + 1
    MTerm( SymbolicNumber(1), d)
end

function MTERM(::typeof(*), x::SymbolicExpression, d)
    c = SymbolicNumber(1)
    for xᵢ ∈ arguments(x)
        if isnumeric(xᵢ)
            c *= xᵢ
        else
            ct, d = MTERM(xᵢ, d)
            c *= ct
        end
    end
    MTerm( c, d)
end

function MTERM(::typeof(^), x::SymbolicExpression, d)
    a, b = arguments(x)
    if isvariable(b)
        d[a] = get(d, a, 0) + b
        return MTerm( 1, d)
    end

    c, dd = MTERM(a)
    for (k,v) ∈ dd
        d[k] = get(d,k,0)  +  v * b
    end
    return MTerm( c^b, d)
end

# want c * (x1^p1 * x2^p2 ...)
function MTERM(::typeof(/), x::SymbolicExpression, d)
    a, b = arguments(x)
    num, u = MTERM(a,d)
    den, v = MTERM(b)

    for (var,pow) ∈ v
        u[var] = get(u, var, 0) - pow
    end

    return MTerm( num/den, u)


end

function MTERM(::typeof(+), x::SymbolicExpression, d)
    a, b = ATERM(+, x, IdDict())
    c = a + sum(k*v for (v,k) ∈ b; init=zero(x))
    d[c] = get(d, c, 0) + 1
    MTerm( one(x), d)
end
