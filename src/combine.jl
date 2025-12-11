# quickish method to combine terms in an expression

# ax + bx -> (a+b)x
# x^n*x^m -> x^(n+m)
"""
    combine(ex)

Lightly simplify symbolic expressions.


## Example

```@repl combine
julia> using SimpleExpressions: @symbolic, combine

julia> @symbolic x p
(x,)

julia> ex = 1 + x + 2x + 3x
1 + x + (2 * x) + (3 * x)

julia> combine(ex)
1 + (6 * x)

julia> ex = 1 + x^2 + 2x^2 + 3x*x + x^4/x
1 + (x ^ 2) + (2 * (x ^ 2)) + (3 * x * x) + ((x ^ 4) / x)

julia> combine(ex)
1 + (x ^ 3) + (6 * (x ^ 2))

julia> combine(x + p*x)
(1 + p) * x

```

Not exported. This will cancel terms such as `x/x`.

"""
function combine(ex::AbstractSymbolic, _isnumber=isconstant; n=5)
    _isnumber(ex) && return ex
    for _ in 1:n
        ex′ = _combine(ex, _isnumber)
        ex′ == ex && return ex
        ex = ex′
    end
    ex
end

_combine(x::T, _) where {T <: Union{Number, SymbolicNumber, SymbolicParameter, SymbolicVariable}} = x

function _combine(ex::AbstractSymbolic, _isnumber)
    _combine(operation(ex), ex, _isnumber)
end

function _combine(::typeof(+), ex::SymbolicExpression, _isnumber)
    ATERM(ex; _isnumber) |> materialize |> sum
end

function _combine(::typeof(*), ex::SymbolicExpression, _isnumber)
    MTERM(ex; _isnumber) |> materialize |> prod
end

function _combine(::typeof(/), ex::SymbolicExpression, _isnumber)
    MTERM(ex; _isnumber) |> materialize |> prod
end

function _combine(op::Any, ex::SymbolicExpression, _isnumber)
    maketerm(SymbolicExpression, op, combine.(arguments(ex),(_isnumber,)), nothing)
end


## ---- experimental
## SymEngine, Symbolics, ... use this structure to add
## c + c₁⋅T₁ + c₂⋅T₂ + ⋯
## and
## c ⋅ T₁^c₁ ⋅ T₂^c₂ ⋅ ⋯
## to multiply.
## Term uses a dict to store Tᵢ => cᵢ
## Term has constant (`c`) and terms fields
abstract type Term end

struct ATerm <: Term
    constant
    terms
end

struct MTerm <: Term
    constant
    terms
end

function Base.iterate(t::Term, state=nothing)
    isnothing(state) && return t.constant, 1
    state == 1 && return t.terms, 2
    nothing
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

function Base.:*(a::MTerm, b::MTerm)
    ca, cd = a
    ba, bd = b
    d = copy(cd)
    for (k,v) ∈ bd
        d[k] = get(d,k,0) + v
    end
    MTerm(ca * ba, d)
end

# materialize ATerm and MTerm as expressions
function materialize(a::ATerm)
    c,d = a
    c, sum(v*k for (k,v) ∈ d; init=SymbolicNumber(0))
end

ATERM(ex::Number, d=IdDict(); _isnumber=isconstant) = ATerm(SymbolicNumber(0),d)
ATERM(ex::SymbolicNumber, d=IdDict(); _isnumber=isconstant) = ATerm(ex, d)
function ATERM(x::𝑉, d=IdDict(); _isnumber=isconstant)
    d[x] = get(d, x, 0) + 1
    ATerm(SymbolicNumber(0), d)
end

ATERM(x::SymbolicExpression, d=IdDict(); _isnumber=isconstant) = ATERM(operation(x), x, d; _isnumber)

function ATERM(::typeof(+), x::SymbolicExpression, d; _isnumber=isconstant)
    b = SymbolicNumber(0)
    for a ∈ arguments(x)
        a′ = _combine(a, _isnumber)
        if _isnumber(a′)
            b += a′
        else
            c, d = ATERM(a′, d; _isnumber)
            b += c
        end
    end
    b′ = combine(b, isnumeric)
    ATerm(b′, d)
end

# fallback
function ATERM(::Any, x::SymbolicExpression, d; _isnumber=isconstant)
    m = MTERM(x; _isnumber)
    c, k = materialize(m)
    d[k] = get(d, k, 0) + c
    ATerm(SymbolicNumber(0), d)
end

## --- multiplicative terms simplified
# MTERM stores c*a₁^b₁*a₂^b+^2 as (c, Dict(a₁=>b₁, a₂ => b₂,...))

# materialize as  c*prod
function materialize(m::MTerm)
    c, d = m
    k = __from_mterm(d)
    c, k
end

_abs(x::Number) = abs(x)
_abs(x::AbstractSymbolic) = abs(x())
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

MTERM(x::SymbolicNumber, d= IdDict(); _isnumber=isconstant) = MTerm(x, d)
function MTERM(x::T, d = IdDict(); _isnumber=isconstant) where {T <: Union{SymbolicParameter, SymbolicVariable}}
    if _isnumber(x)
        MTerm(x, d)
    else
        d[x] = get(d, x, 0) + 1
        MTerm(SymbolicNumber(1), d)
    end
end

function MTERM(x::SymbolicExpression, d=IdDict(); _isnumber=isconstant)
    MTERM(operation(x), x, d; _isnumber)
end

function MTERM(::Any, x::SymbolicExpression, d; _isnumber=isconstant)
    d[x] = get(d, x, 0) + 1
    MTerm(SymbolicNumber(1), d)
end

function MTERM(::typeof(*), x::SymbolicExpression, d; _isnumber=isconstant)
    b = SymbolicNumber(1)
    for a ∈ arguments(x)
        a′ = _combine(a, _isnumber)
        if _isnumber(a′)
            b *= a′
        else
            c, d = MTERM(a′, d; _isnumber)
            b *= c
        end
    end
    b′ = combine(b, isnumeric)
    MTerm(b′, d)
end

function MTERM(::typeof(^), x::SymbolicExpression, d; _isnumber=isconstant)
    a, b = arguments(x)
    if isvariable(b)
        d[a] = get(d, a, 0) + b
        return MTerm(1, d)
    end

    c, dd = MTERM(a; _isnumber=isconstant)
    for (k,v) ∈ dd
        d[k] = get(d,k,0)  +  v * b
    end
    return MTerm(c^b, d)
end

# want c * (x1^p1 * x2^p2 ...)
function MTERM(::typeof(/), x::SymbolicExpression, d; _isnumber=isconstant)
    a, b = arguments(x)
    a′, b′ = combine.((a,b), _isnumber)
    num, u = MTERM(a′, d; _isnumber)
    den, v = MTERM(b′; _isnumber)

    for (var,pow) ∈ v
        u[var] = get(u, var, 0) - pow
    end

    return MTerm(num/den, u)
end

function MTERM(::typeof(+), x::SymbolicExpression, d; _isnumber=isconstant)
    a, b = ATERM(+, x, IdDict(); _isnumber)
    c = a + sum(k*v for (v,k) ∈ b; init=zero(x))
    d[c] = get(d, c, 0) + 1
    MTerm(one(x), d)
end
