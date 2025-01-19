# quickish method to combine terms in an expression

# ax + bx -> (a+b)x
# x^n*x^m -> x^(n+m)
"""
    combine(ex)

Lightly simplify symbolic expressions.


## Example

```@repl combine
julia> using SimpleExpressions

julia> import SimpleExpressions: combine

julia> @symbolic x
(x,)

julia> ex = 1 + x + 2x + 3x
1 + x + (2 * x) + (3 * x)

julia> combine(ex)
1 + (6 * x)

julia> ex = 1 + x^2 + 2x^2 + 3x*x + x^4/x
1 + (x ^ 2) + (2 * (x ^ 2)) + (3 * x * x) + ((x ^ 4) / x)

julia> combine(ex)
1 + (6 * (x ^ 2)) + (x ^ 3)

```

Not exported.

"""
function combine(@nospecialize(ex))
    c, d = ATERM(ex)
    c + sum(isone(k) ? v : k*v for (v,k) ∈ d if !iszero(k); init=0)
end

## ---- experimental
## SymEngine uses this structure to add
## c + (c₁,T₁) + (c₂,T₂) + ⋯
## uses a dict to store Tᵢ => cᵢ
## TERM should have + or * types (powers or coefficients?)
struct Term
    constant
    terms
end

function Base.iterate(t::Term, state=nothing)
    isnothing(state) && return t.constant, 1
    state == 1 && return t.terms, 2
    nothing
end

# A term c + k*v
ATERM(ex::SymbolicNumber, d=IdDict()) = Term(ex, d)
function ATERM(x::𝑉, d=IdDict())
    d[x] = get(d, x, 0) + 1
    Term(0, d)
end

ATERM(x::SymbolicExpression, d=IdDict()) = ATERM(operation(x), x, d)

function ATERM(::typeof(*), x, d=IdDict())
    c,dx = MTERM(x, IdDict())
    x′ =  one(x)
    for (v, k) ∈ dx
        iszero(k) && continue
        if isone(k)
            x′ *= v
        elseif isnegative(k)
            x′ *= isone(-k) ? 1/v : (1/v)^(-k)
        else
            x′ *= v^k
        end
    end
    d[x′] = get(d, x′, 0) + c
    Term(zero(x), d)
end


function ATERM(::typeof(/), x, d=IdDict())
    c, dx = MTERM(x)
    e = one(x)
    for (v,k) ∈ dx
        iszero(k) && continue
        if isone(k)
            e = e * v
        elseif isnegative(k)
            e = e * (isone(-k) ? 1/v : (1/v)^(-k))
        else
            e = e * v^k
        end
    end
    d[e] = get(d, e, 0) + c
    return Term(zero(x), d)

    
    a, b = arguments(x)
    ac, ad = ATERM(a)
    bc, bd = ATERM(b)
    c = iszero(bc) ? ac : ac / bc 
    denom = prod(v*k for (v,k) ∈ bd; init=1)
    if isone(denom)
        Term(c, copy(av))
    else
        d = IdDict()
        for (v,k) ∈ ad
            vv = v/denom
            d[vv] = get(d, vv, 0) + k
        end
        Term(c, d)
    end
end

# (cxyz)^n -> c^n, x^n y^n z^n => (0, (x^n y^n z^n,c^n)
function ATERM(::typeof(^), x, d)
    xc, xd = MTERM(x, IdDict())
    v = prod(isone(k) ? v : v^k for (v,k) ∈ xd; init=1)
    d[v] = get(d, v, 0) + xc
    Term(0,d)
end

function ATERM(::typeof(-), x,d)
    a, b = arguments(x)
    TERM(a + (-b))
end
    
function ATERM(::Any, x, d)
    d[x] = get(d,x,0) + 1
    Term(0, d)
end

function ATERM(::typeof(+), x, d)
    c = 0
    for a in arguments(x)
        ca, d = ATERM(a,d)
        c = c + ca
    end
    Term(c, d)
end

## --- multiplicative terms simplified

MTERM(x::SymbolicNumber, d= IdDict()) = Term(x, d)
function MTERM(x::SymbolicVariable, d = IdDict())
    d[x] = get(d, x, 0) + 1
    Term(1, d)
end
function MTERM(x::SymbolicParameter, d=IdDict())
    d[x] = get(d, x, 0) + 1
    Term(1, d)
end

MTERM(x::SymbolicExpression, d=IdDict()) = MTERM(operation(x), x, d)

function MTERM(::Any, x, d)
    d[x] = get(d, x, 0) + 1
    Term(1, d)
end

function MTERM(::typeof(*), x, d)
    cs,ts = tuplesplit(Base.Fix2(isa, SymbolicNumber), sorted_arguments(x))
    c = prod(cs, init=1)
    for t ∈ ts
        ct,d = MTERM(t, d)
        c = c * ct
    end
    Term(c, d)
end

function MTERM(::typeof(^), x, d)
    a, b = arguments(x)
    if is_operation(*)(a)
        cs,ts = tuplesplit(Base.Fix2(isa, SymbolicNumber), sorted_arguments(a))
        if isnegative(b)
            c = prod((1/cᵢ)^b for cᵢ in cs; init=1)
        else
            c = prod(cᵢ^b for cᵢ in cs; init=1)
        end
        for t ∈ ts
            d[t] = get(d,t,0) + b
        end
    elseif isconstant(a) && isconstant(b)
        return Term(a^b, d)
    else
        c = 1
        d[a] = get(d, a, 0) + b
    end
    Term(c, d)
end

# want c * (x1^p1 * x2^p2 ...)
function MTERM(::typeof(/), x, d)
    a, b = arguments(x)
    ac, ad = MTERM(a, d)
    if is_operation(*)(b)
        bs′ = tuple((SymbolicExpression(^, (b, -1)) for b in arguments(b))...)
        b′ = maketerm(SymbolicExpression, *, bs′, nothing)
        bc, bd = MTERM(b′,ad)
    else
        bc, bd′ = MTERM(b, IdDict())
        bd = copy(ad)
        for (v,k) ∈ bd′
            bd[v] = get(d,v,0) - k
        end
    end
    c = ac / bc
    Term(c, bd)
end


function MTERM(::typeof(+), x, d)
    a, b = ATERM(+, x, IdDict())
    c = a + sum(k*v for (v,k) ∈ b; init=zero(x))
    d[c] = get(d, c, 0) + 1
    Term(one(x), d)
end
