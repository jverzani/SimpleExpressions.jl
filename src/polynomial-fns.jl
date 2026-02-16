
# polynomial in x?
ispolynomial(x) = Base.Fix2(ispolynomial, x)
ispolynomial(ex, x, n) = false # XXX----XXX degree
function ispolynomial(ex, x)::Bool
    !isvariable(x) && return false # need a symbol
    !contains(ex,x) && return true
    isequal(ex, x) && return true
    iscall(ex) || return false
    op = operation(ex)
    if op ∈ (+, -, *)
        return all(ispolynomial(x), arguments(ex))
    elseif op ∈ (/,)
        a, b = arguments(ex)
        return ispolynomial(a, x) && !contains(b,x)
    elseif op ∈ (^,)
        a, b = arguments(ex)
        return ispolynomial(a, x) && isnumeric(b) && unwrap_const(b) ≥ 0
    else
        return false # not constant, so must be nonliner
    end
    return false
end

"""
    coefficients(ex, x)

If expression or equation is a polynomial in `x`, return the coefficients. Otherwise return `nothing`.

## Example

```
julia> @symbolic x p;

julia> eq = x*(x+2)*(x-p) ~ 2;

julia> a0, as... = cs = SimpleExpressions.coefficients(eq, x)
(a₀ = -2, a₁ = -2 * p, a₂ = 2 + (-1 * p), a₃ = 1)

julia> a0 + sum(aᵢ*x^i for (i,aᵢ) ∈ enumerate(Iterators.rest(cs,2)) if !iszero(aᵢ))
-2 + (-2 * p * (x ^ 1)) + ((2 + (-1 * p)) * (x ^ 2)) + (1 * (x ^ 3))
```

Not exported.
"""
coefficients(ex::SymbolicEquation, x) = coefficients(ex.lhs - ex.rhs, x)
function coefficients(ex, x)
    n = poly_degree(ex, x)
    isnothing(n) && return nothing #throw(ArgumentError("expression is not a polynomial"))
    coeffs, ispoly = is_Πₙ(ex, x, n)
    nms = Tuple(SimpleExpressions._aᵢ(i) for i in 0:n)
    NamedTuple{nms}(tuple(coeffs...))
end

function _aᵢ(i)
    aᵢs = ("₀","₁","₂","₃","₄","₅","₆","₇","₈","₉")
    io = IOBuffer()
    print(io, "a")
    for j in Iterators.reverse(digits(i))
        print(io, aᵢs[1 + j])
    end
    Symbol(take!(io))
end


# Πₙ -- polys of degree n *or* less
function conv!(xs, ys)
    n,m = length(xs), length(ys)
    nz, nm = findlast.(!iszero, (xs, ys))
    !isnothing(nz) && !isnothing(nm) && (nz-1) * (nm-1) > n && return false
    zs = Any[0 for _ in eachindex(xs)]
    for i in 0:(n-1)
        for j in 0:(m-1)
            i + j + 1 > n && continue
            aij = xs[i+1] * ys[j+1]
            zs[i+j+1] += xs[i+1] * ys[j+1]
        end
    end
    xs[:] = zs
    return true
end


function is_Πₙ(ex, x, n)
    cs = Any[zero(Int) for i in 1: unwrap_const(n)+1]
    val = is_Πₙ!(cs, ex, x, unwrap_const(n))
    (cs, val)
end

function is_Πₙ!(cs, ex, x, n::Integer)
    @assert n ≥ 0
    # mutate cs, return bool
    if is_number(ex) || !contains(ex,x)
        cs[0+1] = ex
        return true
    end
    if isequal(ex, x)
        cs[1 + 1] = 1
        return true
    end
    if !iscall(ex)
        cs[0 + 1] = ex
        return true
    end
    op = operation(ex)
    cs′ = Any[zero(Int) for i in 1:n+1] #zeros(typeof(x), n+1)
    if op ∈ (+, -)
        for a ∈ arguments(ex)
            cs′ .= 0
            out = is_Πₙ!(cs′, a, x, n)
            !out && return false
            if op == +
                cs[:] = cs + cs′
            else
                cs[:] = cs - cs′
            end
        end
        return true
    elseif op ∈ (*,)
        a, as... = arguments(ex)
        is_Πₙ!(cs, a, x, n) || return false
        cs′ = Any[zero(c) for c in cs]
        for aᵢ ∈ as
            cs′[:] .= 0
            is_Πₙ!(cs′, aᵢ, x, n) || return false
            conv!(cs, cs′) || return false
        end
        return true
    elseif op ∈ (/,)
        a, b = arguments(ex)
        contains(b,x) && return false
        is_Πₙ!(cs, a, x, n) || return false
        cs ./= b
        return true
    elseif op ∈ (^,)
        a, b = arguments(ex)
        is_number(b) && unwrap_const(b) ≥ 0 || return false
        cs′ .= 0
        is_Πₙ!(cs′, a, x, n) || return false
        cs[:] = cs′[:]
        for i in 2:unwrap_const(b)
            conv!(cs, cs′) || return false
        end
        return true
    end
    return false
end

# If u is a polynomial in x of degree n, poly_degree(u,x) returns n::Int,
# else nothing (not false!)
poly_degree(ex::Real, x) = 0
function poly_degree(ex, x)
    notpoly = nothing
    !contains(ex,x) && return 0
    Symbol(ex) == Symbol(x)  && return 1
    iscall(ex) || return 0

    op = operation(ex)
    if op ∈ (+, -)
        k = 0
        for a ∈ arguments(ex)
            j = poly_degree(a,x)
            isnothing(j) && return notpoly
            k = max(k, j)
        end
        return unwrap_const(k)
    elseif op ∈ (*,)
        k = 0
        for a ∈ arguments(ex)
            j = poly_degree(a,x)
            isnothing(j) && return notpoly
            k += j
        end
        return k
    elseif op ∈ (/,)
        a, b = arguments(ex)
        contains(b,x) && return notpoly
        return poly_degree(a,x)
    elseif op ∈ (^,)
        a, b = arguments(ex)
        isnumeric(b) && unwrap_const(b) ≥ 0 || return  notpoly # must unwrap
        n = poly_degree(a,x)
        isnothing(n) && return notpoly
        return n*unwrap_const(b)
    end
    return notpoly
end
