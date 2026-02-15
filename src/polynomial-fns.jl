
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
    # x is variable? expression?
    ispolynomial(ex, x) || return nothing
    ex = expand(ex)

    cs = is_operation(+)(ex) ? arguments(ex) : (ex,)
    d = Dict{Any, Any}()
    for c in cs
        (aᵢ, i) = _monomial(c, x)
        d[i] = aᵢ + get(d, i, zero(x))
    end

    n = maximum(collect(keys(d)))
    coeffs = Tuple(_combine_numbers(get(d,i,zero(x))) for i in 0:n)
    nms = Tuple(SimpleExpressions._aᵢ(i) for i in 0:n)

    NamedTuple{nms}(coeffs)

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


# take monomial and return aᵢ,i where c = aᵢ ⋅ xⁱ
_monomial(c::𝐿, x) = c == x ? (one(x), 1) : (c, 0)
function _monomial(c, x)

    @assert iscall(c)
    isconstant(c) && return (c, 0)

    if is_operation(*)(c)
        ps = _monomial.(arguments(c), x)
        aᵢ = reduce(*, first.(ps), init=one(x))
        i  = sum(last.(ps))

        return (aᵢ, i)
    elseif is_operation(^)(c)
        a, b = arguments(c) # b is symbolic integer
        u, v = _monomial(a,x) # v is integer
        return (u^(v*b), (b()^v))
    else
        error("$(operation(c)) ")
    end
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
