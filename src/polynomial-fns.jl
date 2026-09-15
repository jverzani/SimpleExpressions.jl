# co-pilot authored for the most part
#   kept ispolynomial code from before as it was more performant
# Polynomial analysis with a middle-ground approach:
# - keeps a single recursive pass
# - uses fixed-length coefficient vectors instead of sparse Dicts
# - avoids repeated tree scans and still minimizes allocations compared to
#   the sparse-dict prototype

## ---- helpers

# Dense coefficient vector representation for polynomial in x:
#   coeffs[k+1] is coefficient of x^k, with degree = length(coeffs)-1.
# The vector is kept as `Any[]` to accommodate symbolic coefficients.

_polynomial_zero_T(::Type{T}) where {T} = zero(T)
_polynomial_zero_T(::Any) = zero(typeof(0))

poly_degree(ex::Real, x) = 0

function _poly_coeffs(ex, x)
    !isvariable(x) && return nothing

    if is_number(ex) || !contains(ex, x)
        return Any[ex]
    end
    if isequal(ex, x)
        return Any[zero(ex), one(ex)]
    end
    if !iscall(ex)
        return nothing
    end

    op = operation(ex)
    args = arguments(ex)

    if op == +
        out = Any[zero(Int)]
        for a in args
            acoeff = _poly_coeffs(a, x)
            isnothing(acoeff) && return nothing
            out = _poly_add(out, acoeff)
        end
        return out
    elseif op == -
        if length(args) == 1
            acoeff = _poly_coeffs(args[1], x)
            isnothing(acoeff) && return nothing
            return _poly_neg(acoeff)
        end
        out = _poly_coeffs(args[1], x)
        for a in view(args, 2:length(args))
            acoeff = _poly_coeffs(a, x)
            isnothing(acoeff) && return nothing
            out = _poly_sub(out, acoeff)
        end
        return out
    elseif op == *
        out = Any[one(Int)]
        for a in args
            acoeff = _poly_coeffs(a, x)
            isnothing(acoeff) && return nothing
            out = _poly_mul(out, acoeff)
        end
        return out
    elseif op == /
        length(args) == 2 || return nothing
        a, b = args
        contains(b, x) && return nothing
        acoeff = _poly_coeffs(a, x)
        isnothing(acoeff) && return nothing
        return _poly_div_by_const(acoeff, b)
    elseif op == ^
        length(args) == 2 || return nothing
        a, b = args
        is_number(b) || return nothing
        n = unwrap_const(b)
        if !(n isa Integer)
            return nothing
        end
        n = Int(n)
        n < 0 && return nothing
        acoeff = _poly_coeffs(a, x)
        isnothing(acoeff) && return nothing
        return _poly_pow(acoeff, n)
    end

    return nothing
end

# Dense arithmetic helpers.

function _poly_trim(v)
    n = length(v)
    while n > 1 && iszero(v[n])
        n -= 1
    end
    resize!(v, n)
    return v
end

function _poly_add(a, b)
    n = max(length(a), length(b))
    out = Any[zero(Int) for _ in 1:n]
    for i in eachindex(a)
        out[i] = a[i]
    end
    for i in eachindex(b)
        out[i] += b[i]
    end
    _poly_trim(out)
end

function _poly_sub(a, b)
    n = max(length(a), length(b))
    out = Any[zero(Int) for _ in 1:n]
    for i in eachindex(a)
        out[i] = a[i]
    end
    for i in eachindex(b)
        out[i] -= b[i]
    end
    _poly_trim(out)
end

function _poly_neg(a)
    out = similar(a)
    for i in eachindex(a)
        out[i] = -a[i]
    end
    out
end

function _poly_mul(a, b)
    n = length(a) + length(b) - 1
    out = Any[zero(Int) for _ in 1:n]
    for i in 1:length(a), j in 1:length(b)
        out[i+j-1] += a[i] * b[j]
    end
    _poly_trim(out)
end

function _poly_div_by_const(a, b)
    out = similar(a)
    for i in eachindex(a)
        out[i] = a[i] / b
    end
    _poly_trim(out)
end

function _poly_pow(a, n::Int)
    out = Any[one(Int)]
    base = copy(a)
    while n > 0
        if n & 1 == 1
            out = _poly_mul(out, base)
        end
        n >>= 1
        if n > 0
            base = _poly_mul(base, base)
        end
    end
    out
end

# Exposed API.

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
#=
function ispolynomial(ex, x)::Bool
    !isvariable(x) && return false
    !isnothing(_poly_coeffs(ex, x))
end
=#

function poly_degree(ex, x)
    coeffs = _poly_coeffs(ex, x)
    isnothing(coeffs) && return nothing
    length(coeffs) - 1
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


function coefficients(ex, x)
    coeffs = _poly_coeffs(ex, x)
    isnothing(coeffs) && return nothing
    degree = length(coeffs) - 1
    nms = Tuple(SimpleExpressions._aᵢ(i) for i in 0:degree)
    NamedTuple{nms}(tuple(coeffs...))
end

if isdefined(@__MODULE__, :SymbolicEquation)
    coefficients(ex::SymbolicEquation, x) = coefficients(ex.lhs - ex.rhs, x)
end
