# co-pilot authored for the most part
#   kept ispolynomial code from before as it was more performant
# Polynomial analysis with a middle-ground approach:
# - keeps a single recursive pass
# - uses fixed-length coefficient vectors instead of sparse Dicts
# - avoids repeated tree scans and still minimizes allocations compared to
#   the sparse-dict prototype

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

function polynomial_degree(ex, x)
    coeffs = _poly_coeffs(ex, x)
    isnothing(coeffs) && return nothing
    length(coeffs) - 1
end

polynomial_degree(ex::Real, x) = 0


function polynomial_coefficients(ex::AbstractSymbolic, x)
    coeffs = _poly_coeffs(ex, x)
    isnothing(coeffs) && return nothing
    degree = length(coeffs) - 1
    nms = Tuple(SimpleExpressions._aᵢ(i) for i in 0:degree)
    NamedTuple{nms}(tuple(coeffs...))
end

polynomial_coefficients(ex::SymbolicEquation, x) = polynomial_coefficients(ex.lhs - ex.rhs, x)


## ---- helpers

# Dense coefficient vector representation for polynomial in x:
#   coeffs[k+1] is coefficient of x^k, with degree = length(coeffs)-1.
# The vector is kept as `Any[]` to accommodate symbolic coefficients.


function _poly_coeffs(ex, x)
    isvariable(x) || return nothing
    nd = _ratfun(ex, e -> is_number(e) || !contains(e, x),
                 e -> isequal(e, x) ? (Any[zero(e), one(e)], Any[1]) : nothing,
                 _dense_algebra)
    isnothing(nd) && return nothing
    n, d = nd
    length(d) == 1 || return nothing # x in a denominator
    isone(only(d)) ? n : _poly_div_by_const(n, only(d))
end

## ---- rational functions
# (numerator, denominator) of ex as polynomials in the atoms recognized by
# `atom(ex)` (the pair for an atom, else `nothing`), using the arithmetic
# A = (constant, add, mul, pow) of the representation: dense coefficient
# vectors for one variable (_dense_algebra), or others (e.g. the sparse
# polynomials in sin and cos of solve.jl). Subexpressions with `isconst(ex)`
# are coefficients. `nothing` if ex isn't a rational function of the atoms.
_ratfun(ex, isconst, atom, A) = _ratfun(ex, isconst, atom, A, A.constant(1))
function _ratfun(ex, isconst, atom, A, 𝟙)
    isconst(ex) && return (A.constant(ex), 𝟙)
    nd = atom(ex)
    isnothing(nd) || return nd
    iscall(ex) || return nothing
    op, args = operation(ex), arguments(ex)
    if op == (-) && length(args) == 1
        nd = _ratfun(only(args), isconst, atom, A, 𝟙)
        return isnothing(nd) ? nothing : (A.neg(first(nd)), last(nd))
    elseif op ∈ (+, *, -)
        n, d = A.constant(op == (*) ? 1 : 0), 𝟙
        for (i, a) ∈ enumerate(args)
            nd = _ratfun(a, isconst, atom, A, 𝟙)
            isnothing(nd) && return nothing
            n′, d′ = nd
            op == (-) && i > 1 && (n′ = A.neg(n′))
            if op == (*)
                n, d = A.mul(n, n′), _rf_mul(A, 𝟙, d, d′)
            elseif isequal(d, d′)
                n = A.add(n, n′)
            else
                n, d = A.add(_rf_mul(A, 𝟙, n, d′), _rf_mul(A, 𝟙, n′, d)), _rf_mul(A, 𝟙, d, d′)
            end
        end
        return (n, d)
    elseif op ∈ (/, //) && length(args) == 2
        nd = _ratfun(args[1], isconst, atom, A, 𝟙)
        nd′ = _ratfun(args[2], isconst, atom, A, 𝟙)
        (isnothing(nd) || isnothing(nd′)) && return nothing
        return (_rf_mul(A, 𝟙, first(nd), last(nd′)), _rf_mul(A, 𝟙, last(nd), first(nd′)))
    elseif op == (^) && length(args) == 2
        a, b = args
        is_number(b) || return nothing
        k = unwrap_const(b)
        (k isa Real && isinteger(k)) || return nothing
        k = Int(k)
        nd = _ratfun(a, isconst, atom, A, 𝟙)
        isnothing(nd) && return nothing
        n, d = k ≥ 0 ? nd : reverse(nd)
        return (A.pow(n, abs(k)), isequal(d, 𝟙) ? d : A.pow(d, abs(k)))
    end
    nothing
end
# a⋅b, skipping the common case of a factor 1 (most denominators are 1)
_rf_mul(A, 𝟙, a, b) = isequal(a, 𝟙) ? b : isequal(b, 𝟙) ? a : A.mul(a, b)

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

function _aᵢ(i)
    aᵢs = ("₀","₁","₂","₃","₄","₅","₆","₇","₈","₉")
    io = IOBuffer()
    print(io, "a")
    for j in Iterators.reverse(digits(i))
        print(io, aᵢs[1 + j])
    end
    Symbol(take!(io))
end

# arithmetic of dense coefficient vectors, for _ratfun
const _dense_algebra = (constant = c -> Any[c], add = _poly_add, mul = _poly_mul, pow = _poly_pow, neg = _poly_neg)
