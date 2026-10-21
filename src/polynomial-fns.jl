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
    frac = _ratfun(ex, e -> is_number(e) || !contains(e, x),
                 e -> isequal(e, x) ? (Any[zero(e), one(e)], Any[1]) : nothing,
                 _dense_algebra)
    isnothing(frac) && return nothing
    n, d = frac
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
    frac = atom(ex)
    isnothing(frac) || return frac
    iscall(ex) || return nothing
    op, args = operation(ex), arguments(ex)
    if op == (-) && length(args) == 1
        frac = _ratfun(only(args), isconst, atom, A, 𝟙)
        return isnothing(frac) ? nothing : (A.neg(first(frac)), last(frac))
    elseif op ∈ (+, *, -)
        n, d = A.constant(op == (*) ? 1 : 0), 𝟙
        for (i, a) ∈ enumerate(args)
            frac = _ratfun(a, isconst, atom, A, 𝟙)
            isnothing(frac) && return nothing
            n′, d′ = frac
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
        frac = _ratfun(args[1], isconst, atom, A, 𝟙)
        frac′ = _ratfun(args[2], isconst, atom, A, 𝟙)
        (isnothing(frac) || isnothing(frac′)) && return nothing
        return (_rf_mul(A, 𝟙, first(frac), last(frac′)), _rf_mul(A, 𝟙, last(frac), first(frac′)))
    elseif op == (^) && length(args) == 2
        a, b = args
        is_number(b) || return nothing
        k = unwrap_const(b)
        (k isa Real && isinteger(k)) || return nothing
        k = Int(k)
        frac = _ratfun(a, isconst, atom, A, 𝟙)
        isnothing(frac) && return nothing
        n, d = k ≥ 0 ? frac : reverse(frac)
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

## ---- roots
# Real roots of polynomials with coefficient vectors, as exact values where
# possible (see exact.jl): rational roots, the quadratic, cubic and quartic
# formulas, polynomials in xᵏ; and polynomial algebra used by the solver.

# solve a₀ + a₁⋅x + ⋯ + aₙ⋅xⁿ ~ 0, n ≥ 1; `nothing` if not possible
function _poly_roots(cs)
    cs = map(_fold, cs)
    # factor out xᵏ
    k = findfirst(!_iszero, cs) - 1
    k > 0 && return _unique_roots([_fold(0)], _poly_roots(cs[k+1:end]))

    n = length(cs) - 1
    n == 0 && return []
    if n == 1
        a₀, a₁ = cs
        return [_expand_poly(_fold(-a₀ / a₁))]
    end
    ρs = _rational_roots(cs)
    !isnothing(ρs) && return ρs
    n == 2 && return _quadratic_roots(cs)

    # polynomial in xᵍ
    g = gcd([i - 1 for i ∈ eachindex(cs) if !_iszero(cs[i])])
    if g > 1
        ys = _poly_roots(cs[1:g:end])
        isnothing(ys) && return nothing
        return _unique_roots((_nth_roots(y, g) for y ∈ ys)...)
    end

    n == 3 && return _cubic_roots(cs)
    n == 4 && return _quartic_roots(cs)
    nothing
end

# real solutions of xᵍ = y
function _nth_roots(y, g)
    ρ = _root(y, g)
    isodd(g) && return [ρ]
    _maybe_nonneg(y) || return []
    iszero(ρ) ? [ρ] : [_fold(-ρ), _fold(ρ)]
end

# the roots in vectors rss, without duplicates; `nothing`s are skipped
function _unique_roots(rss...)
    out = Any[]
    for rs ∈ rss
        isnothing(rs) && continue
        for ρ ∈ rs
            any(σ -> isequal(σ, ρ), out) || push!(out, ρ)
        end
    end
    out
end

# numeric coefficients, as exact rationals when possible
function _num_coeffs(cs)
    vs = map(_value, cs)
    all(v -> !isnothing(v) && isreal(v), vs) || return nothing
    all(_isexact, vs) ? Rational{BigInt}.(vs) : float.(real.(vs))
end

# a₀ + a₁x + a₂x² + a₃x³ ~ 0, numeric coefficients. With x = t - a₂/(3a₃),
# t³ + p⋅t + q = 0: Cardano's formula when there is one real root, the
# trigonometric form when there are three
function _cubic_roots(cs)
    vs = _num_coeffs(cs)
    isnothing(vs) && return nothing
    a₀, a₁, a₂, a₃ = vs
    s = a₂ / (3a₃)
    p = (3a₃ * a₁ - a₂^2) / (3a₃^2)
    q = (2a₂^3 - 9a₃ * a₂ * a₁ + 27a₃^2 * a₀) / (27a₃^3)
    Δ = q^2 / 4 + p^3 / 27
    shift(t) = _pi_fold(_fold(t - _fold(s)))
    iszero(p) && iszero(q) && return [shift(0)]
    if iszero(Δ) # a double root
        return _unique_roots([shift(_fold(3q / p))], [shift(_fold(-3q / (2p)))])
    elseif Δ > 0
        sΔ = _root(_fold(Δ), 2)
        u, v = _fold(_fold(-q / 2) + sΔ), _fold(_fold(-q / 2) - sΔ)
        return [shift(_fold(_root(u, 3) + _root(v, 3)))]
    end
    m = _fold(2 * _root(_fold(-p / 3), 2))
    θ = _fold(_apply(acos, _fold(_fold(3q / (2p)) * _root(_fold(-3 / p), 2))) / 3)
    [shift(_fold(m * _apply(cos, _pi_fold(_fold(θ - 2k * _constant(π) / 3))))) for k ∈ 0:2]
end

# a₀ + a₁x + ⋯ + a₄x⁴ ~ 0, numeric coefficients, by Ferrari's method: with
# x = y - b/4, y⁴ + p⋅y² + q⋅y + r = 0 factors into two quadratics using a
# positive root m of the resolvent 8m³ + 8p⋅m² + (2p² - 8r)⋅m - q² = 0
function _quartic_roots(cs)
    vs = _num_coeffs(cs)
    isnothing(vs) && return nothing
    e, d, c, b = vs[1:4] ./ vs[5]
    p = c - 3b^2 / 8
    q = d - b * c / 2 + b^3 / 8
    r = e - b * d / 4 + b^2 * c / 16 - 3b^4 / 256
    shift(ρs) = isnothing(ρs) ? nothing : [_fold(ρ - _fold(b / 4)) for ρ ∈ ρs]
    iszero(q) && return shift(_poly_roots(Any[_fold(r), 0, _fold(p), 0, 1]))
    ms = _poly_roots(Any[_fold(-q^2), _fold(2p^2 - 8r), _fold(8p), 8])
    isnothing(ms) && return nothing
    ms = [m for m ∈ ms if (v = _value(m); isreal(v) && real(v) > 0)]
    isempty(ms) && return nothing
    m = argmin(m -> length(string(m)), ms) # the simplest
    s₂ = _root(_fold(2m), 2)
    _unique_roots(shift(_quadratic_roots(Any[_fold(_fold(p / 2) + m - _fold(q / 2) / s₂), s₂, 1])),
                  shift(_quadratic_roots(Any[_fold(_fold(p / 2) + m + _fold(q / 2) / s₂), _fold(-s₂), 1])))
end

function _quadratic_roots(cs)
    a₀, a₁, a₂ = map(_fold, cs)
    Δ = _fold(_pow_int(a₁, 2) - 4 * a₂ * a₀)
    v = _value(Δ)
    if !isnothing(v)
        v < 0 && return []
        iszero(v) && return [_fold(-a₁ / (2a₂))]
    end
    c = _value(2a₂)
    if _isexact(v) && _isexact(c) # p ± k⋅√m exactly
        p = _fold(-a₁ / (2a₂))
        k, m = _surd(v, 2)
        isone(m) && return [_fold(p - k // c), _fold(p + k // c)]
        q = _fold(k // c) * sqrt(_constant(m))
        iszero(p) && return [-q, q]
        return [p - q, p + q]
    end
    Δ = _expand_poly(Δ)
    sΔ = _sqrt_poly(Δ)
    sΔ = isnothing(sΔ) ? _root(Δ, 2) : sΔ
    [_expand_poly(_fold((-a₁ - sΔ) / (2a₂))), _expand_poly(_fold((-a₁ + sΔ) / (2a₂)))]
end

# rational root theorem for numeric, rational coefficients
function _rational_roots(cs)
    vs = map(_value, cs)
    all(v -> v isa Union{Integer, Rational} || (v isa AbstractFloat && isinteger(v)), vs) ||
        return nothing
    qs = Rational{BigInt}.(vs)
    qs = qs .* lcm(denominator.(qs))  # integer coefficients
    roots = Rational{BigInt}[]
    while length(qs) > 1
        iszero(first(qs)) && (push!(roots, 0); qs = qs[2:end]; continue)
        a₀, aₙ = numerator(first(qs)), numerator(last(qs))
        max(abs(a₀), abs(aₙ)) > 10^10 && return nothing # too many candidates
        cands = _candidates(a₀, aₙ)
        i = findfirst(c -> iszero(_horner(qs, c)), cands)
        isnothing(i) && break
        push!(roots, cands[i])
        qs = _deflate(qs, cands[i])
    end
    length(qs) > 5 && return nothing
    isempty(roots) && return nothing  # leave to the formulas
    ρs = [_fold(c) for c ∈ sort!(roots)]
    length(qs) < 3 && return _unique_roots(ρs)
    f = (_quadratic_roots, _cubic_roots, _quartic_roots)[length(qs) - 2]
    _unique_roots(ρs, f(map(_fold, qs)))
end

_horner(qs, c) = foldr((q, acc) -> acc * c + q, qs; init = zero(c))

# divide by (x - c)
function _deflate(qs, c)
    n = length(qs) - 1
    out = similar(qs, n)
    acc = zero(c)
    for i ∈ n:-1:1
        acc = acc * c + qs[i+1]
        out[i] = acc
    end
    out
end

function _candidates(a₀, aₙ)
    ps, qs = _divisors(a₀), _divisors(aₙ)
    unique!([s * p // q for p ∈ ps for q ∈ qs for s ∈ (1, -1)])
end

function _divisors(n)
    n = abs(n)
    ds = BigInt[]
    for d ∈ 1:isqrt(n)
        if iszero(n % d)
            push!(ds, d)
            d != n ÷ d && push!(ds, n ÷ d)
        end
    end
    ds
end

# coefficients folded, trailing zeros dropped
function _trim(cs)
    cs = map(_fold, cs)
    while length(cs) > 1 && _iszero(last(cs))
        pop!(cs)
    end
    cs
end

# expand an expression polynomial in its symbols, folding numbers
function _expand_poly(u)
    u isa Number && return _fold(u)
    syms = _symbols(u)
    isempty(syms) && return _fold(u)
    s = first(syms)
    cs = _poly_coeffs(u, s)
    isnothing(cs) && return u
    terms = [(isone(i) ? _expand_poly(c) : _expand_poly(c) * s^(i - 1)) for (i, c) ∈ enumerate(cs) if !_iszero(_expand_poly(c))]
    isempty(terms) && return _fold(0)
    _fold(sum(terms))
end

# √u when u is the square of a polynomial in one symbol with rational coefficients
function _sqrt_poly(u)
    u isa Number && return nothing
    syms = _symbols(u)
    length(syms) == 1 || return nothing
    s = only(syms)
    cs = _poly_coeffs(u, s)
    isnothing(cs) && return nothing
    P = _exact_coeffs(cs)
    (isnothing(P) || iseven(length(P))) && return nothing
    m = (length(P) - 1) ÷ 2
    k, rest = _surd(P[end], 2)
    (isone(rest) && P[end] > 0) || return nothing
    Q = zeros(Rational{BigInt}, m + 1)
    Q[m+1] = k
    for j ∈ 1:m # coefficient of s^(m-j)
        d = 2m - j + 1
        acc = P[d] - sum(Q[a] * Q[d - a + 1] for a ∈ (m - j + 2):(m + 1) if 1 ≤ d - a + 1 ≤ m + 1 && d - a + 1 > m - j + 1; init = 0)
        Q[m - j + 1] = acc / (2Q[m+1])
    end
    _poly_mul(Any[Q...], Any[Q...]) == Any[P...] || return nothing
    _expand_poly(_poly_expr(Q, s))
end

_exact_coeffs(P) = (vs = map(_value, P); all(_isexact, vs) ? Rational{BigInt}.(vs) : nothing)

_poly_expr(q, x) = sum(_fold(Rational{Int}(c)) * x^(i - 1) for (i, c) ∈ enumerate(q) if !iszero(c); init = zero(x))

# quotient and remainder of a ÷ b; coefficients in increasing degree
function _poly_divrem(a, b)
    a = copy(a)
    n, m = length(a), length(b)
    n < m && return ([zero(eltype(a))], a)
    q = zeros(eltype(a), n - m + 1)
    for i ∈ (n - m + 1):-1:1
        c = a[i + m - 1] / b[m]
        q[i] = c
        for j ∈ 1:m
            a[i + j - 1] -= c * b[j]
        end
    end
    r = a[1:m-1]
    while length(r) > 1 && iszero(last(r)); pop!(r); end
    (q, isempty(r) ? [zero(eltype(a))] : r)
end

# monic greatest common divisor
function _poly_gcd(a, b)
    while !(length(b) == 1 && iszero(only(b)))
        a, b = b, last(_poly_divrem(a, b))
    end
    a ./ last(a)
end
