# Exact numbers for the solver (solve.jl) and the polynomial code
# (polynomial-fns.jl): numbers are kept as wrapped constants so arithmetic stays
# exact; roots and special values (sqrt(8) = 2sqrt(2), asin(1/2) = pi/6,
# log(8)/log(2) = 3, ...) are found exactly when possible. Uses the hooks of
# solve_interface.jl.

# k⋅log_b(u) ↦ (k, u, b), for log, log10, log2 and log(b, u) = log(u)/log(b);
# else `nothing`
_log_base(::typeof(log)) = ℯ
_log_base(::typeof(log10)) = 10
_log_base(::typeof(log2)) = 2
_log_base(::Any) = nothing

function _log_term(t; numeric = false)
    iscall(t) || return nothing
    !numeric && _isnumeric(t) && return nothing
    b = _log_base(operation(t))
    !isnothing(b) && return (_fold(1), only(arguments(t)), b)
    operation(t) == (*) || return nothing
    k, u, b = 1, nothing, ℯ
    for f ∈ arguments(t)
        if (numeric || !_isnumeric(f)) && iscall(f) && !isnothing(_log_base(operation(f)))
            isnothing(u) || return nothing
            u, b′ = only(arguments(f)), _log_base(operation(f))
            b′ == ℯ || (b == ℯ ? (b = b′) : return nothing)
        elseif _isnumeric(f) && iscall(f) && operation(f) == (^) && _value(last(arguments(f))) == -1 &&
               iscall(first(arguments(f))) && operation(first(arguments(f))) == log # 1/log(b)
            b == ℯ || return nothing
            b = _value(only(arguments(first(arguments(f)))))
        elseif _isnumeric(f)
            k = k * f
        else
            return nothing
        end
    end
    isnothing(u) ? nothing : (_fold(k), u, b)
end

# bˢ, exact when possible
function _exp_base(b, s)
    b == ℯ && return _apply(exp, s)
    v = _value(s)
    _isexact(v) && return isinteger(v) ? _fold(Rational(b)^Int(v)) : _rpow(b, v)
    _fold(_fold(b)^s)
end

# log(r) / log(a), an integer when r is a power of a
function _log_ratio(r, a)
    a == ℯ && return _apply(log, r)
    v, w = _value(r), _value(a)
    if _isexact(v) && _isexact(w) && v > 0 && w > 0 && !isone(w)
        k = round(Int, log(v) / log(w))
        Rational{BigInt}(w)^k == v && return _fold(k)
    end
    _fold(_apply(log, r) / _apply(log, a))
end

# asin, acos, atan at the standard values, as a multiple of π: angles
# k⋅π/12 and k⋅π/8 in [0, π/2]
const _std_angles = (0//1, 1//12, 1//8, 1//6, 1//4, 1//3, 3//8, 5//12, 1//2)

const _sin_tbl = Tuple((sin(k * π), k) for k ∈ _std_angles)

const _tan_tbl = Tuple((tan(k * π), k) for k ∈ _std_angles if k < 1//2)

function _special_angle(f, v)
    f ∈ (asin, acos, atan) && isreal(v) || return nothing
    tbl = f == atan ? _tan_tbl : _sin_tbl
    i = findfirst(t -> isapprox(first(t), abs(v); atol = 1e-14), tbl)
    isnothing(i) && return nothing
    k = last(tbl[i])
    if f == acos # acos(v) = π/2 - asin(v)
        k = 1//2 - k
        return v < 0 ? 1 - k : k
    end
    v < 0 ? -k : k
end

# rⁿ, undoing a square or cube root
function _pow_int(r, n)
    if iscall(r)
        op, args = operation(r), arguments(r)
        op == sqrt && iseven(n) && return _fold(only(args)^(n ÷ 2))
        op == cbrt && iszero(n % 3) && return _fold(only(args)^(n ÷ 3))
        if op == (*) && length(args) == 2 && _isnumeric(first(args))
            return _fold(_pow_int(first(args), n) * _pow_int(last(args), n))
        end
    end
    _fold(_fold(r)^n)
end

# replace atoms by their substitutes
function _subst(ex, atoms, subs, x)
    _has(ex, x) || return ex
    i = findfirst(a -> isequal(a, ex), atoms)
    !isnothing(i) && return subs[i]
    iscall(ex) || return ex
    _rebuild(ex, operation(ex), map(a -> _subst(a, atoms, subs, x), arguments(ex)))
end

# c₀ + c₁⋅π for a combination of rationals and π; else u
const _𝓅 = _fresh(:𝓅)

function _pi_fold(u)
    u isa Number && return u
    (_isnumeric(u) && !_isconstantleaf(u)) || return u
    π′ = _constant(π)
    u′ = _subst(u, [π′], [_𝓅], π′)
    _has(u′, _𝓅) || return u
    cs = _poly_coeffs(u′, _𝓅)
    (isnothing(cs) || length(cs) != 2 || !all(_isexact ∘ _value, cs)) && return u
    _fold(_fold(cs[1]) + _fold(cs[2]) * π′)
end

# r^(1/n), kept exact: a rational when possible, else k⋅ⁿ√m
function _root(r, n)
    v = _value(r)
    if _isexact(v)
        iszero(v) && return 0
        v < 0 && isodd(n) && return _fold(-_root(-v, n))
        k, m = _surd(v, n)
        isone(m) && return _fold(k)
        rad = _radical(m, n)
        !isnothing(rad) && return isone(k) ? rad : _fold(k) * rad
    end
    if n == 2 && isnothing(v) # used for ±√r, so a polynomial square root will do
        q = _sqrt_poly(_expand_poly(r))
        !isnothing(q) && return q
    end
    r = _fold(r)
    _fold(n == 2 ? sqrt(r) : n == 3 ? cbrt(r) : r^(1//n))
end

# write v ≥ 0 as kⁿ⋅m with k rational, m an integer free of nth powers
function _surd(v, n)
    p, q = numerator(v), denominator(v)
    N = big(p) * big(q)^(n - 1) # v = N / qⁿ
    k = big(1)
    d = 2
    while d^n <= N && d <= 10^5
        while iszero(N % d^n)
            N ÷= d^n
            k *= d
        end
        d += 1
    end
    (Rational{Int}(k // q), Int(N))
end

# symbolic ⁿ√m using square and cube roots; `nothing` if not possible
function _radical(m, n)
    n == 1 && return _constant(m)
    iszero(n % 2) && (r = _radical(m, n ÷ 2); return isnothing(r) ? r : sqrt(r))
    iszero(n % 3) && (r = _radical(m, n ÷ 3); return isnothing(r) ? r : cbrt(r))
    nothing
end

# rᵉ for a rational e, exact when possible
function _rpow(r, e)
    v = _value(r)
    p, q = numerator(e), denominator(e)
    if _isexact(v) && !iszero(v) && (v > 0 || isodd(q))
        return _root(_fold(Rational{BigInt}(v)^p), q)
    end
    _fold(_fold(r)^e)
end

# Numbers are kept as exact constants: wrapped, so arithmetic on them stays
# exact (1/2 is 1//2, never 0.5). _fold wraps a number (reducing rationals,
# narrowing BigInts); evaluates a numeric expression which already involves
# floating point values; and keeps exact values such as `sqrt(2)`, `π/6` or
# `log(2)` symbolic. Numeric decisions use the plain value, `_value(u)`.
_fold(u::Number) = _constant(_narrow(u))

function _fold(u)
    _isnumeric(u) || return u
    _isconstantleaf(u) && return _fold(_numval(u))
    _has_float(u) ? _fold(_numval(u)) : u
end

_narrow(u::Rational) = isone(denominator(u)) ? _narrow(numerator(u)) :
    (_fits(numerator(u)) && _fits(denominator(u)) ? Rational{Int}(u) : u)

_narrow(u::Integer) = _fits(u) ? Int(u) : u
_narrow(u) = u

_fits(n) = typemin(Int) ≤ n ≤ typemax(Int)

_has_float(u) = _isconstantleaf(u) ? _numval(u) isa AbstractFloat :
    iscall(u) && any(_has_float, arguments(u))

_isexact(v) = v isa Union{Integer, Rational}

# value of a number, or of a numeric expression
_numval(u::Number) = u
_numval(u) = _evaluate(u)

_value(u::Number) = u
_value(u) = _isnumeric(u) ? _numval(u) : nothing

_iszero(u) = (v = _value(u); !isnothing(v) && iszero(v))

# can be ruled out as negative?
_maybe_nonneg(u) = (v = _value(u); isnothing(v) || (isreal(v) && real(v) ≥ 0))

# can f be applied to u? (false only if u is numeric and outside f's domain)
function _domain_ok(f, u)
    v = _value(u)
    isnothing(v) && return true
    try
        f(v); true
    catch err
        err isa DomainError || rethrow()
        false
    end
end

_domain_ok(::typeof(log), u) = (v = _value(u); isnothing(v) || (isreal(v) && real(v) > 0))

# f(r), exact when possible
function _apply(f, r)
    r = _fold(r)
    _has_float(r) && return _fold(f(_value(r)))
    if f == exp # exp(k⋅log(w)) = wᵏ
        lt = _log_term(r; numeric = true)
        if !isnothing(lt) && lt[3] == ℯ && _isexact(_value(lt[1]))
            k, w = _value(lt[1]), lt[2]
            isone(k) && return w
            _isnumeric(w) && return _rpow(w, k)
            isinteger(k) && return w^Int(k)
        end
    end
    v = _value(r)
    if !isnothing(v)
        f == log && (isone(v) || v == ℯ) && return _fold(isone(v) ? 0 : 1)
        if iszero(v)
            y = f(0.0)
            (iszero(y) || isone(y)) && return _fold(Int(y))
        end
        k = _special_angle(f, v)
        !isnothing(k) && return _fold(k * _constant(π))
        if f ∈ (asind, acosd, atand) # in degrees
            k = _special_angle(f == asind ? asin : f == acosd ? acos : atan, v)
            !isnothing(k) && return _fold(180k)
        end
    end
    _fold(f(r))
end
