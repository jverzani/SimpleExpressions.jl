# Solve equations symbolically. Generic: expressions are walked with
# TermInterface (iscall, operation, arguments) and built with ordinary
# operators; everything else goes through the hooks in solve_interface.jl.

## --------
## "Solve" an equation
## Strategy:
## * peel off layers of `lhs - rhs` top-down: move terms (or factors)
##   free of `x` to the other side and apply inverse functions
## * when both sides share an outer injective function (or are exponentials),
##   strip it from both sides
## * when stuck, if what remains is a rational function in `x`, solve its
##   numerator; or substitute `y = g(x)` when it is a polynomial in `g(x)`
## * numeric roots of equations with no parameters are checked in the
##   original equation, discarding extraneous ones
"""
    solve_equation(l, r, x; conditions=false)

Solve `l ~ r` for `x`. This is the generic solver behind `solve`; see its
docstring for what is solved and what is returned. It walks expressions with
TermInterface, builds them with ordinary operators, and uses the hooks of
`solve_interface.jl` for everything else.
"""
function solve_equation(l, r, x; conditions::Bool = false)
    sols = _solve(l, r, x)
    isnothing(sols) && return nothing
    sols = _verify(sols, l, r, x)
    out = Tuple((e = _equation(s.lhs, s.rhs); conditions ? e => c : e) for (s, c) ∈ sols)
    length(out) == 1 ? only(out) : out
end

# A solution lhs ~ rhs (solved when lhs is x), valid under conditions on x;
# a condition is (ex, kind), kind one of :pos, :nonneg, :nonzero
struct _Sol
    lhs
    rhs
    conds::Vector{Any}
    # numbers are wrapped as constants so later arithmetic stays exact
    _Sol(l, r, conds) = new(l, r isa Number ? _constant(r) : r, conds)
end
_Sol(l, r) = _Sol(l, r, Any[])
_tag(sols, cond) = isnothing(sols) ? sols : Tuple(_Sol(s.lhs, s.rhs, Any[s.conds..., cond]) for s ∈ sols)

function _solve(l, r, x)
    if _has(l, x) && _has(r, x)
        m = _match_sides(l, r, x)
        !isnothing(m) && return _solve(m..., x)
        l, r = l - r, zero(x)
    elseif _has(r, x)
        l, r = r, l
    end
    _has(l, x) || return nothing
    _isolate(l, r, x)
end

# Check solutions against l ~ r, returning (equation, conditions) pairs.
# Without parameters numeric solutions are checked directly. With parameters
# the conditions are evaluated at the solution; those which are numeric are
# decided, the others are sampled over the parameters: conditions holding at
# every sample are dropped, as are solutions valid at no sample.
function _verify(sols, l, r, x)
    ps = Tuple(s for s ∈ _symbols(l - r) if !isequal(s, x))
    dom = _domain!(Any[], l - r, nothing)
    out = Any[]
    for s ∈ sols
        e = s
        if !isequal(e.lhs, x)
            push!(out, (e, ()))
        elseif isempty(ps)
            _isnumeric(e.rhs) && !_satisfies(l, r, x, _numval(e.rhs)) && continue
            push!(out, (e, ()))
        else
            ρ = e.rhs
            cs = Any[(_expand_poly(_subst(c, [x], [ρ], x)), k) for (c, k) ∈ vcat(dom, s.conds)]
            append!(cs, _domain!(Any[], ρ, nothing))
            cs = _decide(cs, ps)
            isnothing(cs) && continue
            _valid_somewhere(l, r, x, ρ, cs, ps) || continue
            conds = Any[]
            for c ∈ cs
                c′ = _cond_expr(c)
                any(d -> isequal(d, c′), conds) || push!(conds, c′)
            end
            push!(out, (e, Tuple(conds)))
        end
    end
    # sort when all are numeric
    all(((e, _),) -> isequal(e.lhs, x) && _isnumeric(e.rhs), out) &&
        sort!(out, by = ((e, _),) -> _numval(e.rhs))
    Tuple(out)
end

# conditions for the expressions in ex to be defined; with x == nothing,
# those for any non-numeric subexpression
function _domain!(out, ex, x)
    iscall(ex) || return out
    op, args = operation(ex), arguments(ex)
    has(w) = isnothing(x) ? !_isnumeric(w) : _has(w, x)
    if length(args) == 1 && has(only(args))
        w = only(args)
        op ∈ (log, log10, log2) && push!(out, (w, :pos))
        op == sqrt && push!(out, (w, :nonneg))
        op == acosh && push!(out, (w - 1, :nonneg))
        op ∈ (asin, acos) && push!(out, (1 - w^2, :nonneg))
    elseif op == (^) && has(first(args))
        b, n = args
        v = _value(n)
        if !isnothing(v)
            v < 0 && push!(out, (b, :nonzero))
            v isa Rational && iseven(denominator(v)) && push!(out, (b, :nonneg))
        end
    end
    foreach(a -> _domain!(out, a, x), args)
    out
end

# Decide conditions (ex, kind) which are numeric, or which hold at every
# sample where they are defined (where they are not is covered by the
# conditions for the solution itself); `nothing` if one never holds
function _decide(cs, ps)
    out = Any[]
    for (c, k) ∈ cs
        any(((c′, k′),) -> isequal(c′, c) && k′ == k, out) && continue
        if _isnumeric(c) || c isa Number
            _holds(_value(c), k) || return nothing
            continue
        end
        vs = filter(!isnothing, [_at(c, ps, i) for i ∈ 1:_nsamples])
        hs = map(v -> _holds(v, k), vs)
        any(hs) || return nothing
        all(hs) && continue
        push!(out, (c, k))
    end
    out
end
_holds(::Nothing, k) = false
_holds(v, k) = isreal(v) && !isnan(v) &&
    (k == :pos ? real(v) > 0 : k == :nonneg ? real(v) ≥ -1e-12 : abs(v) > 1e-12)

# is there a sample where all conditions hold and l ~ r is satisfied?
function _valid_somewhere(l, r, x, ρ, cs, ps)
    for i ∈ 1:_nsamples
        all(((c, k),) -> _holds(_at(c, ps, i), k), cs) || continue
        v = _at(ρ, ps, i)
        (isnothing(v) || !isreal(v)) && continue
        lv, rv = _at(l, ps, i, x => real(v)), _at(r, ps, i, x => real(v))
        (isnothing(lv) || isnothing(rv)) && continue
        isfinite(lv) && isfinite(rv) && isapprox(lv, rv; atol = 1e-8, rtol = 1e-8) && return true
    end
    false
end

# sample values for the parameters: 0 and ±10ᵏ over k ∈ -3:1/8:3/2 (larger
# values underflow exp(-p)); several parameters take values from the list
# with different strides
const _samples = vcat(0.0, [s * 10.0^k for k ∈ -3:1/8:3/2 for s ∈ (1, -1)])
const _nsamples = length(_samples)
const _strides = (1, 7, 11, 13, 17, 19, 23)
function _at(ex, ps, i, extra...)
    prs = (p => _samples[mod1(i * _strides[mod1(j, length(_strides))], _nsamples)]
           for (j, p) ∈ enumerate(ps))
    _value_at(ex, prs..., extra...)
end

# (ex, kind) as a symbolic comparison; a linear condition is solved for its symbol
function _cond_expr((c, k))
    op = k == :pos ? (>) : k == :nonneg ? (≥) : (!=)
    syms = _symbols(c)
    if length(syms) == 1
        cs = _poly_coeffs(c, only(syms))
        vs = isnothing(cs) ? nothing : map(_value, cs)
        if !isnothing(vs) && length(vs) == 2 && all(_isexact, vs)
            b, a = vs
            op′ = a < 0 && op != (!=) ? (op == (>) ? (<) : (≤)) : op
            return _comparison(op′, only(syms), _constant(_fold(-b // a)))
        end
    end
    _comparison(op, c, _constant(0))
end

# expand an expression polynomial in its symbols, folding numbers
function _expand_poly(u)
    u isa Number && return u
    _isnumeric(u) && return _fold(u)
    syms = _symbols(u)
    isempty(syms) && return _fold(u)
    s = first(syms)
    cs = _poly_coeffs(u, s)
    isnothing(cs) && return u
    terms = [(isone(i) ? _expand_poly(c) : _expand_poly(c) * s^(i - 1)) for (i, c) ∈ enumerate(cs) if !_iszero(_expand_poly(c))]
    isempty(terms) && return 0
    _fold(sum(terms))
end

# √u when u is the square of a polynomial in one symbol with rational coefficients
function _sqrt_poly(u)
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

function _satisfies(l, r, x, v)
    lv, rv = _value_at(l, x => v), _value_at(r, x => v)
    (isnothing(lv) || isnothing(rv)) && return false
    isfinite(lv) && isfinite(rv) && isapprox(lv, rv; atol = 1e-8, rtol = 1e-8)
end

# numeric value of ex with symbols replaced (pairs); `nothing` if undefined
function _value_at(ex, prs...)
    v = try
        ex isa Number ? ex : _substitute_values(ex, prs...)
    catch err
        err isa DomainError || err isa DivideError || rethrow()
        return nothing
    end
    v isa Number && return v
    _isnumeric(v) || return nothing
    try
        _numval(v)
    catch err
        err isa DomainError || err isa DivideError || rethrow()
        nothing
    end
end

# injective functions; f(u) ~ f(v) ⟹ u ~ v
const _injective = (exp, log, sqrt, cbrt, sinh, asinh, tanh, atanh,
                    asin, acos, atan)

# simplify l ~ r, both containing x, by removing a common outer function.
# Returns (l′, r′) or `nothing`
function _match_sides(l, r, x)
    if iscall(l) && iscall(r) && operation(l) == operation(r)
        op = operation(l)
        op ∈ _injective && return (only(arguments(l)), only(arguments(r)))
        if op == (^)
            a, b = arguments(l)
            c, d = arguments(r)
            isequal(a, c) && !_has(a, x) && return (b, d) # aᵘ ~ aᵛ
        end
    end
    L, R = _explog(l, x), _explog(r, x)
    (isnothing(L) || isnothing(R)) && return nothing
    (L, R)
end

# log(t) when t is a product of constants and exponentials, else `nothing`
function _explog(t, x)
    fs = iscall(t) && operation(t) == (*) ? arguments(t) : (t,)
    out = zero(x)
    for f ∈ fs
        if !_has(f, x)
            _domain_ok(log, f) || return nothing
            out += _apply(log, f)
        elseif iscall(f) && operation(f) == exp
            out += only(arguments(f))
        elseif iscall(f) && operation(f) == (^) && !_has(first(arguments(f)), x)
            a, b = arguments(f)
            out += a == ℯ ? b : b * _apply(log, a)
        else
            return nothing
        end
    end
    out
end

# -t, folding a numeric factor
function _neg(t)
    if iscall(t) && operation(t) == (*)
        args = arguments(t)
        i = findfirst(_isnumeric, args)
        if !isnothing(i)
            c = _fold(-args[i])
            rest = prod(args[j] for j ∈ eachindex(args) if j != i)
            return isone(c) ? rest : c * rest
        end
    end
    -t
end

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
    !isnothing(b) && return (1, only(arguments(t)), b)
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
            b = _fold(only(arguments(first(arguments(f)))))
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
    _fold(_S(b)^s)
end

# sign of a product of constants and exponentials; `nothing` if unknown
function _exp_sign(t, x)
    sgn = 1
    for f ∈ (iscall(t) && operation(t) == (*) ? arguments(t) : (t,))
        if !_has(f, x)
            v = _value(f)
            (isnothing(v) || !isreal(v) || iszero(v)) && return nothing
            sgn *= sign(real(v))
        elseif iscall(f) && operation(f) == exp
            continue
        elseif iscall(f) && operation(f) == (^) && !_has(first(arguments(f)), x) &&
               (v = _value(first(arguments(f))); !isnothing(v) && v > 0)
            continue
        else
            return nothing
        end
    end
    sgn
end

# a⋅cos(w) + b⋅sin(w) ~ c, numeric a, b: R⋅cos(w - φ) ~ c with
# R = √(a² + b²), φ = atan(b, a)
function _harmonic(xs, c, x)
    t₁, t₂ = _trig_term(xs[1]), _trig_term(xs[2])
    (isnothing(t₁) || isnothing(t₂) || t₁[2] == t₂[2] || !isequal(t₁[3], t₂[3])) && return nothing
    (a, _, w), (b, _, _) = t₁[2] == cos ? (t₁, t₂) : (t₂, t₁)
    av, bv = _value(a), _value(b)
    (isnothing(av) || isnothing(bv) || !isreal(av) || !isreal(bv)) && return nothing
    R = _root(_fold(a^2 + b^2), 2)
    φ = iszero(av) ? _fold(sign(bv) * _constant(π) / 2) : _apply(atan, _fold(_S(b) / a))
    av < 0 && (φ = _fold(φ + (bv ≥ 0 ? 1 : -1) * _constant(π)))
    _isolate(cos(w - φ), _fold(_S(c) / R), x)
end

# c⋅sin(w) or c⋅cos(w) ↦ (c, op, w); else `nothing`
function _trig_term(t)
    fs = iscall(t) && operation(t) == (*) ? arguments(t) : (t,)
    i = findfirst(f -> iscall(f) && operation(f) ∈ (sin, cos), fs)
    isnothing(i) && return nothing
    all(_isnumeric(fs[j]) for j ∈ eachindex(fs) if j != i) || return nothing
    c = prod((fs[j] for j ∈ eachindex(fs) if j != i); init = 1)
    (c, operation(fs[i]), only(arguments(fs[i])))
end


# Evaluate a symbolic number when nothing is lost: a rational value, or an
# expression which already involves floating point values. Exact values such
# as `sqrt(2)`, `π/6` or `log(2)` are kept symbolic.
_fold(u::Rational) = isone(denominator(u)) ? numerator(u) : u
_fold(u::Number) = u
function _fold(u)
    _isnumeric(u) || return u
    _isconstantleaf(u) && return _fold(_numval(u))
    _has_float(u) ? _numval(u) : u
end
_has_float(u) = _isconstantleaf(u) ? _numval(u) isa AbstractFloat :
    iscall(u) && any(_has_float, arguments(u))
_S(r::Number) = _constant(r)
_S(r) = r
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
    r isa AbstractFloat && return f(r)
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
        f == log && (isone(v) || v == ℯ) && return Int(isone(v) ? 0 : 1)
        if iszero(v)
            y = f(0.0)
            (iszero(y) || isone(y)) && return Int(y)
        end
        k = _special_angle(f, v)
        !isnothing(k) && return iszero(k) ? 0 : _fold(k * _constant(π))
    end
    _fold(f(_S(r)))
end

# asin, acos, atan at the standard values, as a multiple of π
function _special_angle(f, v)
    f ∈ (asin, acos, atan) && isreal(v) || return nothing
    tbl = f == atan ? ((0, 0//1), (1/sqrt(3), 1//6), (1, 1//4), (sqrt(3), 1//3)) :
        ((0, 0//1), (1/2, 1//6), (sqrt(2)/2, 1//4), (sqrt(3)/2, 1//3), (1, 1//2))
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
        op == sqrt && iseven(n) && return _fold(_S(only(args))^(n ÷ 2))
        op == cbrt && iszero(n % 3) && return _fold(_S(only(args))^(n ÷ 3))
        if op == (*) && length(args) == 2 && _isnumeric(first(args))
            return _fold(_pow_int(first(args), n) * _pow_int(last(args), n))
        end
    end
    _fold(_S(r)^n)
end

# log(r) / log(a), an integer when r is a power of a
function _log_ratio(r, a)
    a == ℯ && return _apply(log, r)
    v, w = _value(r), _value(a)
    if _isexact(v) && _isexact(w) && v > 0 && w > 0 && !isone(w)
        k = round(Int, log(v) / log(w))
        Rational{BigInt}(w)^k == v && return k
    end
    _fold(_apply(log, r) / _apply(log, a))
end

# inverses of unary functions; these are applied non-rigorously
# (principal values only). Returns `nothing` if no inverse is known.
_inverse(::Any) = nothing
for (f, f⁻¹) ∈ ((sin, asin), (cos, acos), (tan, atan),
                (sec, asec), (csc, acsc), (cot, acot),
                (sind, asind), (cosd, acosd), (tand, atand),
                (sinh, asinh), (tanh, atanh),
                (exp, log), (cbrt, r -> _pow_int(r, 3)))
    @eval _inverse(::typeof($f)) = $f⁻¹
    @eval _inverse(::typeof($f⁻¹)) = $f
end
_inverse(::typeof(acosh)) = cosh
_inverse(::typeof(sqrt)) = r -> _pow_int(r, 2)

# Isolate `x` in `u ~ r`, where `r` does not contain `x`.
# Returns a tuple of equations (possibly empty) or `nothing` if `x` cancels
function _isolate(u, r, x)
    u == x && return (_Sol(x, _fold(r)),)
    _has(u, x) || return _iszero(_fold(u - r)) ? nothing : () # x cancelled
    iscall(u) || return (_Sol(u, r),)
    op, args = operation(u), arguments(u)

    lt = _log_term(u) # k⋅log_b(w) ~ r
    if !isnothing(lt)
        k, w, b = lt
        return _isolate(w, _exp_base(b, _fold(r / k)), x)
    end

    if op == (+) || op == (*)
        i = _only_index(a -> _has(a, x), args)
        if !isnothing(i)
            rest = (args[j] for j in eachindex(args) if j != i)
            r′ = op == (+) ? r - sum(rest) : r / prod(rest)
            return _isolate(args[i], r′, x)
        end
        if op == (*) && _iszero(r) # zero-product rule
            return _branches((_isolate(a, r, x) for a ∈ args if _has(a, x)))
        end
        if op == (+)
            xs = [a for a ∈ args if _has(a, x)]
            rest = [a for a ∈ args if !_has(a, x)]
            r′ = isempty(rest) ? r : r - sum(rest)
            lts = map(_log_term, xs) # Σ kᵢ⋅log_b(uᵢ) ~ r′  ⟹  Π uᵢ^kᵢ ~ b^r′
            if all(!isnothing, lts) && all(lt -> lt[3] == lts[1][3], lts)
                U = prod(isone(k) ? w : w^k for (k, w, _) ∈ lts)
                return _isolate(U, _exp_base(lts[1][3], _fold(r′)), x)
            end
            if _iszero(r′)
                # sum of exponentials of one sign
                sgns = map(t -> _exp_sign(t, x), xs)
                all(!isnothing, sgns) && allequal(sgns) && return ()
            end
            if length(xs) == 2 && !_iszero(r′)
                # a⋅cos(w) + b⋅sin(w) ~ c  ⟹  R⋅cos(w - φ) ~ c
                sols = _harmonic(xs, r′, x)
                !isnothing(sols) && return sols
            end
            if length(xs) == 2 && _iszero(r′)
                # a⋅cos(w) + b⋅sin(w) ~ 0  ⟹  tan(w) ~ -a/b
                t₁, t₂ = _trig_term(xs[1]), _trig_term(xs[2])
                if !isnothing(t₁) && !isnothing(t₂) && t₁[2] != t₂[2] && isequal(t₁[3], t₂[3])
                    (c, _, w), (s′, _, _) = t₁[2] == cos ? (t₁, t₂) : (t₂, t₁)
                    return _isolate(tan(w), _fold(-_S(c) / s′), x)
                end
                # f(u) - f(v) ~ 0
                m = _match_sides(xs[1], _neg(xs[2]), x)
                !isnothing(m) && return _solve(m..., x)
            end
        end
    elseif op == (^)
        a, b = args
        if !_has(b, x)
            return _isolate_power(a, b, r, x)
        elseif !_has(a, x)  # a^u ~ r
            _domain_ok(log, r) || return ()
            return _isolate(b, _log_ratio(r, a), x)
        end
    elseif length(args) == 1
        a = only(args)
        if op == abs || op == cosh  # two branches
            _maybe_nonneg(r) && _domain_ok(op == abs ? identity : acosh, r) || return ()
            sols = _isolate_pm(a, op == abs ? _fold(r) : _apply(acosh, r), x)
            return op == abs ? _tag(sols, (r, :nonneg)) : sols
        end
        if op ∈ _injective && iscall(r) && operation(r) == op # f(u) ~ f(v)
            return _isolate(a, only(arguments(r)), x)
        end
        f⁻¹ = _inverse(op)
        if !isnothing(f⁻¹)
            op == sqrt && !_maybe_nonneg(r) && return ()
            _domain_ok(f⁻¹, r) || return ()
            sols = _isolate(a, _apply(f⁻¹, r), x)
            return op == sqrt ? _tag(sols, (r, :nonneg)) : sols
        end
    end

    # stuck; try a rational function, then a substitution
    nd = _rat_coeffs(u - r, x)
    if !isnothing(nd)
        num, den = nd
        length(num) == 1 && return nothing # x cancels
        sols = _poly_roots(num, x)
        isnothing(sols) && return (_Sol(u, r),)
        return Tuple(s for s ∈ sols if !_vanishes(den, s.rhs))
    end
    ex = u - r
    for f ∈ (_common_factor, _split_abs, _square_radical, _substitute)
        sols = f(ex, x)
        !isnothing(sols) && return sols
    end
    (_Sol(u, r),)
end

_sum_terms(ex) = iscall(ex) && operation(ex) == (+) ? arguments(ex) : (ex,)
_prod_factors(ex) = iscall(ex) && operation(ex) == (*) ? arguments(ex) : (ex,)

# ex = Σ Pᵢ(x)⋅Nᵢ ~ 0 with polynomials Pᵢ sharing a factor G(x):
# solve G ~ 0 and Σ (Pᵢ/G)⋅Nᵢ ~ 0
function _common_factor(ex, x)
    Ps, Ns = Any[], Any[]
    for t ∈ _sum_terms(ex)
        P, N = Any[1], one(x)
        for f ∈ _prod_factors(t)
            cs = _poly_coeffs(f, x)
            isnothing(cs) ? (N = N * f) : (P = _poly_mul(P, cs))
        end
        i = findfirst(n -> isequal(n, N), Ns)
        isnothing(i) ? (push!(Ps, P); push!(Ns, N)) : (Ps[i] = _poly_add(Ps[i], P))
    end
    length(Ns) > 1 || return nothing
    qs = map(P -> _exact_coeffs(P), Ps)
    any(isnothing, qs) && return nothing
    G = reduce(_poly_gcd, qs)
    length(G) > 1 || return nothing
    Qs = map(q -> first(_poly_divrem(q, G)), qs)
    ex′ = sum(_poly_expr(Q, x) * N for (Q, N) ∈ zip(Qs, Ns))
    _branches((_poly_roots(map(_fold ∘ Rational{Int}, G), x), _isolate(ex′, zero(x), x)))
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

# split abs(w) into the cases w ≥ 0 and w ≤ 0
function _split_abs(ex, x)
    a = _find(t -> iscall(t) && operation(t) == abs && _has(t, x), ex)
    isnothing(a) && return nothing
    w = only(arguments(a))
    _branches((_abs_case(ex, a, w, s, x) for s ∈ (1, -1)))
end
function _abs_case(ex, a, w, s, x)
    sols = _isolate(_subst(ex, [a], [s == 1 ? w : -w], x), zero(x), x)
    isnothing(sols) && return nothing
    sols = Tuple(sol for sol ∈ sols if !(isequal(sol.lhs, x) && _isnumeric(sol.rhs)) ||
                     _sign_ok(w, s, _numval(sol.rhs), x))
    _tag(sols, (s == 1 ? w : -w, :nonneg))
end
function _sign_ok(w, s, v, x)
    _symbols(w) == (x,) || return true
    wv = _value_at(w, x => v)
    isnothing(wv) || s * wv ≥ -1e-12
end

# first subexpression satisfying pred
function _find(pred, ex)
    pred(ex) && return ex
    iscall(ex) || return nothing
    for a ∈ arguments(ex)
        t = _find(pred, a)
        !isnothing(t) && return t
    end
    nothing
end

# Remove square or cube roots: write ex ~ 0 as L ~ R with L holding a
# radical term (about half of them, for three or more square roots) and raise
# both sides to the power n = 2 or 3. Products of square roots are merged,
# √a⋅√b = √(ab), so each step lowers the number of radicals. Squaring may
# introduce extraneous solutions; it needs L⋅R ≥ 0.
function _square_radical(ex, x)
    for n ∈ (2, 3)
        ts = _group_radicals(collect(_sum_terms(ex)), n, x)
        is = [i for (i, t) ∈ enumerate(ts) if _nrads(t, n, x) == 1]
        isempty(is) && continue
        k = n == 2 && length(is) ≥ 3 ? cld(length(is), 2) : 1
        left = is[1:k]
        L = [ts[i] for i ∈ left]
        R = [-t for (i, t) ∈ enumerate(ts) if i ∉ left]
        if n == 2
            ex′ = _sq_sum(L, x) - _sq_sum(R, x)
        else
            any(t -> _nrads(t, 3, x) > 0, R) && continue
            ex′ = _pow_term(only(L), 3, x) - sum(R; init = zero(x))^3
        end
        _nroots(ex′, x) < _nroots(ex, x) || continue
        sols = _isolate(ex′, zero(x), x)
        n == 3 && return sols
        Lₑ, Rₑ = sum(L), sum(R; init = zero(x))
        if k == 1 # c⋅√w = R needs c⋅R ≥ 0
            Lₑ = prod((f for f ∈ _prod_factors(only(L)) if isnothing(_rad_parts(f, x))); init = 1)
        end
        return _tag(sols, (Lₑ * Rₑ, :nonneg))
    end
    nothing
end

# radical factor f = (ⁿ√w)ᵐ ↦ (w, n, m); else `nothing`
function _rad_parts(f, x)
    iscall(f) || return nothing
    op, args = operation(f), arguments(f)
    if op == sqrt || op == cbrt
        w = only(args)
        _has(w, x) || return nothing
        return (w, op == sqrt ? 2 : 3, 1)
    elseif op == (^)
        b, e = args
        v = _value(e)
        isnothing(v) && return nothing
        if isinteger(v)
            inner = _rad_parts(b, x)
            isnothing(inner) && return nothing
            w, n, m = inner
            return (w, n, m * Int(v))
        elseif v isa Rational && denominator(v) ∈ (2, 3) && _has(b, x)
            return (b, denominator(v), numerator(v))
        end
    end
    nothing
end
_isrootnode(f, x) = iscall(f) && _has(f, x) &&
    (operation(f) ∈ (sqrt, cbrt) ||
     (operation(f) == (^) && (v = _value(last(arguments(f))); v isa Rational && denominator(v) ∈ (2, 3))))
# number of distinct radicals in ex
_nroots(ex, x) = length(_roots!(Any[], ex, x))
function _roots!(out, ex, x)
    _isrootnode(ex, x) && !any(r -> isequal(r, ex), out) && push!(out, ex)
    iscall(ex) && foreach(a -> _roots!(out, a, x), arguments(ex))
    out
end

# combine terms c₁⋅ⁿ√w + c₂⋅ⁿ√w into (c₁ + c₂)⋅ⁿ√w
function _group_radicals(ts, n, x)
    keys, coefs, out = Any[], Any[], Any[]
    for t ∈ ts
        fs = _prod_factors(t)
        i = findfirst(f -> (rp = _rad_parts(f, x); !isnothing(rp) && rp[2] == n), fs)
        if isnothing(i) || _nrads(t, n, x) != 1
            push!(out, t)
            continue
        end
        c = prod((fs[j] for j ∈ eachindex(fs) if j != i); init = 1)
        k = findfirst(f -> isequal(f, fs[i]), keys)
        isnothing(k) ? (push!(keys, fs[i]); push!(coefs, c)) : (coefs[k] += c)
    end
    append!(out, (c * f for (c, f) ∈ zip(coefs, keys)))
end
_nrads(t, n, x) = count(f -> (rp = _rad_parts(f, x); !isnothing(rp) && rp[2] == n), _prod_factors(t))

# tⁿ for a term t, with (ⁿ√w)ᵐ ↦ wᵐ
_pow_term(t, n, x) = prod((rp = _rad_parts(f, x); isnothing(rp) || rp[2] != n ? f^n : rp[1]^rp[3])
                          for f ∈ _prod_factors(t))

# (Σ tᵢ)² expanded, merging products of square roots
function _sq_sum(ts, x)
    out = zero(x)
    for (i, t) ∈ enumerate(ts), (j, t′) ∈ enumerate(ts)
        j < i && continue
        out += (i == j ? 1 : 2) * _sqrt_mul(t, t′, x)
    end
    out
end

# t⋅t′ with square-root factors combined into one √
function _sqrt_mul(t, t′, x)
    rest, rads = Any[], Any[] # (w, m) for (√w)ᵐ
    for f ∈ (_prod_factors(t)..., _prod_factors(t′)...)
        rp = _rad_parts(f, x)
        if isnothing(rp) || rp[2] != 2
            push!(rest, f)
        else
            w, _, m = rp
            i = findfirst(((w′, _),) -> isequal(w′, w), rads)
            isnothing(i) ? push!(rads, (w, m)) : (rads[i] = (w, rads[i][2] + m))
        end
    end
    odd = Any[]
    for (w, m) ∈ rads # (√w)ᵐ = w^⌊m/2⌋ ⋅ √w if m is odd
        h = fld(m, 2)
        iszero(h) || push!(rest, w^h)
        isodd(m) && push!(odd, w)
    end
    isempty(odd) || push!(rest, sqrt(prod(odd)))
    prod(rest; init = 1)
end
_count(pred, ex) = pred(ex) + (iscall(ex) ? sum(a -> _count(pred, a), arguments(ex); init = 0) : 0)

# numerator, denominator coefficients of ex as a rational function of x
function _rat_coeffs(ex, x)
    _has(ex, x) || return (Any[ex], Any[1])
    isequal(ex, x) && return (Any[0, 1], Any[1])
    iscall(ex) || return nothing
    op, args = operation(ex), arguments(ex)
    if op == (+) || op == (*)
        n, d = Any[op == (+) ? 0 : 1], Any[1]
        for a ∈ args
            nd = _rat_coeffs(a, x)
            isnothing(nd) && return nothing
            n′, d′ = nd
            if op == (*)
                n, d = _poly_mul(n, n′), _poly_mul(d, d′)
            elseif isequal(d, d′)
                n = _poly_add(n, n′)
            else
                n, d = _poly_add(_poly_mul(n, d′), _poly_mul(n′, d)), _poly_mul(d, d′)
            end
        end
        return (n, d)
    elseif op == (^)
        a, b = args
        k = _value(b)
        (!isnothing(k) && isinteger(k)) || return nothing
        nd = _rat_coeffs(a, x)
        isnothing(nd) && return nothing
        n, d = nd
        k = Int(k)
        return k ≥ 0 ? (_poly_pow(n, k), _poly_pow(d, k)) : (_poly_pow(d, -k), _poly_pow(n, -k))
    end
    nothing
end

# is the polynomial with coefficients cs known to vanish at v?
function _vanishes(cs, v)
    length(cs) == 1 && return false
    vs = map(_value, cs)
    c = _value(v)
    (isnothing(c) || any(isnothing, vs)) || return iszero(_horner(vs, c))
    _iszero(_expand_poly(_horner(cs, v)))
end

# Solve ex ~ 0 when ex is a polynomial in some g(x) (e.g. exp(x), sqrt(x)):
# every term with x is a power gᵠ, written (g, q) below.
const _𝓎 = _fresh(:𝓎)
function _substitute(ex, x)
    atoms = _atoms!(Any[], ex, x)
    gqs = map(a -> _as_power(a, x), atoms)
    g = first(first(gqs))
    all(gq -> isequal(first(gq), g) && last(gq) isa Union{Integer, Rational}, gqs) ||
        return nothing
    q = reduce(gcd, last.(gqs))
    gᵠ = isone(q) ? g : g^q
    ex′ = _subst(ex, atoms, [_𝓎^_fold(Rational(qᵢ // q)) for (_, qᵢ) ∈ gqs], x)
    _has(ex′, x) && return nothing
    # no progress if gᵠ appears once, linearly: it could not be isolated already
    cs = _poly_coeffs(ex′, _𝓎)
    isone(q) && count(t -> _has(t, x), _sum_terms(ex)) == 1 &&
        !isnothing(cs) && length(cs) ≤ 2 && return nothing
    ys = _isolate(ex′, zero(x), _𝓎)
    isnothing(ys) && return nothing
    all(s -> isequal(s.lhs, _𝓎), ys) || return nothing
    _branches((_carry(_isolate(gᵠ, s.rhs, x), s, gᵠ) for s ∈ ys))
end
# conditions on 𝓎 for solution s, as conditions on x through 𝓎 = gᵠ
function _carry(sols, s, gᵠ)
    for (c, k) ∈ s.conds
        sols = _tag(sols, (_subst(c, [_𝓎], [gᵠ], _𝓎), k))
    end
    sols
end

# maximal non-polynomial subexpressions containing x
function _atoms!(out, ex, x)
    _has(ex, x) || return out
    if iscall(ex) && (operation(ex) == (+) || operation(ex) == (*))
        foreach(a -> _atoms!(out, a, x), arguments(ex))
    elseif iscall(ex) && operation(ex) == (^) && _is_int(last(arguments(ex)))
        _atoms!(out, first(arguments(ex)), x)
    else
        any(a -> isequal(a, ex), out) || push!(out, ex)
    end
    out
end
_is_int(b) = (v = _value(b); !isnothing(v) && isinteger(v))

# write atom as (g, q) with atom = gᵠ
function _as_power(a, x)
    iscall(a) || return (a, 1)
    op, args = operation(a), arguments(a)
    op == sqrt && return (only(args), 1//2)
    op == cbrt && return (only(args), 1//3)
    if op == (^)
        b, c = args
        v = _value(c)
        v isa Rational && return (b, v)
        _has(b, x) && return (a, 1)
        k, w = _split_int(c)
        m, e = _min_base(b)               # b = mᵉ
        return (_constant(m)^w, k * e) # bᵏʷ = (mʷ)ᵏᵉ
    elseif op == exp
        k, w = _split_int(only(args))
        return (exp(w), k)                # exp(k⋅w)
    end
    (a, 1)
end

# b = mᵉ for an integer b with the smallest base m; (b, 1) otherwise
function _min_base(b)
    v = _value(b)
    (v isa Integer && v > 1) || return (b, 1)
    for e ∈ floor(Int, log2(v)):-1:2
        m, rest = _surd(v, e)
        isone(rest) && isinteger(m) && return (Int(m), e)
    end
    (v, 1)
end

# k⋅w ↦ (k, w) for an integer k
function _split_int(t)
    if iscall(t) && operation(t) == (*)
        args = arguments(t)
        i = findfirst(_is_int, args)
        if !isnothing(i)
            w = prod(args[j] for j ∈ eachindex(args) if j != i)
            return (Int(_value(args[i])), w)
        end
    end
    (1, t)
end

# replace atoms by their substitutes
function _subst(ex, atoms, subs, x)
    _has(ex, x) || return ex
    i = findfirst(a -> isequal(a, ex), atoms)
    !isnothing(i) && return subs[i]
    iscall(ex) || return ex
    _rebuild(ex, operation(ex), map(a -> _subst(a, atoms, subs, x), arguments(ex)))
end

# solve u ~ ±ρ
function _isolate_pm(u, ρ, x)
    iszero(ρ) && return _isolate(u, ρ, x)
    _branches((_isolate(u, -ρ, x), _isolate(u, ρ, x)))
end

# u^b ~ r with b free of x
function _isolate_power(u, b, r, x)
    n = _value(b)
    if !isnothing(n) && isinteger(n)
        n = Int(n)
        n == 0 && return nothing
        n < 0 && _iszero(r) && return ()
        n < 0 && return _isolate_power(u, -n, 1 / r, x)
        n == 1 && return _isolate(u, r, x)
        if iseven(n)
            _maybe_nonneg(r) || return ()
            return _isolate_pm(u, _root(r, n), x)
        end
        return _isolate(u, _root(r, n), x)
    elseif n isa Rational && iseven(numerator(n)) # u^(2//3) ~ 4
        _maybe_nonneg(r) || return ()
        return _isolate_pm(u, _rpow(r, 1 / n), x)
    elseif n isa Rational && iseven(denominator(n)) # u^(1//2) ~ r
        _maybe_nonneg(r) || return ()
    end
    n isa Rational && return _isolate(u, _rpow(r, 1 / n), x)
    _isolate(u, r^(1 / b), x)
end

# combine solutions from several branches, dropping duplicates
function _branches(itr)
    out = ()
    for sols ∈ itr
        isnothing(sols) && continue
        for s ∈ sols
            any(t -> isequal(t.lhs, s.lhs) && isequal(t.rhs, s.rhs), out) && continue
            out = (out..., s)
        end
    end
    out
end

# solve a₀ + a₁⋅x + ⋯ + aₙ⋅xⁿ ~ 0, n ≥ 1; `nothing` if not possible
function _poly_roots(cs, x)
    # factor out xᵏ
    k = findfirst(!_iszero, cs) - 1
    k > 0 && return _branches(((_Sol(x, 0),), _poly_roots(cs[k+1:end], x)))

    n = length(cs) - 1
    n == 0 && return ()
    if n == 1
        a₀, a₁ = cs
        return (_Sol(x, _expand_poly(_fold(-a₀ / a₁))),)
    end
    sols = _rational_roots(cs, x)
    !isnothing(sols) && return sols
    n == 2 && return _quadratic_roots(cs, x)

    # polynomial in xᵍ
    g = gcd([i - 1 for i ∈ eachindex(cs) if !_iszero(cs[i])])
    if g > 1
        ys = _poly_roots(cs[1:g:end], x)
        isnothing(ys) && return nothing
        all(s -> s.lhs == x, ys) || return nothing
        return _branches((_isolate_power(x, g, s.rhs, x) for s ∈ ys))
    end

    n == 3 && return _cubic_roots(cs, x)
    n == 4 && return _quartic_roots(cs, x)
    nothing
end

# numeric coefficients, as exact rationals when possible
function _num_coeffs(cs)
    vs = map(_value, cs)
    all(v -> !isnothing(v) && isreal(v), vs) || return nothing
    all(_isexact, vs) ? Rational{BigInt}.(vs) : float.(real.(vs))
end
_q(v::Rational) = _fold(Rational{Int}(v))
_q(v) = v

# a₀ + a₁x + a₂x² + a₃x³ ~ 0, numeric coefficients. With x = t - a₂/(3a₃),
# t³ + p⋅t + q = 0: Cardano's formula when there is one real root, the
# trigonometric form when there are three
function _cubic_roots(cs, x)
    vs = _num_coeffs(cs)
    isnothing(vs) && return nothing
    a₀, a₁, a₂, a₃ = vs
    s = a₂ / (3a₃)
    p = (3a₃ * a₁ - a₂^2) / (3a₃^2)
    q = (2a₂^3 - 9a₃ * a₂ * a₁ + 27a₃^2 * a₀) / (27a₃^3)
    Δ = q^2 / 4 + p^3 / 27
    shift(t) = _Sol(x, _pi_fold(_fold(t - _q(s))))
    iszero(p) && iszero(q) && return (shift(0),)
    if iszero(Δ) # a double root
        return _branches(((shift(_q(3q / p)),), (shift(_q(-3q / (2p))),)))
    elseif Δ > 0
        sΔ = _root(_q(Δ), 2)
        u, v = _fold(_q(-q / 2) + sΔ), _fold(_q(-q / 2) - sΔ)
        return (shift(_fold(_root(u, 3) + _root(v, 3))),)
    end
    m = _fold(2 * _root(_q(-p / 3), 2))
    θ = _fold(_apply(acos, _fold(_q(3q / (2p)) * _root(_q(-3 / p), 2))) / 3)
    Tuple(shift(_fold(m * _apply(cos, _pi_fold(_fold(θ - 2k * _constant(π) / 3)))))
          for k ∈ 0:2)
end

# a₀ + a₁x + ⋯ + a₄x⁴ ~ 0, numeric coefficients, by Ferrari's method: with
# x = y - b/4, y⁴ + p⋅y² + q⋅y + r = 0 factors into two quadratics using a
# positive root m of the resolvent 8m³ + 8p⋅m² + (2p² - 8r)⋅m - q² = 0
function _quartic_roots(cs, x)
    vs = _num_coeffs(cs)
    isnothing(vs) && return nothing
    e, d, c, b = vs[1:4] ./ vs[5]
    p = c - 3b^2 / 8
    q = d - b * c / 2 + b^3 / 8
    r = e - b * d / 4 + b^2 * c / 16 - 3b^4 / 256
    shift(sols) = isnothing(sols) ? nothing :
        Tuple(_Sol(x, _fold(sol.rhs - _q(b / 4))) for sol ∈ sols if isequal(sol.lhs, _𝓎))
    iszero(q) && return shift(_poly_roots(Any[_q(r), 0, _q(p), 0, 1], _𝓎))
    ms = _poly_roots(Any[_q(-q^2), _q(2p^2 - 8r), _q(8p), 8], _𝓎)
    isnothing(ms) && return nothing
    ms = [sol.rhs for sol ∈ ms if isequal(sol.lhs, _𝓎) && (v = _value(sol.rhs); isreal(v) && real(v) > 0)]
    isempty(ms) && return nothing
    m = argmin(m -> length(string(m)), ms) # the simplest
    s₂ = _root(_fold(2m), 2)
    _branches((shift(_quadratic_roots(Any[_fold(_q(p / 2) + m - _q(q / 2) / s₂), s₂, 1], _𝓎)),
               shift(_quadratic_roots(Any[_fold(_q(p / 2) + m + _q(q / 2) / s₂), _fold(-s₂), 1], _𝓎))))
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

function _quadratic_roots(cs, x)
    a₀, a₁, a₂ = map(_S, cs)
    Δ = _fold(_pow_int(a₁, 2) - 4 * a₂ * a₀)
    v = _value(Δ)
    if !isnothing(v)
        v < 0 && return ()
        iszero(v) && return (_Sol(x, _fold(-a₁ / (2a₂))),)
    end
    c = _value(2a₂)
    if _isexact(v) && _isexact(c) # p ± k⋅√m exactly
        p = _fold(-a₁ / (2a₂))
        k, m = _surd(v, 2)
        isone(m) && return (_Sol(x, _fold(p - k // c)), _Sol(x, _fold(p + k // c)))
        q = _fold(k // c) * sqrt(_constant(m))
        iszero(p) && return (_Sol(x, -q), _Sol(x, q))
        return (_Sol(x, p - q), _Sol(x, p + q))
    end
    Δ = _expand_poly(Δ)
    sΔ = _sqrt_poly(Δ)
    sΔ = isnothing(sΔ) ? _root(Δ, 2) : sΔ
    (_Sol(x, _expand_poly(_fold((-a₁ - sΔ) / (2a₂)))),
     _Sol(x, _expand_poly(_fold((-a₁ + sΔ) / (2a₂)))))
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
    r = _S(r)
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
    _fold(_S(r)^e)
end

# rational root theorem for numeric, rational coefficients
function _rational_roots(cs, x)
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
    sols = (_Sol(x, _fold(Rational{Int}(c))) for c ∈ sort!(roots))
    length(qs) < 3 && return _branches((sols,))
    rest = map(_fold ∘ Rational{Int}, qs)
    f = (_quadratic_roots, _cubic_roots, _quartic_roots)[length(qs) - 2]
    _branches((sols, f(rest, x)))
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

# index of only element satisfying pred, or `nothing`
function _only_index(pred, xs)
    i = nothing
    for (j, a) ∈ enumerate(xs)
        pred(a) || continue
        isnothing(i) || return nothing
        i = j
    end
    i
end
