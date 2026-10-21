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
#
## extensive modifications by Claude AI
"""
    solve_equation(l, r, x; conditions=false)

Solve `l ~ r` for `x`. This is the generic solver behind `solve`; see its
docstring for what is solved and what is returned. It walks expressions with
TermInterface, builds them with ordinary operators, and uses the hooks of
`solve_interface.jl` for everything else.
"""
function solve_equation(l, r, x; conditions::Bool = false, interval = nothing)
    isnothing(interval) && return _solve_equation(l, r, x, conditions, nothing)
    any(s -> !isequal(s, x), _symbols(l - r)) &&
        throw(ArgumentError("`interval` requires an equation without parameters"))
    a, b = map(_numval, interval)
    a ≤ b || throw(ArgumentError("`interval` must be (a, b) with a ≤ b"))
    task_local_storage(:solve_interval, true) do
        _solve_equation(l, r, x, conditions, (a, b))
    end
end

function _solve_equation(l, r, x, conditions, ab)
    sols = _solve(l, r, x)
    isnothing(sols) && return nothing
    isnothing(ab) || (sols = _in_interval(sols, x, ab...))
    sols = _verify(sols, l, r, x)
    out = Tuple((e = _equation(s.lhs, s.rhs); conditions ? e => c : e) for (s, c) ∈ sols)
    length(out) == 1 ? only(out) : out
end

## ----
# A solution lhs ~ rhs (solved when lhs is x), valid under conditions on x;
# a condition is (ex, kind), kind one of :pos, :nonneg, :nonzero
struct _Sol
    lhs
    rhs
    conds::Vector{Any}
    # numbers are wrapped as constants so later arithmetic stays exact
    _Sol(l, r, conds) = new(l, r isa Number ? _fold(r) : r, conds)
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

# Isolate `x` in `u ~ r`, where `r` does not contain `x`.
# Returns a tuple of equations (possibly empty) or `nothing` if `x` cancels
function _isolate(u, r, x)
    r = _fold(r)
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
                    return _isolate(tan(w), _fold(-c / s′), x)
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
        if _interval_mode() # all branches of a periodic function
            f⁻¹ = _inverse(op)
            fams = isnothing(f⁻¹) || !_domain_ok(f⁻¹, r) ? nothing : _periodic_branches(op, r)
            !isnothing(fams) && return _branches((_isolate(a, θ, x) for θ ∈ fams))
            !isnothing(f⁻¹) && op ∈ (sin, cos, tan, sec, csc, cot, sind, cosd, tand) && return ()
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
    frac = _rat_coeffs(u - r, x)
    if !isnothing(frac)
        num, den = frac
        length(num) == 1 && return nothing # x cancels
        ρs = _poly_roots(num)
        isnothing(ρs) && return (_Sol(u, r),)
        return Tuple(_Sol(x, ρ) for ρ ∈ ρs if !_vanishes(den, ρ))
    end
    ex = u - r
    for f ∈ (_common_factor, _split_abs, _square_radical, _substitute, _lambert, _trig_identity)
        sols = f(ex, x)
        !isnothing(sols) && return sols
    end
    (_Sol(u, r),)
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
    if op == lambertw && has(first(args)) # W₀ needs z ≥ -1/e; W₋₁ also z < 0
        w = first(args)
        push!(out, (w + exp(_constant(-1)), :nonneg))
        length(args) == 2 && push!(out, (-w, :pos))
    elseif length(args) == 1 && has(only(args))
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

## ---- all solutions in an interval
# In interval mode inverting a periodic function gives families of solutions
# using the integer symbol 𝓃; these are enumerated over the interval [a, b].
const _𝓃 = _fresh(:𝓃)
_interval_mode() = get(task_local_storage(), :solve_interval, false)
const _max_solutions = 10_000

# u + 2π𝓃 style families for f(u) ~ r, f periodic; `nothing` if f isn't
function _periodic_branches(f, r)
    π′, 𝓃 = _constant(π), _𝓃
    fam(θ, p) = _fold(θ + p * 𝓃)
    if f ∈ (sin, csc, cos, sec)
        θ = _apply(f ∈ (sin, csc) ? asin : acos, f ∈ (csc, sec) ? _fold(1 / r) : r)
        θ′ = f ∈ (sin, csc) ? _pi_fold(_fold(π′ - θ)) : _fold(-θ)
        return (fam(θ, 2π′), fam(θ′, 2π′))
    elseif f ∈ (tan, cot)
        return (fam(f == tan ? _apply(atan, r) : _apply(acot, r), π′),)
    elseif f ∈ (sind, cosd)
        θ = _apply(f == sind ? asind : acosd, r)
        θ′ = f == sind ? _fold(180 - θ) : _fold(-θ)
        return (fam(θ, 360), fam(θ′, 360))
    elseif f == tand
        return (fam(_apply(atand, r), 180),)
    end
    nothing
end

# solutions in [a, b], enumerating the families
function _in_interval(sols, x, a, b)
    tol = 1e-10 * (1 + max(abs(a), abs(b)))
    inside(v) = !isnothing(v) && isreal(v) && a - tol ≤ real(v) ≤ b + tol
    out, vals = Any[], Float64[]
    function keep!(s, ρ)
        v = _value(ρ)
        inside(v) || return
        any(w -> isapprox(w, real(v); atol = tol), vals) && return
        push!(vals, real(v))
        push!(out, _Sol(x, ρ, s.conds))
    end
    for s ∈ sols
        if !isequal(s.lhs, x)
            push!(out, s)
        elseif !_has(s.rhs, _𝓃)
            keep!(s, s.rhs)
        else
            for ρ ∈ _enumerate(s.rhs, a, b, tol)
                keep!(s, ρ)
            end
        end
    end
    Tuple(out)
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
            return _comparison(op′, only(syms), _fold(-b // a))
        end
    end
    _comparison(op, c, _fold(0))
end

function _satisfies(l, r, x, v)
    lv, rv = try
        _value_at(l, x => v), _value_at(r, x => v)
    catch err # can't evaluate (e.g. x//4 at a float): keep, unverified
        err isa MethodError || rethrow()
        return true
    end
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

## ----- inverses
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

_sum_terms(ex) = iscall(ex) && operation(ex) == (+) ? arguments(ex) : (ex,)
_prod_factors(ex) = iscall(ex) && operation(ex) == (*) ? arguments(ex) : (ex,)

## ----- Exponential and logs

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

## ------- Trig functions
# a⋅cos(w) + b⋅sin(w) ~ c, numeric a, b: R⋅cos(w - φ) ~ c with
# R = √(a² + b²), φ = atan(b, a)
function _harmonic(xs, c, x)
    t₁, t₂ = _trig_term(xs[1]), _trig_term(xs[2])
    (isnothing(t₁) || isnothing(t₂) || t₁[2] == t₂[2] || !isequal(t₁[3], t₂[3])) && return nothing
    (a, _, w), (b, _, _) = t₁[2] == cos ? (t₁, t₂) : (t₂, t₁)
    av, bv = _value(a), _value(b)
    (isnothing(av) || isnothing(bv) || !isreal(av) || !isreal(bv)) && return nothing
    R = _root(_fold(a^2 + b^2), 2)
    φ = iszero(av) ? _fold(sign(bv) * _constant(π) / 2) : _apply(atan, _fold(b / a))
    av < 0 && (φ = _fold(φ + (bv ≥ 0 ? 1 : -1) * _constant(π)))
    _isolate(cos(w - φ), _fold(c / R), x)
end

# c⋅sin(w) or c⋅cos(w) ↦ (c, op, w); else `nothing`
function _trig_term(t)
    fs = iscall(t) && operation(t) == (*) ? arguments(t) : (t,)
    i = findfirst(f -> iscall(f) && operation(f) ∈ (sin, cos), fs)
    isnothing(i) && return nothing
    all(_isnumeric(fs[j]) for j ∈ eachindex(fs) if j != i) || return nothing
    c = _fold(prod((fs[j] for j ∈ eachindex(fs) if j != i); init = 1))
    (c, operation(fs[i]), only(arguments(fs[i])))
end

## ---- Lambert W
# Equations solved with W, the inverse of w ↦ w⋅eʷ, with real branches W₀
# (z ≥ -1/e) and W₋₁ (-1/e ≤ z < 0):
#   α⋅x + β + γ⋅b^(p⋅x + q) ~ 0     x = -W(z)/(p⋅log b) - β/α
#   c⋅(a₁⋅x + a₀)⋅b^(p⋅x + q) ~ r   x = W(z)/(p⋅log b) - a₀/a₁
#   α⋅x + β + γ⋅log(p⋅x + q) ~ 0    (any log base)
#   c⋅uᵘ ~ r,  c⋅u⋅log(u) ~ r       u = exp(W(z))
function _lambert(ex, x)
    ts = collect(_sum_terms(ex))
    xts = [t for t ∈ ts if _has(t, x)]
    r = _fold(-sum((t for t ∈ ts if !_has(t, x)); init = zero(x))) # x-terms ~ r
    if length(xts) == 1
        sols = _lambert_product(only(xts), r, x)
        !isnothing(sols) && return sols
    end
    _lambert_sum(xts, r, x)
end

# c⋅uᵘ ~ r, c⋅u⋅log(u) ~ r, c⋅(a₁⋅x + a₀)⋅b^(p⋅x + q) ~ r
function _lambert_product(t, r, x)
    fs = _prod_factors(t)
    c = _fold(prod((f for f ∈ fs if !_has(f, x)); init = 1))
    xfs = [f for f ∈ fs if _has(f, x)]
    R = _fold(r / c)
    if length(xfs) == 1 && iscall(only(xfs)) && operation(only(xfs)) == (^)
        u, v = arguments(only(xfs))
        isequal(u, v) || return nothing # uᵘ ~ R ⟹ log(u)⋅e^log(u) = log(R)
        _domain_ok(log, R) || return ()
        return _branches((_isolate(u, _apply(exp, ω), x) for ω ∈ _W(_apply(log, R))))
    end
    length(xfs) == 2 || return nothing
    i = findfirst(f -> iscall(f) && operation(f) == log, xfs)
    if !isnothing(i) # u⋅log(u) ~ R
        u = only(arguments(xfs[i]))
        isequal(u, xfs[3 - i]) || return nothing
        return _branches((_isolate(u, _apply(exp, ω), x) for ω ∈ _W(R)))
    end
    for (P, E) ∈ (xfs, reverse(xfs)) # (a₁⋅x + a₀)⋅b^(p⋅x + q) ~ R
        cs = _linear(P, x)
        e = _exponential(E, x)
        (isnothing(cs) || isnothing(e)) && continue
        a₀, a₁ = cs
        b, p, q = e
        _iszero(R) && return nothing
        a = _fold(-a₀ / a₁)
        z = _fold(p * _log_of(b) * R / a₁ * _exp_base(b, _fold(-p * a - q)))
        return Tuple(_Sol(x, _fold(_div_log(ω, b) / p + a)) for ω ∈ _W(z))
    end
    nothing
end

# α⋅x + β + γ⋅b^(p⋅x + q) ~ 0 or α⋅x + β + γ⋅log_b(p⋅x + q) ~ 0, terms xts ~ r
function _lambert_sum(xts, r, x)
    α, β, other = _fold(0), _fold(-r), nothing
    for t ∈ xts
        cs = _linear(t, x)
        if !isnothing(cs)
            α, β = _fold(α + cs[2]), _fold(β + cs[1])
        else
            isnothing(other) || return nothing
            other = t
        end
    end
    (isnothing(other) || _iszero(α)) && return nothing
    fs = _prod_factors(other)
    γ = _fold(prod((f for f ∈ fs if !_has(f, x)); init = 1))
    xfs = [f for f ∈ fs if _has(f, x)]
    length(xfs) == 1 || return nothing
    e = _exponential(only(xfs), x)
    if !isnothing(e)
        b, p, q = e
        z = _fold(p * _log_of(b) * γ / α * _exp_base(b, _fold(q - p * β / α)))
        return Tuple(_Sol(x, _fold(-_div_log(ω, b) / p - β / α)) for ω ∈ _W(z))
    end
    lt = _log_term(other)
    isnothing(lt) && return nothing
    k, w, b = lt
    cs = _linear(w, x)
    isnothing(cs) && return nothing
    q, p = cs
    γ = _fold(k / _log_of(b)) # natural logarithm coefficient
    κ = _fold(α / (p * γ)) # t = p⋅x + q: (κt)⋅e^(κt) = κ⋅e^C
    C = _fold(α * q / (p * γ) - β / γ)
    Tuple(_Sol(x, _fold((ω / κ - q) / p)) for ω ∈ _W(_fold(κ * _apply(exp, C))))
end

# (a₀, a₁) when t = a₀ + a₁⋅x, a₁ ≠ 0
function _linear(t, x)
    cs = _poly_coeffs(t, x)
    (isnothing(cs) || length(cs) != 2 || _iszero(cs[2])) && return nothing
    (cs[1], cs[2])
end

# (b, p, q) when E = b^(p⋅x + q) (b = ℯ for exp), b > 0
function _exponential(E, x)
    iscall(E) || return nothing
    if operation(E) == exp
        b, L = ℯ, only(arguments(E))
    elseif operation(E) == (^) && !_has(first(arguments(E)), x)
        b, L = arguments(E)
        b = _value(b)
        (isnothing(b) || !isreal(b) || b ≤ 0) && return nothing
    else
        return nothing
    end
    cs = _linear(L, x)
    isnothing(cs) && return nothing
    (b, cs[2], cs[1])
end
_log_of(b) = b == ℯ ? _fold(1) : _apply(log, b)

# ω / log(b), exact when ω = log(m) with m a power of b
function _div_log(ω, b)
    b == ℯ && return ω
    _iszero(ω) && return _fold(0)
    if iscall(ω) && operation(ω) == log && _isexact(_value(only(arguments(ω))))
        return _log_ratio(only(arguments(ω)), b)
    end
    _fold(ω / _log_of(b))
end

# the real values W₀(z), W₋₁(z); exact when W(z) is a small rational or the
# logarithm of one (checked numerically), symbolic otherwise
function _W(z)
    zv = _value(z)
    isnothing(zv) && return (lambertw(z), lambertw(z, -1)) # with conditions
    isreal(zv) || return ()
    zv, e⁻¹ = real(zv), exp(-1)
    zv < -e⁻¹ - 1e-14 && return ()
    ks = zv < 0 && zv > -e⁻¹ + 1e-14 ? (0, -1) : (0,)
    Tuple(_W(z, zv, k) for k ∈ ks)
end
function _W(z, zv, k)
    k == 0 && iszero(zv) && return _fold(0)
    wv = lambertw(max(zv, -exp(-1)), k)
    _has_float(z) && return _fold(wv)
    ok(w) = isapprox(w * exp(w), zv; atol = 1e-13, rtol = 1e-13)
    q = rationalize(wv; tol = 1e-12)
    denominator(q) ≤ 12 && ok(float(q)) && return _fold(q)
    m = rationalize(exp(wv); tol = 1e-12)
    denominator(m) ≤ 12 && numerator(m) ≤ 10^4 && ok(log(float(m))) && return _apply(log, _constant(m))
    k == 0 ? lambertw(z) : lambertw(z, k)
end

## ---- trig identities
# Write ex, built from trig functions of integer multiples of one angle u,
# as a polynomial P(S, C) in S = sin(u), C = cos(u) (the numerator, for tan,
# sec, ...). With C² = 1 - S², P = A(S) + C⋅B(S) (or with S, C swapped):
# solve for sin(u) or cos(u) when one of them is absent, factor out C when
# A = 0, else square: A² = (1 - S²)⋅B², choosing asin(σ) or π - asin(σ) by
# the sign of C = -A/B.
const _trigfns = (sin, cos, tan, sec, csc, cot)
function _trig_identity(ex, x)
    atoms = Any[]
    _find_all!(t -> iscall(t) && operation(t) ∈ _trigfns && _has(t, x), ex, atoms)
    isempty(atoms) && return nothing
    kws = map(a -> _split_int(only(arguments(a))), atoms)
    w = last(first(kws))
    all(kw -> isequal(last(kw), w), kws) || return nothing
    g = reduce(gcd, first.(kws))
    u = isone(g) ? w : g * w
    frac = _sc(ex, x, w, g)
    isnothing(frac) && return nothing
    P = first(frac)
    A₁, B₁ = _reduce_sc(P, 1) # A(S) + C⋅B(S)
    A₂, B₂ = _reduce_sc(P, 2) # A(C) + S⋅B(C)
    z(cs) = all(_iszero, cs)
    z(B₁) && return _trig_roots(A₁, sin(u), x)
    z(B₂) && return _trig_roots(A₂, cos(u), x)
    z(A₁) && return _branches((_isolate(cos(u), 0, x), _trig_roots(B₁, sin(u), x)))
    z(A₂) && return _branches((_isolate(sin(u), 0, x), _trig_roots(B₂, cos(u), x)))
    # A + C⋅B = 0 ⟹ A² - (1 - S²)⋅B² = 0
    Q = _poly_add(_poly_mul(A₁, A₁), _poly_mul(Any[-1, 0, 1], _poly_mul(B₁, B₁)))
    σs = _poly_roots(_trim(Q))
    isnothing(σs) && return nothing
    out = Any[]
    for σ ∈ σs
        σv, Av, Bv = _value(σ), _value(_horner(A₁, σ)), _value(_horner(B₁, σ))
        if isnothing(σv) || isnothing(Av) || isnothing(Bv) || abs(Bv) < 1e-12
            push!(out, _isolate(sin(u), σ, x))
        elseif _interval_mode() # all branches; the check drops the wrong ones
            push!(out, _isolate(sin(u), σ, x))
        elseif isreal(σv) && abs(σv) ≤ 1
            θ = _apply(asin, σ) # C = -A/B decides the quadrant
            θ = -Av / Bv ≥ 0 ? θ : _pi_fold(_fold((σv ≥ 0 ? 1 : -1) * _constant(π) - θ)) # in (-π, π]
            push!(out, _isolate(u, θ, x))
        end
    end
    _branches(out)
end

# solve the polynomial with coefficients cs (in increasing degree) for f(u)
function _trig_roots(cs, f, x)
    cs = _trim(cs)
    length(cs) == 1 && return _iszero(only(cs)) ? nothing : ()
    ts = _poly_roots(cs)
    isnothing(ts) && return nothing
    _branches((_isolate(f, t, x) for t ∈ ts))
end

# sin(m⋅u), cos(m⋅u) as polynomials in S, C
function _sin_cos(m)
    m < 0 && ((s, c) = _sin_cos(-m); return (_bp_mul(_bp(-1), s), c))
    s, c = _bp(0), _bp(1)
    S, C = Dict{Tuple{Int,Int},Any}((1, 0) => 1), Dict{Tuple{Int,Int},Any}((0, 1) => 1)
    for _ ∈ 1:m # sin((k+1)u) = sin(ku)C + cos(ku)S, cos((k+1)u) = cos(ku)C - sin(ku)S
        s, c = _bp_add(_bp_mul(s, C), _bp_mul(c, S)), _bp_add(_bp_mul(c, C), _bp_mul(_bp(-1), _bp_mul(s, S)))
    end
    (s, c)
end

# P(S, C) as A(T) + U⋅B(T) using U² = 1 - T²; T, U = S, C (t = 1) or C, S (t = 2)
function _reduce_sc(P, t)
    A, B = Any[0], Any[0]
    for ((i, j), c) ∈ P
        a, b = t == 1 ? (i, j) : (j, i)
        term = _poly_mul(Any[c], _poly_mul(Any[(k == a ? 1 : 0) for k ∈ 0:a], _poly_pow(Any[1, 0, -1], b ÷ 2)))
        isodd(b) ? (B = _poly_add(B, term)) : (A = _poly_add(A, term))
    end
    (A, B)
end

## ---- absolute value

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
_rat_coeffs(ex, x) = _ratfun(ex, e -> !_has(e, x),
                             e -> isequal(e, x) ? (Any[0, 1], Any[1]) : nothing, _dense_algebra)

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

## ------- Polynomial/rational function specific functions

# values of ρ(𝓃) in [a, b] over the integers
function _enumerate(ρ, a, b, tol)
    cs = _poly_coeffs(ρ, _𝓃)
    if !isnothing(cs) && length(cs) == 2 && all(c -> !isnothing(_value(c)), cs)
        α, β = cs # ρ = α + β⋅𝓃
        αv, βv = _value(α), _value(β)
        lo, hi = minmax((a - tol - αv) / βv, (b + tol - αv) / βv)
        ns = ceil(Int, lo):floor(Int, hi)
        length(ns) > _max_solutions && (@warn "more than $(_max_solutions) solutions in the interval; truncated";
                                       ns = first(ns, _max_solutions))
        return [_pi_fold(_fold(α + β * n)) for n ∈ ns]
    end
    # not linear in 𝓃: scan outward in each direction, stopping after a run
    # of values outside [a, b] (or undefined)
    out = Any[]
    for sgn ∈ (1, -1)
        misses = 0
        for n ∈ (sgn == 1 ? 0 : 1):_max_solutions
            ρₙ = _pi_fold(_expand_poly(_subst(ρ, [_𝓃], [_constant(sgn * n)], _𝓃)))
            v = _value_at(ρₙ)
            if !isnothing(v) && isreal(v) && a - tol ≤ real(v) ≤ b + tol
                push!(out, ρₙ)
                misses = 0
            else
                (misses += 1) > 100 && break
            end
        end
    end
    out
end

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
    ρs = _poly_roots(map(_fold, G))
    _branches((isnothing(ρs) ? nothing : Tuple(_Sol(x, ρ) for ρ ∈ ρs), _isolate(ex′, zero(x), x)))
end

# (numerator, denominator) of ex as polynomials in S, C (Dicts (i, j) => coeff
# of SⁱCʲ); trig arguments are k⋅w with g dividing k
function _sc(ex, x, w, g)
    function atom(e)
        (iscall(e) && operation(e) ∈ _trigfns) || return nothing
        k, w′ = _split_int(only(arguments(e)))
        isequal(w′, w) || return nothing # not a rational function of sin(u), cos(u)
        sₘ, cₘ = _sin_cos(k ÷ g)
        one, op = _bp(1), operation(e)
        op == sin ? (sₘ, one) : op == cos ? (cₘ, one) : op == tan ? (sₘ, cₘ) :
            op == cot ? (cₘ, sₘ) : op == sec ? (one, cₘ) : (one, sₘ)
    end
    _ratfun(ex, e -> !_has(e, x), atom, _bp_algebra)
end

# bivariate polynomials in S, C
_bp(c) = Dict{Tuple{Int,Int},Any}((0, 0) => c)
function _bp_add(a, b)
    out = copy(a)
    for (k, v) ∈ b
        out[k] = haskey(out, k) ? _fold(out[k] + v) : v
    end
    filter!(((_, v),) -> !_iszero(v), out)
    isempty(out) ? _bp(0) : out
end
function _bp_mul(a, b)
    out = Dict{Tuple{Int,Int},Any}()
    for ((i, j), v) ∈ a, ((k, l), w) ∈ b
        key = (i + k, j + l)
        out[key] = _fold(get(out, key, 0) + v * w)
    end
    filter!(((_, v),) -> !_iszero(v), out)
    isempty(out) ? _bp(0) : out
end
_bp_pow(a, n) = n == 0 ? _bp(1) : foldl(_bp_mul, ntuple(_ -> a, n))
const _bp_algebra = (constant = _bp, add = _bp_add, mul = _bp_mul, pow = _bp_pow,
                     neg = p -> _bp_mul(_bp(-1), p))

## ---- utilities
# all subexpressions satisfying pred
function _find_all!(pred, ex, out)
    pred(ex) && (any(a -> isequal(a, ex), out) || push!(out, ex))
    iscall(ex) && foreach(a -> _find_all!(pred, a, out), arguments(ex))
    out
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
