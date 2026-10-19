# can we solve equations?
# this shows *easy* ones can be solved.
import CommonSolve
import CommonSolve: solve

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
    solve(eq::SymbolicEquation, x)

Very *simple* symbolic equations can be solved with the unexported `solve` method.

The equation is rearranged by moving terms free of `x` to the right-hand side
and applying inverse functions (e.g. `exp(u) ~ r` becomes `u ~ log(r)`,
`abs(u) ~ r` becomes `u ~ ±r`). A product equal to `0` is solved factor by
factor. Polynomials in `x` are solved when they are linear or quadratic, after
factoring out powers of `x`, when they are a polynomial in `xᵏ`
(e.g. `x^4 - 5x^2 + 4`), or when their numeric rational coefficients allow
roots to be found by the rational root theorem.

Answers are kept exact where possible: rational values stay rational and
other values are left as symbolic numbers, e.g. `x^2 ~ 8` gives `x ~ ±2⋅sqrt(2)`,
`exp(x) ~ 2` gives `x ~ log(2)`, `2^x ~ 8` gives `x ~ 3` and `sin(x) ~ 1//2`
gives `x ~ π/6` (standard values of `asin`, `acos` and `atan` are recognized).
Calling a symbolic number, e.g. `solve(x^2 ~ 2, x)[2].rhs()`, gives its
floating point value. When the equation has floating point values, so does
the answer.

Some further rewriting is done:

* an injective function on both sides is removed, e.g. `log(u) ~ log(v)`
  becomes `u ~ v`; exponentials on both sides are compared through their
  logarithms, e.g. `2^x ~ 3^(x+1)`;
* sums of logarithms are combined, `log(u) + log(v) ~ r` becomes `log(u⋅v) ~ r`;
* rational functions, e.g. `1/x + 1/(x+1) ~ 1`, are solved through their
  numerator, discarding roots of the denominator;
* a polynomial in some `g(x)`, e.g. `exp(2x) - 3exp(x) + 2 ~ 0` or
  `sqrt(x) + x ~ 6`, is solved for `g(x)` first.

When the equation has no symbolic parameters, numeric solutions are checked in
the original equation and extraneous ones are discarded.

Return value:

* an equation `x ~ r` when there is a single solution;
* a tuple of equations, e.g. `(x ~ r₁, x ~ r₂)`, when there are more
  (quadratics, even powers);
* `()` when numeric values show there are no real solutions;
* `nothing` when `x` does not appear in the equation (or cancels out);
* an equation whose left-hand side is not `x` when `x` could not be isolated.

!!! warning
    Inverse functions are applied without regard to domain or range
    restrictions and only the *principal value* is returned. For example,
    `sin(x) ~ a` gives `x ~ asin(a)` and not the infinitely many other
    solutions; `sqrt(u) ~ r` gives `u ~ r^2`, which may introduce extraneous
    solutions when `r` is negative. Symbolic answers are not checked; check them.

```{julia}
@symbolic w p; @symbolic h  # two variables, one parameter
import SimpleExpressions: solve, D
constraint = p ~ 2w + 2h
A = w * h

u = solve(constraint, h)
A = A(u) # use equation in replacement
v = solve(D(A, w) ~ 0, w)
```
"""
function CommonSolve.solve(eq::SymbolicEquation, x::𝑉)
    l, r = eq.lhs, eq.rhs
    sols = _solve(l, r, x)
    isnothing(sols) && return nothing
    sols = _verify(sols, l, r, x)
    length(sols) == 1 ? only(sols) : sols
end

function _solve(l, r, x)
    if contains(l, x) && contains(r, x)
        m = _match_sides(l, r, x)
        !isnothing(m) && return _solve(m..., x)
        l, r = l - r, zero(x)
    elseif contains(r, x)
        l, r = r, l
    end
    contains(l, x) || return nothing
    _isolate(l, r, x)
end

# drop numeric solutions which do not satisfy l ~ r
function _verify(sols, l, r, x)
    fs = free_symbols(l - r)
    (fs.x == (x,) && isempty(fs.p)) || return sols
    sols = Tuple(s for s ∈ sols if !(isequal(s.lhs, x) && is_number(s.rhs)) ||
                     _satisfies(l, r, s.rhs()))
    # sort when all are numeric
    all(s -> isequal(s.lhs, x) && is_number(s.rhs), sols) || return sols
    Tuple(sort!(collect(sols), by = s -> s.rhs()))
end

function _satisfies(l, r, v)
    lv, rv = try
        l(v), r(v)
    catch err
        err isa DomainError || rethrow()
        return false
    end
    isfinite(lv) && isfinite(rv) && isapprox(lv, rv; atol = 1e-8, rtol = 1e-8)
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
            isequal(a, c) && !contains(a, x) && return (b, d) # aᵘ ~ aᵛ
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
        if !contains(f, x)
            _domain_ok(log, f) || return nothing
            out += _apply(log, f)
        elseif iscall(f) && operation(f) == exp
            out += only(arguments(f))
        elseif iscall(f) && operation(f) == (^) && !contains(first(arguments(f)), x)
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
        i = findfirst(is_number, args)
        if !isnothing(i)
            c = _fold(-args[i])
            rest = prod(args[j] for j ∈ eachindex(args) if j != i)
            return isone(c) ? rest : c * rest
        end
    end
    -t
end

# log(u) ↦ u, c⋅log(u) ↦ uᶜ; else `nothing`
function _log_arg(t)
    iscall(t) || return nothing
    operation(t) == log && return only(arguments(t))
    if operation(t) == (*) && length(arguments(t)) == 2
        a, b = arguments(t)
        is_number(b) && ((a, b) = (b, a))
        is_number(a) && iscall(b) && operation(b) == log && return only(arguments(b))^_fold(a)
    end
    nothing
end

CommonSolve.init(eq::SymbolicEquation) = throw(ArgumentError("Must specify variable to solve for"))

# Evaluate a symbolic number when nothing is lost: a rational value, or an
# expression which already involves floating point values. Exact values such
# as `sqrt(2)`, `π/6` or `log(2)` are kept symbolic.
_fold(u::Rational) = isone(denominator(u)) ? numerator(u) : u
_fold(u::Number) = u
function _fold(u)
    is_number(u) || return u
    u isa SymbolicNumber && return _fold(u())
    _has_float(u) ? u() : u
end
_has_float(u::SymbolicNumber) = u() isa AbstractFloat
_has_float(u) = iscall(u) && any(_has_float, arguments(u))
_S(r::Number) = SymbolicNumber(r)
_S(r) = r
_isexact(v) = v isa Union{Integer, Rational}
_value(u::Number) = u
_value(u) = is_number(u) ? u() : nothing
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
    v = _value(r)
    if !isnothing(v)
        f == log && (isone(v) || v == ℯ) && return Int(isone(v) ? 0 : 1)
        if iszero(v)
            y = f(0.0)
            (iszero(y) || isone(y)) && return Int(y)
        end
        k = _special_angle(f, v)
        !isnothing(k) && return iszero(k) ? 0 : _fold(k * SymbolicNumber(π))
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
        if op == (*) && length(args) == 2 && is_number(first(args))
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
    u == x && return (x ~ _fold(r),)
    iscall(u) || return (u ~ r,)
    op, args = operation(u), arguments(u)

    if op == (+) || op == (*)
        i = _only_index(a -> contains(a, x), args)
        if !isnothing(i)
            rest = (args[j] for j in eachindex(args) if j != i)
            r′ = op == (+) ? r - sum(rest) : r / prod(rest)
            return _isolate(args[i], r′, x)
        end
        if op == (*) && _iszero(r) # zero-product rule
            return _branches((_isolate(a, r, x) for a ∈ args if contains(a, x)))
        end
        if op == (+)
            xs = [a for a ∈ args if contains(a, x)]
            rest = [a for a ∈ args if !contains(a, x)]
            r′ = isempty(rest) ? r : r - sum(rest)
            us = map(_log_arg, xs) # log(u) + log(v) ~ r′
            all(!isnothing, us) && return _isolate(log(prod(us)), r′, x)
            if length(xs) == 2 && _iszero(r′) # f(u) - f(v) ~ 0
                m = _match_sides(xs[1], _neg(xs[2]), x)
                !isnothing(m) && return _solve(m..., x)
            end
        end
    elseif op == (^)
        a, b = args
        if !contains(b, x)
            return _isolate_power(a, b, r, x)
        elseif !contains(a, x)  # a^u ~ r
            _domain_ok(log, r) || return ()
            return _isolate(b, _log_ratio(r, a), x)
        end
    elseif length(args) == 1
        a = only(args)
        if op == abs || op == cosh  # two branches
            _maybe_nonneg(r) && _domain_ok(op == abs ? identity : acosh, r) || return ()
            return _isolate_pm(a, op == abs ? _fold(r) : _apply(acosh, r), x)
        end
        if op ∈ _injective && iscall(r) && operation(r) == op # f(u) ~ f(v)
            return _isolate(a, only(arguments(r)), x)
        end
        f⁻¹ = _inverse(op)
        if !isnothing(f⁻¹)
            op == sqrt && !_maybe_nonneg(r) && return ()
            _domain_ok(f⁻¹, r) || return ()
            return _isolate(a, _apply(f⁻¹, r), x)
        end
    end

    # stuck; try a rational function, then a substitution
    nd = _rat_coeffs(u - r, x)
    if !isnothing(nd)
        num, den = nd
        length(num) == 1 && return nothing # x cancels
        sols = _poly_roots(num, x)
        isnothing(sols) && return (u ~ r,)
        return Tuple(s for s ∈ sols if !_vanishes(den, s.rhs))
    end
    sols = _substitute(u - r, x)
    isnothing(sols) ? (u ~ r,) : sols
end

# numerator, denominator coefficients of ex as a rational function of x
function _rat_coeffs(ex, x)
    contains(ex, x) || return (Any[ex], Any[1])
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
    (isnothing(c) || any(isnothing, vs)) && return false
    iszero(_horner(vs, c))
end

# Solve ex ~ 0 when ex is a polynomial in some g(x) (e.g. exp(x), sqrt(x)):
# every term with x is a power gᵠ, written (g, q) below.
const _𝓎 = SymbolicVariable(:𝓎)
function _substitute(ex, x)
    atoms = _atoms!(Any[], ex, x)
    gqs = map(a -> _as_power(a, x), atoms)
    g = first(first(gqs))
    all(gq -> isequal(first(gq), g) && last(gq) isa Union{Integer, Rational}, gqs) ||
        return nothing
    q = reduce(gcd, last.(gqs))
    gᵠ = isone(q) ? g : g^q
    ex′ = _subst(ex, atoms, [_𝓎^_fold(Rational(qᵢ // q)) for (_, qᵢ) ∈ gqs], x)
    contains(ex′, x) && return nothing
    ys = _isolate(ex′, zero(x), _𝓎)
    isnothing(ys) && return nothing
    all(s -> isequal(s.lhs, _𝓎), ys) || return nothing
    _branches((_isolate(gᵠ, s.rhs, x) for s ∈ ys))
end

# maximal non-polynomial subexpressions containing x
function _atoms!(out, ex, x)
    contains(ex, x) || return out
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
        contains(b, x) && return (a, 1)
        k, w = _split_int(c)
        return (b^w, k)                   # bᵏʷ
    elseif op == exp
        k, w = _split_int(only(args))
        return (exp(w), k)                # exp(k⋅w)
    end
    (a, 1)
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
    contains(ex, x) || return ex
    i = findfirst(a -> isequal(a, ex), atoms)
    !isnothing(i) && return subs[i]
    iscall(ex) || return ex
    maketerm(AbstractSymbolic, operation(ex), map(a -> _subst(a, atoms, subs, x), arguments(ex)), nothing)
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
    k > 0 && return _branches(((x ~ 0,), _poly_roots(cs[k+1:end], x)))

    n = length(cs) - 1
    n == 0 && return ()
    if n == 1
        a₀, a₁ = cs
        return (x ~ _fold(-a₀ / a₁),)
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

    nothing
end

function _quadratic_roots(cs, x)
    a₀, a₁, a₂ = cs
    Δ = _fold(a₁^2 - 4 * a₂ * a₀)
    v = _value(Δ)
    if !isnothing(v)
        v < 0 && return ()
        iszero(v) && return (x ~ _fold(-a₁ / (2a₂)),)
    end
    c = _value(2a₂)
    if _isexact(v) && _isexact(c) # p ± k⋅√m exactly
        p = _fold(-a₁ / (2a₂))
        k, m = _surd(v, 2)
        isone(m) && return (x ~ _fold(p - k // c), x ~ _fold(p + k // c))
        q = _fold(k // c) * sqrt(SymbolicNumber(m))
        iszero(p) && return (x ~ -q, x ~ q)
        return (x ~ p - q, x ~ p + q)
    end
    sΔ = _root(Δ, 2)
    (x ~ _fold((-a₁ - sΔ) / (2a₂)),
     x ~ _fold((-a₁ + sΔ) / (2a₂)))
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
    n == 1 && return SymbolicNumber(m)
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
    length(qs) > 3 && return nothing
    isempty(roots) && return nothing  # leave to quadratic formula
    sols = (x ~ _fold(Rational{Int}(c)) for c ∈ sort!(roots))
    length(qs) < 3 && return _branches((sols,))
    _branches((sols, _quadratic_roots(map(_fold ∘ Rational{Int}, qs), x)))
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
