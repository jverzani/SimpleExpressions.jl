function limit(ex, x, c; dir=:+)
    Main.eval(Gruntz.gruntz_limit(convert(Expr,ex), Symbol(x), c; dir))
end


"""
    Gruntz

Limits of univariate real expressions using Gruntz's algorithm
(D. Gruntz, "On Computing Limits in a Symbolic Manipulation System", 1996),
for any term type implementing TermInterface.jl
(`iscall`, `operation`, `arguments`, `maketerm`), e.g. `Expr` or Symbolics terms.

    gruntz_limit(expr, x, x0 = Inf; dir = :+)

* `x0` may be `Inf`, `-Inf`, or a finite value / term not containing `x`.
* `dir` is `:+`, `:-` or `:both` (only relevant for finite `x0`).
* Returns `Inf`, `-Inf`, a number (Int / Rational), or a term of the input's type.

Supported: `+ - * / ^ sqrt cbrt inv exp log sin cos tan sinh cosh tanh`, rational
and symbolic exponents. Not supported: limits that do not exist because of
bounded oscillation (e.g. `sin(x)` at Inf) -> `GruntzError`.

Design: terms are first converted to an internal canonical form (sums of monomials
with rational coefficients/exponents over "atoms": variables, exp/log/sin/cos
applications, and multi-term bases). This gives the zero-recognition that Gruntz's
algorithm needs, without depending on a particular CAS. Results are converted back
with `maketerm`.
"""
module Gruntz

# Written by claude AI from a simple prompt
#=
The algorithm for find the limit of a univariate scalar-valued function by Gruntz is implemented in several open-source programming languages. I want an implementation in Julia that works for symbolic terms that follow the TermInterface.jl interface. (https://github.com/JuliaSymbolics/TermInterface.jl). Can you create such functionality?
=#

using TermInterface

export gruntz_limit, GruntzError

const Q = Rational{BigInt}
const HUGE = Q(10)^12   # "exact" precision marker for power series

struct GruntzError <: Exception
    msg::String
end
Base.showerror(io::IO, e::GruntzError) = print(io, "GruntzError: ", e.msg)

struct NeedMorePrecision <: Exception end
struct Infinity
    sign::Int
end

# ---------------------------------------------------------------------------
# Canonical form
# ---------------------------------------------------------------------------
struct Atom
    kind::Symbol          # :leaf, :fun (exp/log/sin/cos), :base (multi-term sum used as base)
    head::Any
    args::Vector{Any}     # Vector of Sum
    key::String
end
Base.:(==)(a::Atom, b::Atom) = a.key == b.key
Base.hash(a::Atom, h::UInt) = hash(a.key, h)

struct Mono
    fs::Vector{Pair{Atom,Q}}   # sorted by atom key, exponents nonzero
    key::String
end
Mono(fs::Vector{Pair{Atom,Q}}) = Mono(fs, join(("$(a.key)^$(e)" for (a, e) in fs), "*"))
Base.:(==)(a::Mono, b::Mono) = a.key == b.key
Base.hash(a::Mono, h::UInt) = hash(a.key, h)

struct Sum
    t::Dict{Mono,Q}
end
Base.iszero(s::Sum) = isempty(s.t)

const ONEMONO = Mono(Pair{Atom,Q}[])
zeroS() = Sum(Dict{Mono,Q}())
constS(c) = iszero(c) ? zeroS() : Sum(Dict{Mono,Q}(ONEMONO => Q(c)))
oneS() = constS(1)
monoS(m::Mono, c::Q) = Sum(Dict{Mono,Q}(m => c))
atomS(a::Atom) = monoS(Mono(Pair{Atom,Q}[a => Q(1)]), Q(1))

skey(s::Sum) = isempty(s.t) ? "0" : join(sort!(String["$(c)*$(m.key)" for (m, c) in s.t]), "+")

leafatom(obj) = Atom(:leaf, obj, Any[], "L:" * string(obj))
funatom(h::Symbol, args::Vector{Sum}) =
    Atom(:fun, h, Any[args...], string(h, "(", join([skey(a) for a in args], ","), ")"))
baseatom(s::Sum) = Atom(:base, nothing, Any[s], "B(" * skey(s) * ")")

const WLEAF = leafatom(:__gruntz_w__)

mono(d::Dict{Atom,Q}) = Mono(sort!(collect(d); by = p -> p.first.key))

function mulMono(a::Mono, b::Mono)
    d = Dict{Atom,Q}(a.fs)
    for (at, e) in b.fs
        v = get(d, at, zero(Q)) + e
        iszero(v) ? delete!(d, at) : (d[at] = v)
    end
    mono(d)
end
powMono(m::Mono, q::Q) = Mono(Pair{Atom,Q}[a => e * q for (a, e) in m.fs])

function addS(a::Sum, b::Sum)
    d = copy(a.t)
    for (m, c) in b.t
        v = get(d, m, zero(Q)) + c
        iszero(v) ? delete!(d, m) : (d[m] = v)
    end
    Sum(d)
end
scaleS(a::Sum, c::Q) = iszero(c) ? zeroS() : Sum(Dict{Mono,Q}(m => c * v for (m, v) in a.t))
negS(a::Sum) = scaleS(a, Q(-1))
subS(a::Sum, b::Sum) = addS(a, negS(b))
function mulS(a::Sum, b::Sum)
    d = Dict{Mono,Q}()
    for (m1, c1) in a.t, (m2, c2) in b.t
        m = mulMono(m1, m2)
        v = get(d, m, zero(Q)) + c1 * c2
        iszero(v) ? delete!(d, m) : (d[m] = v)
    end
    Sum(d)
end

function ratconst(s::Sum)
    isempty(s.t) && return Q(0)
    length(s.t) == 1 || return nothing
    m, c = first(s.t)
    isempty(m.fs) ? c : nothing
end

hasleaf(a::Atom, k::String) =
    a.kind === :leaf ? a.key == k : any(u -> hasleaf(u::Sum, k), a.args)
hasleaf(s::Sum, k::String) = any(m -> any(p -> hasleaf(p.first, k), m.fs), keys(s.t))

# --- rational powers --------------------------------------------------------
function iroot(n::BigInt, k::Int)
    n == 0 && return big(0)
    r = round(BigInt, BigFloat(n)^inv(BigFloat(k)))
    r^k == n ? r : nothing
end

function ratpow(c::Q, e::Q)
    if isone(denominator(e))
        n = numerator(e)
        abs(n) > 100_000 && throw(GruntzError("exponent too large"))
        return c^Int(n)
    end
    c < 0 && throw(GruntzError("complex power of a negative number"))
    p, d = Int(numerator(e)), Int(denominator(e))
    rn = iroot(numerator(c), d)
    rd = iroot(denominator(c), d)
    (rn === nothing || rd === nothing) && return nothing
    (rn // rd)^p
end

function powS(s::Sum, q::Q)
    iszero(q) && return oneS()
    if isempty(s.t)
        q > 0 && return zeroS()
        throw(GruntzError("division by zero"))
    end
    if length(s.t) == 1
        m, c = first(s.t)
        mm = powMono(m, q)
        r = ratpow(c, q)
        if r === nothing
            mm = mulMono(mm, Mono(Pair{Atom,Q}[baseatom(constS(c)) => q]))
            r = Q(1)
        end
        return monoS(mm, r)
    end
    if isone(denominator(q)) && 0 < q <= 8
        res = s
        for _ in 2:Int(q)
            res = mulS(res, s)
        end
        return res
    end
    monoS(Mono(Pair{Atom,Q}[baseatom(s) => q]), Q(1))
end

# --- exp / log / trig constructors -------------------------------------------
function mkexp(u::Sum)
    res = oneS()
    for (m, c) in u.t
        a1 = length(m.fs) == 1 ? m.fs[1] : nothing
        f = if a1 !== nothing && a1.second == 1 && a1.first.kind === :fun && a1.first.head === :log
            powS(a1.first.args[1]::Sum, c)                      # exp(c*log y) = y^c
        else
            a = funatom(:exp, Sum[monoS(m, Q(1))])
            monoS(Mono(Pair{Atom,Q}[a => c]), Q(1))             # exp(c*m) = exp(m)^c
        end
        res = mulS(res, f)
    end
    res
end

function factorint(n::BigInt)
    f = Pair{BigInt,Int}[]
    p = big(2)
    while p * p <= n && p < 1_000_000
        k = 0
        while n % p == 0
            n ÷= p
            k += 1
        end
        k > 0 && push!(f, p => k)
        p += (p == 2 ? 1 : 2)
    end
    n > 1 && push!(f, n => 1)
    f
end

function lograt(c::Q)
    res = zeroS()
    for (p, k) in factorint(numerator(c))
        res = addS(res, scaleS(atomS(funatom(:log, Sum[constS(p)])), Q(k)))
    end
    for (p, k) in factorint(denominator(c))
        res = subS(res, scaleS(atomS(funatom(:log, Sum[constS(p)])), Q(k)))
    end
    res
end

function logatom(a::Atom)
    if a.kind === :fun && a.head === :exp
        a.args[1]::Sum
    elseif a.kind === :base
        mklog(a.args[1]::Sum)
    else
        atomS(funatom(:log, Sum[atomS(a)]))
    end
end

function mklog(s::Sum)
    isempty(s.t) && throw(GruntzError("log(0)"))
    if length(s.t) == 1
        m, c = first(s.t)
        c < 0 && throw(GruntzError("log of a negative number"))
        res = lograt(c)
        for (a, e) in m.fs
            res = addS(res, scaleS(logatom(a), e))
        end
        return res
    end
    atomS(funatom(:log, Sum[s]))
end

mksin(s::Sum) = isempty(s.t) ? zeroS() : atomS(funatom(:sin, Sum[s]))
mkcos(s::Sum) = isempty(s.t) ? oneS() : atomS(funatom(:cos, Sum[s]))

function mkfun(h::Symbol, a::Vector{Sum})
    h === :exp ? mkexp(a[1]) :
    h === :log ? mklog(a[1]) :
    h === :sin ? mksin(a[1]) :
    h === :cos ? mkcos(a[1]) : throw(GruntzError("unsupported function $h"))
end

function mkpow(a::Sum, b::Sum)
    r = ratconst(b)
    r !== nothing ? powS(a, r) : mkexp(mulS(b, mklog(a)))
end

# --- substitution --------------------------------------------------------------
function subs_atoms(s::Sum, f)::Sum
    res = zeroS()
    for (m, c) in s.t
        term = constS(c)
        for (a, e) in m.fs
            r = f(a)
            r === nothing && (r = rebuild(a, f))
            term = mulS(term, powS(r, e))
        end
        res = addS(res, term)
    end
    res
end
function rebuild(a::Atom, f)
    a.kind === :leaf && return atomS(a)
    a.kind === :base && return subs_atoms(a.args[1]::Sum, f)
    mkfun(a.head, Sum[subs_atoms(u::Sum, f) for u in a.args])
end

# --- numeric evaluation (for signs of constants) -----------------------------
function numval(a::Atom)
    if a.kind === :leaf
        a.head isa Number || throw(GruntzError(
            "cannot decide the sign of an expression containing the free symbol `$(a.head)`"))
        return BigFloat(a.head)
    elseif a.kind === :base
        return numval(a.args[1]::Sum)
    end
    v = numval(a.args[1]::Sum)
    a.head === :exp ? exp(v) : a.head === :log ? log(v) : a.head === :sin ? sin(v) : cos(v)
end
function numval(s::Sum)
    acc = BigFloat(0)
    for (m, c) in s.t
        v = BigFloat(numerator(c)) / BigFloat(denominator(c))
        for (a, e) in m.fs
            v *= numval(a)^(BigFloat(numerator(e)) / BigFloat(denominator(e)))
        end
        acc += v
    end
    acc
end
function csign(s::Sum)::Int
    isempty(s.t) && return 0
    r = ratconst(s)
    r !== nothing && return Int(sign(r))
    v = try
        numval(s)
    catch err
        err isa GruntzError ? rethrow() : throw(GruntzError("cannot evaluate constant $(skey(s))"))
    end
    abs(v) < big"1e-60" ? 0 : (v > 0 ? 1 : -1)
end

# ---------------------------------------------------------------------------
# Truncated power series in w with rational exponents
# ---------------------------------------------------------------------------
struct PS
    c::Dict{Q,Sum}   # exponent -> nonzero coefficient (an x-expression)
    prec::Q          # exact for exponents < prec
end
val(a::PS) = isempty(a.c) ? a.prec : minimum(keys(a.c))
onePS() = PS(Dict{Q,Sum}(Q(0) => oneS()), HUGE)

function trunc_ps(c::Dict{Q,Sum}, prec::Q)
    d = Dict{Q,Sum}()
    for (k, v) in c
        (k < prec && !isempty(v.t)) && (d[k] = v)
    end
    PS(d, prec)
end
function addPS(a::PS, b::PS)
    c = copy(a.c)
    for (k, v) in b.c
        c[k] = haskey(c, k) ? addS(c[k], v) : v
    end
    trunc_ps(c, min(a.prec, b.prec))
end
scalePS(a::PS, s::Sum) = trunc_ps(Dict{Q,Sum}(k => mulS(v, s) for (k, v) in a.c), a.prec)
shiftPS(a::PS, d::Q) = PS(Dict{Q,Sum}(k + d => v for (k, v) in a.c), a.prec + d)
function mulPS(a::PS, b::PS)
    prec = min(a.prec + val(b), b.prec + val(a))
    c = Dict{Q,Sum}()
    for (ka, ca) in a.c, (kb, cb) in b.c
        k = ka + kb
        k >= prec && continue
        v = mulS(ca, cb)
        c[k] = haskey(c, k) ? addS(c[k], v) : v
    end
    trunc_ps(c, prec)
end

binomq(q::Q, k::Int) = (r = one(Q); for j in 0:k-1; r *= (q - j) // (j + 1); end; r)

# sum_{k>=kmin} coef(k) u^k, u having positive valuation; relative target `Trel`
function polyPS(u::PS, coef, kmin::Int, Trel::Q)
    acc = PS(Dict{Q,Sum}(), min(Trel, u.prec))
    K = isempty(u.c) ? kmin : max(kmin, Int(ceil(BigInt, Trel / val(u))))
    pw = onePS()
    for k in 0:K
        if k >= kmin
            ck = coef(k)
            iszero(ck) || (acc = addPS(acc, scalePS(pw, constS(ck))))
        end
        k < K && (pw = mulPS(pw, u))
    end
    acc
end

# a = c0 * w^v * (1 + r); returns (v, c0, r)
function split_lead(a::PS)
    isempty(a.c) && throw(NeedMorePrecision())
    v = val(a)
    c0 = a.c[v]
    ic0 = powS(c0, Q(-1))
    r = PS(Dict{Q,Sum}(k - v => mulS(cc, ic0) for (k, cc) in a.c if k != v), a.prec - v)
    v, c0, r
end

function powPS(a::PS, q::Q, P::Q)
    q == 1 && return a
    iszero(q) && return onePS()
    v, c0, r = split_lead(a)
    T = polyPS(r, k -> binomq(q, k), 0, P)
    shiftPS(scalePS(T, powS(c0, q)), v * q)
end

function const_and_rest(a::PS)
    a.prec <= 0 && throw(NeedMorePrecision())
    any(k -> k < 0, keys(a.c)) &&
        throw(GruntzError("argument of exp/sin/cos diverges (oscillation or essential singularity)"))
    c0 = get(a.c, Q(0), zeroS())
    c0, trunc_ps(Dict{Q,Sum}(k => v for (k, v) in a.c if k != 0), a.prec)
end

function expPS(a::PS, P::Q)
    c0, u = const_and_rest(a)
    scalePS(polyPS(u, k -> Q(1) // factorial(big(k)), 0, P), mkexp(c0))
end

function trigPS(a::PS, P::Q)
    c0, u = const_and_rest(a)
    su = polyPS(u, k -> iseven(k) ? Q(0) : Q((-1)^((k - 1) ÷ 2)) // factorial(big(k)), 0, P)
    cu = polyPS(u, k -> isodd(k) ? Q(0) : Q((-1)^(k ÷ 2)) // factorial(big(k)), 0, P)
    s0, k0 = mksin(c0), mkcos(c0)
    addPS(scalePS(cu, s0), scalePS(su, k0)), addPS(scalePS(cu, k0), scalePS(su, negS(s0)))
end

function logPS(a::PS, logw::Sum, P::Q)
    v, c0, r = split_lead(a)
    L = polyPS(r, k -> Q((-1)^(k + 1)) // k, 1, P)
    cst = addS(mklog(c0), scaleS(logw, v))
    addPS(L, PS(isempty(cst.t) ? Dict{Q,Sum}() : Dict{Q,Sum}(Q(0) => cst), HUGE))
end

function series_atom(a::Atom, w::Atom, logw::Sum, P::Q)
    a.kind === :leaf && return PS(Dict{Q,Sum}(Q(1) => oneS()), HUGE)
    a.kind === :base && return series(a.args[1]::Sum, w, logw, P)
    arg = series(a.args[1]::Sum, w, logw, P)
    h = a.head
    h === :exp ? expPS(arg, P) :
    h === :log ? logPS(arg, logw, P) :
    h === :sin ? trigPS(arg, P)[1] : trigPS(arg, P)[2]
end

function series(s::Sum, w::Atom, logw::Sum, P::Q)::PS
    acc = PS(Dict{Q,Sum}(), P)
    for (m, c) in s.t
        cst = constS(c)
        ps = nothing
        for (a, e) in m.fs
            if hasleaf(a, w.key)
                pe = powPS(series_atom(a, w, logw, P), e, P)
                ps = ps === nothing ? pe : mulPS(ps, pe)
            else
                cst = mulS(cst, powS(atomS(a), e))
            end
        end
        term = ps === nothing ?
               PS(isempty(cst.t) ? Dict{Q,Sum}() : Dict{Q,Sum}(Q(0) => cst), HUGE) :
               scalePS(ps, cst)
        acc = addPS(acc, term)
    end
    acc
end

function leadterm(f::Sum, w::Atom, logw::Sum)
    P = Q(4)
    for _ in 1:8
        try
            ps = series(f, w, logw, P)
            if !isempty(ps.c)
                k = minimum(keys(ps.c))
                return ps.c[k], k
            end
        catch err
            err isa NeedMorePrecision || rethrow()
        end
        P *= 2
    end
    throw(GruntzError("could not determine the leading term (deep cancellation, " *
                      "or a zero that the internal simplifier cannot recognise)"))
end

# ---------------------------------------------------------------------------
# Gruntz's algorithm
# ---------------------------------------------------------------------------
function limitinf(e::Sum, x::Atom)::Union{Sum,Infinity}
    hasleaf(e, x.key) || return e
    c0, e0 = mrv_leadterm(e, x)
    if e0 > 0
        zeroS()
    elseif e0 < 0
        s = sgn(c0, x)
        s == 0 && throw(GruntzError("could not determine the sign of the leading coefficient"))
        Infinity(s)
    else
        limitinf(c0, x)
    end
end

# sign of e as x -> +oo (nonzero for nonzero e)
function sgn(c::Sum, x::Atom)::Int
    hasleaf(c, x.key) || return csign(c)
    if length(c.t) == 1
        m, co = first(c.t)
        sg = co < 0 ? -1 : 1
        simple = true
        for (a, e) in m.fs
            if hasleaf(a, x.key)
                if !(a == x || (a.kind === :fun && a.head === :exp))
                    simple = false
                    break
                end
            else
                s2 = csign(atomS(a))
                s2 == 0 && throw(GruntzError("zero constant factor"))
                if s2 < 0
                    isone(denominator(e)) || throw(GruntzError("complex power"))
                    isodd(numerator(e)) && (sg = -sg)
                end
            end
        end
        simple && return sg
    end
    L = limitinf(c, x)
    L isa Infinity && return L.sign
    v = csign(L)
    v != 0 && return v
    c1, _ = mrv_leadterm(c, x)
    sgn(c1, x)
end

logof(a::Atom) = (a.kind === :fun && a.head === :exp) ? a.args[1]::Sum : mklog(atomS(a))

function compare(a::Atom, b::Atom, x::Atom)
    c = limitinf(mulS(logof(a), powS(logof(b), Q(-1))), x)
    c isa Infinity ? :gt : (iszero(c) ? :lt : :eq)
end

function mrv_max(f::Vector{Atom}, g::Vector{Atom}, x::Atom)
    isempty(f) && return g
    isempty(g) && return f
    any(a -> a in g, f) && return union(f, g)
    c = compare(f[1], g[1], x)
    c === :gt ? f : c === :lt ? g : union(f, g)
end

function mrv(e::Sum, x::Atom)::Vector{Atom}
    Ω = Atom[]
    hasleaf(e, x.key) || return Ω
    for m in keys(e.t), (a, _) in m.fs
        Ω = mrv_max(Ω, mrv_atom(a, x), x)
    end
    Ω
end

function mrv_atom(a::Atom, x::Atom)::Vector{Atom}
    hasleaf(a, x.key) || return Atom[]
    a.kind === :leaf && return Atom[a]
    a.kind === :base && return mrv(a.args[1]::Sum, x)
    if a.head === :exp
        u = a.args[1]::Sum
        L = limitinf(u, x)
        return L isa Infinity ? mrv_max(Atom[a], mrv(u, x), x) : mrv(u, x)
    end
    Ω = Atom[]
    for u in a.args
        Ω = mrv_max(Ω, mrv(u::Sum, x), x)
    end
    Ω
end

function single_atom(s::Sum)
    length(s.t) == 1 || return nothing
    m, c = first(s.t)
    (isone(c) && length(m.fs) == 1 && isone(m.fs[1].second)) ? m.fs[1].first : nothing
end

# returns (c0, e0) with e = c0 * w^e0 + ..., w -> 0+ the exponential of the mrv class
function mrv_leadterm(e::Sum, x::Atom)
    Ω = mrv(e, x)
    isempty(Ω) && return e, Q(0)
    if any(==(x), Ω)
        # move up: x -> exp(x). The mrv set is mapped, never recomputed (avoids infinite recursion).
        xup = mkexp(atomS(x))
        up = a -> a == x ? xup : nothing
        newΩ = Atom[]
        ok = true
        for a in Ω
            at = single_atom(subs_atoms(atomS(a), up))
            (at === nothing || at.kind !== :fun || at.head !== :exp) ? (ok = false) : push!(newΩ, at)
        end
        e = subs_atoms(e, up)
        Ω = ok ? newΩ : mrv(e, x)
        any(==(x), Ω) && throw(GruntzError("internal error while moving up"))
    end
    sort!(Ω; by = a -> length(a.key))
    g1 = Ω[1].args[1]::Sum
    σ = sgn(g1, x)
    wS = atomS(WLEAF)
    repl = Dict{String,Sum}()
    for a in Ω
        gi = a.args[1]::Sum
        ci = limitinf(mulS(gi, powS(g1, Q(-1))), x)
        ci isa Infinity && throw(GruntzError("internal error: mrv class is inconsistent"))
        cq = ratconst(ci)
        (cq === nothing || iszero(cq)) &&
            throw(GruntzError("irrational ratio between exponents of comparable terms is unsupported"))
        repl[a.key] = mulS(powS(wS, Q(-σ) * cq), mkexp(subS(gi, scaleS(g1, cq))))
    end
    f = subs_atoms(e, a -> get(repl, a.key, nothing))
    leadterm(f, WLEAF, scaleS(g1, Q(-σ)))   # log(w) = -σ g1
end

# ---------------------------------------------------------------------------
# TermInterface <-> canonical form
# ---------------------------------------------------------------------------
function from_number(t)
    t isa Integer && return constS(Q(t))
    t isa Rational && return constS(Q(t))
    if t isa AbstractIrrational
        return t === ℯ ? mkexp(oneS()) : atomS(leafatom(t))
    end
    if t isa AbstractFloat
        isfinite(t) || throw(GruntzError("non-finite number in expression"))
        return constS(rationalize(BigInt, t))
    end
    t isa Complex && throw(GruntzError("complex numbers are not supported"))
    atomS(leafatom(t))          # symbolic wrapper types (e.g. Num) that are leaves
end

function from_term(t)::Sum
    if iscall(t)
        return apply_op(operation(t), Sum[from_term(a) for a in arguments(t)])
    elseif isexpr(t)
        throw(GruntzError("unsupported (non-call) expression `$t`"))
    elseif t isa Union{Integer,Rational,AbstractFloat,AbstractIrrational,Complex}
        return from_number(t)
    else
        return atomS(leafatom(t))
    end
end

function apply_op(op, a::Vector{Sum})
    n = op isa Symbol ? op : (op isa Function ? nameof(op) : throw(GruntzError("unsupported operation $op")))
    na = length(a)
    half = Q(1) // 2
    if n === :+
        foldl(addS, a; init = zeroS())
    elseif n === :-
        na == 1 ? negS(a[1]) : subS(a[1], foldl(addS, a[2:end]; init = zeroS()))
    elseif n === :*
        foldl(mulS, a; init = oneS())
    elseif n === :/ && na == 2
        mulS(a[1], powS(a[2], Q(-1)))
    elseif n === :inv && na == 1
        powS(a[1], Q(-1))
    elseif n === :^ && na == 2
        mkpow(a[1], a[2])
    elseif n === :sqrt && na == 1
        powS(a[1], half)
    elseif n === :cbrt && na == 1
        powS(a[1], Q(1) // 3)
    elseif n === :exp && na == 1
        mkexp(a[1])
    elseif n === :log && na == 1
        mklog(a[1])
    elseif n === :log && na == 2
        mulS(mklog(a[2]), powS(mklog(a[1]), Q(-1)))
    elseif n === :sin && na == 1
        mksin(a[1])
    elseif n === :cos && na == 1
        mkcos(a[1])
    elseif n === :tan && na == 1
        mulS(mksin(a[1]), powS(mkcos(a[1]), Q(-1)))
    elseif n in (:sinh, :cosh, :tanh) && na == 1
        p, m = mkexp(a[1]), mkexp(negS(a[1]))
        s = scaleS(subS(p, m), half)
        c = scaleS(addS(p, m), half)
        n === :sinh ? s : n === :cosh ? c : mulS(s, powS(c, Q(-1)))
    else
        throw(GruntzError("unsupported function/operator `$n` with $na argument(s)"))
    end
end

const OPS = Dict{Symbol,Any}(:+ => +, :* => *, :^ => ^, :exp => exp, :log => log, :sin => sin, :cos => cos)
mk(T, name::Symbol, args) =
    maketerm(T, :call, Any[T === Expr ? name : OPS[name]; args...], nothing)

fitint(n::BigInt) = typemin(Int) <= n <= typemax(Int) ? Int(n) : n
function num(c::Q)
    n, d = numerator(c), denominator(c)
    isone(d) && return fitint(n)
    (typemin(Int) <= n <= typemax(Int) && d <= typemax(Int)) ? Int(n) // Int(d) : c
end

atom_term(T, a::Atom) =
    a.kind === :leaf ? a.head :
    a.kind === :base ? to_term(T, a.args[1]::Sum) :
    mk(T, a.head, Any[to_term(T, a.args[1]::Sum)])

function mono_term(T, m::Mono, c::Q)
    fs = Any[]
    for (a, e) in m.fs
        t = atom_term(T, a)
        push!(fs, isone(e) ? t : mk(T, :^, Any[t, num(e)]))
    end
    isempty(fs) && return num(c)
    isone(c) && return length(fs) == 1 ? fs[1] : mk(T, :*, fs)
    mk(T, :*, Any[num(c); fs])
end

function to_term(T, s::Sum)
    isempty(s.t) && return 0
    ts = Any[mono_term(T, m, c) for (m, c) in sort!(collect(s.t); by = p -> p.first.key)]
    length(ts) == 1 ? ts[1] : mk(T, :+, ts)
end

# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------
function lim_equal(a, b)
    (a isa Infinity && b isa Infinity) && return a.sign == b.sign
    (a isa Sum && b isa Sum) && return skey(a) == skey(b)
    false
end

function _limit(e::Sum, xa::Atom, x0, dir::Symbol)
    xS = atomS(xa)
    if x0 isa AbstractFloat && isinf(x0)
        return x0 > 0 ? limitinf(e, xa) :
               limitinf(subs_atoms(e, a -> a == xa ? negS(xS) : nothing), xa)
    end
    p = from_term(x0)
    hasleaf(p, xa.key) && throw(GruntzError("the limit point must not contain the variable"))
    inv_x = powS(xS, Q(-1))
    side(sg) = limitinf(subs_atoms(e, a -> a == xa ? addS(p, scaleS(inv_x, Q(sg))) : nothing), xa)
    if dir === :+
        side(1)
    elseif dir === :-
        side(-1)
    elseif dir === :both
        l, r = side(1), side(-1)
        lim_equal(l, r) || throw(GruntzError("one-sided limits differ"))
        l
    else
        throw(GruntzError("dir must be :+, :- or :both"))
    end
end

function gruntz_limit(expr, x, x0 = Inf; dir::Symbol = :+)
    T = iscall(expr) ? typeof(expr) : Expr
    L = _limit(from_term(expr), leafatom(x), x0, dir)
    L isa Infinity && return L.sign > 0 ? Inf : -Inf
    r = ratconst(L)
    r !== nothing && return num(r)
    to_term(T, L)
end

end # module
