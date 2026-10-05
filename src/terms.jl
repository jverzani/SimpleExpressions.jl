## ---- term storage for + and *
##
## Sums and products are stored as
##
##   SymbolicSum:  c + k₁⋅t₁ + k₂⋅t₂ + ⋯     (numbers c, kᵢ; symbolic tᵢ)
##   SymbolicProd: c ⋅ b₁^e₁ ⋅ b₂^e₂ ⋯       (numbers c, eᵢ; symbolic bᵢ)
##
## so like terms are combined and argument order does not matter when
## comparing. Everything else is a `SymbolicExpression` tree.
##
## The `StaticExpression` for a term is only built when needed (evaluation,
## substitution, `↓`, ...) and then cached, so the cost of compiling for the
## expression's shape is paid only for expressions that are used that way.

## This was written using co-pilot
## the simplifications from this storage come at some expense:
#=

> Compile time is about 3.7× lower with the hybrid and equality is
  much faster, but building expressions with +/ is slower than the old
  tree.

> Building: the hybrid is 5–15× slower for sums and products with few
  like terms, and faster only when many terms combine. Cause: each +/
  now builds a term accumulator instead of one tree node. The
  standalone prototype is 3–30× faster than the hybrid at building.

> Evaluation: compiled evaluation is 2–3× slower, because a sum or
  product is turned back into a tree the first time it is
  evaluated. First evaluation is also a bit slower (4.6 s versus 3.3
  s).

> Allocations: building poly n=20 allocates 77k versus 117k for the
  hybrid's earlier numbers. I did not capture an old-tree allocation
  figure this time.

> Canonical equality: y+x == x+y is true in the hybrid and false in
  the old tree.

> The hybrid pays off if you value canonical equality and compile
  time, and you mostly build and compare expressions rather than
  evaluate them in hot loops. The standalone prototype is faster at
  building but evaluates far more slowly.

=#

abstract type SymbolicTerms <: AbstractSymbolic end

struct SymbolicSum <: SymbolicTerms
    c::Number
    keys::Vector{AbstractSymbolic}
    coefs::Vector{Any}   # numbers, or expressions with no variables (parameters)
    cache::Base.RefValue{Any}
end

struct SymbolicProd <: SymbolicTerms
    c::Number
    keys::Vector{AbstractSymbolic}
    coefs::Vector{Number}
    cache::Base.RefValue{Any}
end

# methods written for `SymbolicExpression` that only use `operation`, `arguments`, `↓`
const SymbolicCall = Union{SymbolicExpression, SymbolicTerms}

## ---- numbers
# Only fold "plain" numbers; π, ℯ, ... stay symbolic as before
const PlainNumber = Union{Integer, Rational, AbstractFloat, Complex}
_num(x::Number) = x isa PlainNumber ? x : nothing
function _num(x::SymbolicNumber)
    v = x()
    v isa PlainNumber ? v : nothing
end
_num(x) = nothing

## ---- structural hash / equality for keys
## written with @nospecialize so that they are compiled once, not once per
## expression type (the `StaticExpression` type encodes the whole tree)
function _thash(@nospecialize(a), h::UInt=UInt(0))
    if a isa StaticExpression
        h = _thash(a.operation, hash(:E, h))
        for c in a.children
            h = _thash(c, h)
        end
        h
    elseif a isa DynamicConstant
        hash(a.value, h)
    elseif a isa StaticVariable
        hash(typeof(a), h)
    elseif a isa DynamicVariable
        hash(a.sym, h)
    else
        hash(a, h)
    end
end

function _teq(@nospecialize(a), @nospecialize(b))
    a === b && return true
    if a isa StaticExpression
        b isa StaticExpression || return false
        _teq(a.operation, b.operation) || return false
        ca, cb = a.children, b.children
        length(ca) == length(cb) || return false
        for i in 1:length(ca)
            _teq(ca[i], cb[i]) || return false
        end
        true
    elseif a isa DynamicConstant
        b isa DynamicConstant && isequal(a.value, b.value)
    elseif a isa StaticVariable
        typeof(a) === typeof(b)
    elseif a isa DynamicVariable
        b isa DynamicVariable && a.sym == b.sym
    else
        a == b
    end
end

_khash(@nospecialize(t)) = t isa SymbolicTerms ? hash(t) : _thash(t.u)
_keyeq(@nospecialize(a), @nospecialize(b)) =
    (a isa SymbolicTerms || b isa SymbolicTerms) ? a == b : _teq(a.u, b.u)

## ---- accumulator
mutable struct Acc
    c::Number
    keys::Vector{AbstractSymbolic}
    coefs::Vector{Any}
    hs::Vector{UInt}
end
Acc(c) = Acc(c, AbstractSymbolic[], Any[], UInt[])

## ---- coefficients: plain numbers or parameter-only expressions
_sym(k::Number) = SymbolicNumber(k)
_sym(@nospecialize(k::AbstractSymbolic)) = k
_cnorm(k::Number) = _norm(k)
function _cnorm(@nospecialize(k::AbstractSymbolic))
    v = _num(k)
    v === nothing ? k : _norm(v)
end
_czero(k) = k isa Number && iszero(k)
_cone(k) = k isa Number && isone(k)
_cadd(a::Number, b::Number) = a + b
_cadd(@nospecialize(a), @nospecialize(b)) = _cnorm(_add(_sym(a), _sym(b)))
_cmul(a::Number, b::Number) = a * b
_cmul(@nospecialize(a), @nospecialize(b)) = _cnorm(_mul(_sym(a), _sym(b)))
_ceq(a::Number, b::Number) = a == b
_ceq(@nospecialize(a::AbstractSymbolic), @nospecialize(b::AbstractSymbolic)) = _keyeq(a, b)
_ceq(@nospecialize(a), @nospecialize(b)) = false
_chash(k::Number) = hash(k)
_chash(@nospecialize(k::AbstractSymbolic)) = _khash(k)

# does the expression involve a `SymbolicVariable` (not just parameters)?
function _hasvar(@nospecialize(x))
    x isa SymbolicTerms && return any(_hasvar, x.keys)
    x isa AbstractSymbolic && return _hasvar_u(x.u)
    false
end
function _hasvar_u(@nospecialize(u))
    u isa StaticVariable && return true
    u isa StaticExpression && return any(_hasvar_u, u.children)
    false
end

function put!(a::Acc, @nospecialize(t::AbstractSymbolic), @nospecialize(k))
    h = _khash(t)
    for i in eachindex(a.hs)
        if a.hs[i] == h && _keyeq(a.keys[i], t)
            a.coefs[i] = _cadd(a.coefs[i], k)
            return a
        end
    end
    push!(a.keys, t)
    push!(a.coefs, k)
    push!(a.hs, h)
    a
end

function _drop_zeros!(a::Acc)
    all(!_czero, a.coefs) && return a
    keep = findall(!_czero, a.coefs)
    a.keys = a.keys[keep]
    a.coefs = a.coefs[keep]
    a
end

## ---- constructors (canonical forms)
_powexpr(b, e) = SymbolicExpression(StaticExpression((↓(b), DynamicConstant(e)), ^))

_norm(c::Rational) = isone(denominator(c)) ? numerator(c) : c
_norm(c) = c

function _mkprod(c::Number, keys, coefs)
    c = _norm(c)
    coefs = Number[_norm(e) for e in coefs]
    iszero(c) && return SymbolicNumber(0)
    if isempty(keys)
        return SymbolicNumber(c)
    end
    if length(keys) == 1 && isone(c)
        isone(coefs[1]) && return keys[1]
        coefs[1] > 0 && return _powexpr(keys[1], coefs[1])
    end
    # a number times a sum distributes over the sum
    if length(keys) == 1 && isone(coefs[1]) && keys[1] isa SymbolicSum
        s = keys[1]
        return SymbolicSum(_norm(c * s.c), s.keys, Any[_cnorm(_cmul(c, k)) for k in s.coefs], Ref{Any}(nothing))
    end
    SymbolicProd(c, keys, coefs, Ref{Any}(nothing))
end

function _mksum(c::Number, keys, coefs)
    c = _norm(c)
    coefs = Any[_cnorm(k) for k in coefs]
    isempty(keys) && return SymbolicNumber(c)
    if length(keys) == 1 && iszero(c)
        _cone(coefs[1]) && return keys[1]
        return _mul(_sym(coefs[1]), keys[1])
    end
    SymbolicSum(c, keys, coefs, Ref{Any}(nothing))
end

## ---- additive view: add `x` into accumulator
_addparts!(a::Acc, x::Number) = (a.c += x; a)
function _addparts!(a::Acc, @nospecialize(x::AbstractSymbolic))
    v = _num(x)
    v !== nothing && return _addparts!(a, v)
    _addparts_term!(a, x)
end

_addparts_term!(a::Acc, @nospecialize(x::AbstractSymbolic)) = put!(a, x, 1)
function _addparts_term!(a::Acc, x::SymbolicSum)
    a.c += x.c
    for (t, k) in zip(x.keys, x.coefs)
        put!(a, t, k)
    end
    a
end
# parameters are coefficients: `p*x` is the term `x` with coefficient `p`
function _addparts_term!(a::Acc, x::SymbolicProd)
    vs = findall(_hasvar, x.keys)
    if isempty(vs) || length(vs) == length(x.keys)
        t = isone(x.c) ? x : _mkprod(1, x.keys, x.coefs)
        return put!(a, t, x.c)
    end
    ps = setdiff(eachindex(x.keys), vs)
    coef = _mkprod(x.c, x.keys[ps], x.coefs[ps])
    put!(a, _mkprod(1, x.keys[vs], x.coefs[vs]), _cnorm(coef))
end
function _addparts_term!(a::Acc, @nospecialize(x::SymbolicExpression))
    op = operation(x)
    if op === +
        for y in arguments(x)
            _addparts!(a, y)
        end
        return a
    elseif op === *
        return _addparts!(a, prod(arguments(x)))
    elseif op === (/) && !iszero(arguments(x)[2])
        u, v = arguments(x)
        return _addparts!(a, _mul(u, _pownum(v, -1)))
    end
    put!(a, x, 1)
end

function _add(@nospecialize(x::AbstractSymbolic), @nospecialize(y::AbstractSymbolic))
    a = Acc(0)
    _addparts!(a, x)
    _addparts!(a, y)
    _drop_zeros!(a)
    _mksum(a.c, a.keys, a.coefs)
end

## ---- multiplicative view
_mulparts!(a::Acc, x::Number) = (a.c *= x; a)
function _mulparts!(a::Acc, @nospecialize(x::AbstractSymbolic))
    v = _num(x)
    v !== nothing && return _mulparts!(a, v)
    _mulparts_term!(a, x)
end

_mulparts_term!(a::Acc, @nospecialize(x::AbstractSymbolic)) = put!(a, x, 1)
function _mulparts_term!(a::Acc, x::SymbolicProd)
    a.c *= x.c
    for (b, e) in zip(x.keys, x.coefs)
        put!(a, b, e)
    end
    a
end
function _mulparts_term!(a::Acc, @nospecialize(x::SymbolicExpression))
    op = operation(x)
    if op === *
        for y in arguments(x)
            _mulparts!(a, y)
        end
        return a
    elseif op === ^
        b, e = arguments(x)
        n = _num(e)
        n !== nothing && return put!(a, _canon(b), n)
    elseif op === (/)
        u, v = arguments(x)
        iszero(v) && return put!(a, x, 1)
        _mulparts!(a, u)
        return _mulparts!(a, _pownum(v, -1))
    elseif op === inv
        return _mulparts!(a, _pownum(only(arguments(x)), -1))
    end
    put!(a, x, 1)
end

# rebuild raw `+`/`*` trees as terms so bases compare equal
_canon(@nospecialize(b::AbstractSymbolic)) = b
function _canon(@nospecialize(b::SymbolicExpression))
    op = operation(b)
    op === (+) && return _add(b, SymbolicNumber(0))
    op === (*) && return _mul(b, SymbolicNumber(1))
    b
end

# x^n for a plain number n
function _pownum(@nospecialize(x::AbstractSymbolic), n::Number)
    iszero(n) && return SymbolicNumber(1)
    v = _num(x)
    if v !== nothing
        if n isa Integer && !(iszero(v) && n < 0)
            v isa Integer && (v = Rational(v))
            return SymbolicNumber(_norm(v^n))
        end
        return _powtree(x, SymbolicNumber(n))
    end
    a = Acc(1)
    _mulparts!(a, x)
    if n isa Integer
        c = a.c isa Integer ? Rational(a.c)^n : a.c^n
        for i in eachindex(a.coefs)
            a.coefs[i] *= n
        end
        a.c = c
    elseif !(isone(a.c) && length(a.keys) == 1 && isone(a.coefs[1]))
        return _powtree(x, SymbolicNumber(n))
    else
        a.coefs[1] = n
    end
    _mkprod(a.c, a.keys, a.coefs)
end

function _mul(@nospecialize(x::AbstractSymbolic), @nospecialize(y::AbstractSymbolic))
    a = Acc(1)
    _mulparts!(a, x)
    _mulparts!(a, y)
    _drop_zeros!(a)
    _mkprod(a.c, a.keys, a.coefs)
end

## ---- materialize as a tree (cached)
materialize(x::SymbolicExpression) = x
function materialize(x::SymbolicTerms)
    u = x.cache[]
    u === nothing || return u
    u = SymbolicExpression(StaticExpression(Tuple(_children(x)), operation(x)))
    x.cache[] = u
end

function _children(x::SymbolicSum)
    cs = Any[]
    iszero(x.c) || push!(cs, DynamicConstant(x.c))
    for (t, k) in zip(x.keys, x.coefs)
        push!(cs, _cone(k) ? ↓(t) : ↓(_mul(_sym(k), t)))
    end
    cs
end

function _factors(c, ks, es)
    cs = Any[]
    isone(c) || push!(cs, DynamicConstant(c))
    for (b, e) in zip(ks, es)
        push!(cs, isone(e) ? ↓(b) : ↓(_powexpr(b, e)))
    end
    cs
end
_factortree(cs) = isempty(cs) ? DynamicConstant(1) :
                  length(cs) == 1 ? only(cs) : StaticExpression(Tuple(cs), *)

# negative powers and rational coefficients form the denominator
function _split(x::SymbolicProd)
    pos = findall(>(0), x.coefs)
    neg = findall(<(0), x.coefs)
    c = x.c
    num, den = c isa Rational ? (numerator(c), denominator(c)) : (c, 1)
    (num, x.keys[pos], x.coefs[pos]), (den, x.keys[neg], -x.coefs[neg])
end
_hasden(x::SymbolicProd) = any(<(0), x.coefs) || (x.c isa Rational && !isone(denominator(x.c)))

function _children(x::SymbolicProd)
    _hasden(x) || return _factors(x.c, x.keys, x.coefs)
    n, d = _split(x)
    Any[_factortree(_factors(n...)), _factortree(_factors(d...))]
end

↓(x::SymbolicTerms) = ↓(materialize(x))

## ---- term interface, without materializing
TermInterface.operation(::SymbolicSum) = +
TermInterface.operation(x::SymbolicProd) = _hasden(x) ? (/) : (*)
TermInterface.isexpr(::SymbolicTerms) = true
TermInterface.iscall(::SymbolicTerms) = true
TermInterface.head(x::SymbolicTerms) = operation(x)
TermInterface.arguments(x::SymbolicTerms) = arguments(materialize(x))
TermInterface.children(x::SymbolicTerms) = arguments(x)

## ---- equality, hashing (independent of term order)
function _same_terms(k1, v1, k2, v2)
    length(k1) == length(k2) || return false
    for (k, v) in zip(k1, v1)
        found = false
        for (k′, v′) in zip(k2, v2)
            if _ceq(v, v′) && _keyeq(k, k′)
                found = true
                break
            end
        end
        found || return false
    end
    true
end

Base.:(==)(x::SymbolicSum, y::SymbolicSum) =
    x.c == y.c && _same_terms(x.keys, x.coefs, y.keys, y.coefs)
Base.:(==)(x::SymbolicProd, y::SymbolicProd) =
    x.c == y.c && _same_terms(x.keys, x.coefs, y.keys, y.coefs)
Base.:(==)(x::SymbolicSum, y::SymbolicProd) = false
Base.:(==)(x::SymbolicProd, y::SymbolicSum) = false
Base.:(==)(x::SymbolicTerms, y::AbstractSymbolic) = _teq(↓(x), ↓(y))
Base.:(==)(x::AbstractSymbolic, y::SymbolicTerms) = _teq(↓(x), ↓(y))

function _hash_terms(tag, x)
    h = hash(tag, hash(x.c))
    for (k, v) in zip(x.keys, x.coefs)
        h ⊻= hash(_chash(v), _khash(k))
    end
    h
end
Base.hash(x::SymbolicSum) = _hash_terms(:sum, x)
Base.hash(x::SymbolicProd) = _hash_terms(:prod, x)
Base.hash(x::SymbolicTerms, h::UInt) = hash(hash(x), h)

## ---- forward remaining methods
Base.isless(x::SymbolicTerms, y::AbstractSymbolic) = isless(materialize(x), y)
Base.isless(x::AbstractSymbolic, y::SymbolicTerms) = isless(x, materialize(y))
Base.isless(x::SymbolicTerms, y::SymbolicTerms) = isless(materialize(x), materialize(y))
Base.isless(x::SymbolicTerms, ::Number) = false
Base.isless(::Number, x::SymbolicTerms) = true

## `combine` works on the tree
_combine(op::Any, ex::SymbolicTerms, f) = _combine(op, materialize(ex), f)
