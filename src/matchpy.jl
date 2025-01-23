# implement algorithm of matchpy paper through Ch. 3
# Non-linear Associative-Commutative Many-to-One Pattern Matching with Sequence Variables by Manuel Krebber

# 𝑋 variables: regular, [star, plus]
# 𝐹 function heads

# split symbolic objects into
# 𝐹₀ 0-arity expressions
# 𝐿 all symbolic variables
# 𝑋 wildcard expressions which split into
# Xʳᵉᵍᵘˡᵃʳ regular        -- `_is_Wild`
# 𝑋Xᵖˡᵘˢ   plus variables -- `_is_Plus`
# Xˢᵗᵃʳ    star variables -- `_is_Star`
_is_𝐹₀(::Any) = false  # 𝐹ₙ is arity of function; this is no function
_is_𝐿(x::Any) =  false #
_is_Wild(x::Any) = false
_is_Plus(x::Any) = false # atleast one
_is_Star(x::Any) = false
_is_𝑋(x) = _is_Wild(x) || _is_Plus(x) || _is_Star(x) #

# predicates
isassociative(::Any) = false
iscommutative(::Any) = false

isassociative(::typeof(+)) = true
isassociative(::typeof(*)) = true

iscommutative(::typeof(+)) = true
iscommutative(::typeof(*)) = true

# ExpressionType = SymbolicExpression

## ---------------------------------
## only TermInterface below this line

## matchpy

# Δ could use Dict for this

# σ△σ′
function iscompatible(σ, σ′)
    isnothing(σ) && return true
    isnothing(σ′) && return false
    for (s,p) ∈ σ
        for (s′, p′) ∈ σ′
            s == s′ && p != p′ && return false
        end
    end
    true
end

# σ⊔σ′
function union_match(σ, σ′)
    isnothing(σ) && return σ′
    for (s′, p′) ∈ σ′
        any(s′ == s for (s,p) ∈ σ) && continue
        if _is_𝑋(s′)
            σ = TupleTools.vcat(σ, (s′ => p′,))
        end
    end
    σ
end

# \sqcup σ -> nothing or (join, )
function ⊔(σ, σ′)
    iscompatible(σ, σ′) || return nothing
    (union_match(σ, σ′),)
end

# {σ⊔σ′ |σ∈Θ∧σ△σ′}
function union_matches(Θ, σ′)
    isnothing(Θ) && return (σ′, )
    in(σ′, Θ) && return Θ
    out = tuple((union_match(σ, σ′) for σ ∈ Θ
                 if iscompatible(σ, σ′))...)
    out
end


## return iterator -- doesn't seem more performant
function _union_matches(Θ, σ′)
    isnothing(Θ) && return Iterators.rest((σ′,), 1)
    Iterators.map(Iterators.filter(Θ) do σ
                  iscompatible(σ, σ′)
                  end ) do σ
                      union_match(σ, σ′)
                  end
end

# Θ ∪ Θ′
function union_match_sets(Θ, Θ′)
    Θ == ∅ && return Θ′
    Θ′ == ∅ && return Θ
    Θ′′ = filter(!in(Θ), tuple(Θ′...))
    TupleTools.vcat(Θ, Θ′′)
end

# return substitution tuple (p1 => s1, p2 => s2, ...) possibly empty ()
# or return nothing if no match
function SyntacticMatch(s, p, σ=nothing)
    _is_𝑋(p) && return (p => s,)
    _is_𝐿(p) && return s == p ? () : nothing
    s == p && return ()
    _is_𝐹₀(p) && return nothing

    opₛ, opₚ = operation(s), operation(p)
    opₛ != opₚ && return nothing

    argsₛ, argsₚ = arguments(s), arguments(p)
    length(argsₛ) == length(argsₚ) || return nothing

    for (si,pi) ∈ zip(argsₛ, argsₚ)
        σ′ = SyntacticMatch(si, pi, σ)
        (isnothing(σ′) || !iscompatible(σ, σ′)) && return nothing
        σ = union_match(σ, σ′)
    end

    return σ
end


# σ is nothing or a substitution tuple, possibly ()
# Θ is empty, (), or
∅ =  () # is not ((),)

# fₐ is +,*, or nothing

function MatchOneToOne(ss::Tuple, p, fₐ=nothing, Θ=((),))
    n = length(ss)
    if _is_𝐿(p) && !_is_𝑋(p) # 𝐹₀ -- not a SymbolicExpression
        n == 1 && p == only(ss) && return Θ
    elseif _is_Wild(p) && isnothing(fₐ)
        σ′ = (p => first(ss),)
        n == 1 && return union_matches(Θ, σ′)
    elseif _is_𝑋(p)
        if _is_𝑋(p) && !isnothing(fₐ)
            σ′ = (p => maketerm(ExpressionType, fₐ, ss, nothing),)
        else
            σ′ = (p => ss,)
        end
        if _is_Star(p) || n ≥ 1
            return union_matches(Θ, σ′)
        end
    elseif n == 1
        s = only(ss)
        hₚ, hₛ = operation(p), operation(s)
        if hₚ == hₛ
            ss = arguments(s)
            ps = arguments(p)
            fₐ′ = isassociative(hₚ) ? hₚ : nothing
            if iscommutative(fₐ′)
                return MatchCommutativeSequence(ss, ps, fₐ′, Θ)
            else
                return MatchSequence(ss, ps, fₐ′, Θ)
            end
        end
    end
    return ∅
end

function MatchSequence(ss, ps, fₐ=nothing, Θ=((),))
    n,m = length(ss), length(ps)
    nstar = sum(_is_Star(p) for p in ps)
    m - nstar > n && return ∅
    nplus = sum(_is_Plus(p) for p in ps)
    if isassociative(fₐ)
        nplus = nplus + sum(_is_Wild(p) for p in ps)
    end
    nfree = n - m + nstar
    nseq = nstar + nplus
    Θᵣ = ∅

    for ks ∈ Base.Iterators.product((0:nfree for _ in 1:nseq)...)
        (!isempty(ks) && sum(ks) != nfree) && continue
        i, j = 1, 1 # 0,0??
        Θ′ = Θ
        for (l,pl) ∈ enumerate(ps)
            lsub = 1
            if (_is_Plus(pl) || _is_Star(pl)) ||
                (_is_Wild(pl) && !isnothing(fₐ))
                kj = isempty(ks) ? 1 : ks[j]
                lsub = lsub + kj
                if _is_Star(pl)
                    lsub = lsub - 1
                end
                j = j + 1
            end
            ss′ = ss[i:(i+lsub-1)] # note -1 here
            Θ′ = MatchOneToOne(ss′, pl, fₐ, Θ′)
            Θ′ == ∅  && break
            i = i + lsub
        end
        Θᵣ = union_match_sets(Θᵣ, Θ′)
    end
    return Θᵣ
end

function MatchCommutativeSequence(ss, ps, fₐ = nothing, Θ = ((),))

    out = _match_constant_patterns(ss, ps)
    isnothing(out) && return ∅
    ss, ps = out
    
    function f1(a)
        ss, ps, σ = a
        _match_non_variable_patterns(ss, ps, fₐ, σ)
    end
    function f2(a)
        ss, ps, σ = a
        _match_regular_variables(ss, ps, fₐ, σ)
    end
    
    function f3(a)
        ss, ps, σ = a
        _match_sequence_variables(ss, ps, fₐ, σ)
    end


    itr = let ss=ss,ps=ps,Θ=Θ
        ((ss, ps, σ) for σ ∈ Θ)
    end

    
    t1 =  Iterators.map(f1, itr) |>
        Iterators.flatten |>
        Base.Fix1(Iterators.filter, !isnothing)

    t2 = Iterators.map(f2, t1) |> Iterators.flatten |>
        Base.Fix1(Iterators.filter, !isnothing)

    t3 = Iterators.map(f3, t2) |> Iterators.flatten |>
        Base.Fix1(Iterators.filter, !isnothing)

    return t3

    #=
    function f3a(a)
        ss, ps, σ = a
        Θ = _match_regular_variables(ss, ps, fₐ, σ)
        Iterators.map(f2, Θ) |>
            Iterators.flatten
    end

    t2 = Iterators.map(f3, t1) |>
        Iterators.flatten
    =#
    
    return t2
    
end
function _check_matched_variables(σ, ss, ps)
    # check for each match in σ
    # there are as many subjects as needed for the match
    for (p,s) ∈ σ
        # how many times does s appear in pattern
        inds = findall(==(s), ss)
        n = length(inds)
        inds = findall(==(p), ps)
        length(inds) >= n || return false
    end
    return true
end

# return trimmed ss, ps or nothing
function _match_constant_patterns(ss, ps)
    pred(a) = any(any(_is_𝑋(u) for u in s) for s in free_symbols(a))
    Pconst = filter(!pred, ps)
    ss′ = ss
    for p ∈ Pconst
        p in ss′ || return nothing
        ss′ = filter(!=(p), ss′)
    end
    ps′ = filter(p -> p ∉ Pconst, ps)
    (ss′, ps′)
end

# trims down ss, ps
# returns (ss,ps) or nothing
function  _match_matched_variables(ss, ps, σ)
    # subtract from, ps, ss previously matched variables
    (isnothing(σ) || isempty(σ)) && return (ss, ps)
    for (p,s) ∈ σ
        for _ in 1:count(==(p), ps)
            # delete s from ss or return nothhing
            itr = isa(s, Tuple) ? s : (s,)
            for si ∈ itr
                i = findfirst(==(si), ss)
                isnothing(i) && return nothing
                ss = tuple((v for (j,v) ∈ enumerate(ss) if j != i)...)
            end
        end
    end
    ps = tuple((v for v in ps if v ∉ first.(σ))...)
    ss, ps
end

# match non_variable_patterns
# return iterator of (ss, ps, σ)
function _match_non_variable_patterns(ss, ps, fc=nothing, σ=())
    out = _match_matched_variables(ss, ps, σ)
    isnothing(out) && return nothing
    ss, ps = out
    
    ps′, ps′′ = tuplesplit(iscall, ps)
    length(ps′) == 0 && return ((ss, ps, σ),)
    ss′′, ss′ = tuplesplit(!iscall, ss)
    length(ps′) == length(ss′) || return nothing # ∅
    
    i = Combinatorics.permutations(1:length(ss′))
    ii = Iterators.map(i) do inds
        ss′′′ = ss′[inds]
        Θ′ = (σ,)
        for (s,p) ∈ zip(ss′′′, ps′)
            operation(s) == operation(p) || return nothing
            Θ′ = MatchSequence(arguments(s), arguments(p), fc, Θ′)
            Θ′ == ∅ && return nothing
        end
        Θ′ == ∅ && return nothing
        Θ′
    end
    iii = Iterators.flatten(Iterators.filter(!isnothing, ii))
    return Iterators.map(Θ -> (ss′′, ps′′, Θ), iii)
end

# match x_ type variables
# return iterator of (ss, ps, σ)
function _match_regular_variables(ss, ps, fc=nothing, σ = ())
    out =  _match_matched_variables(ss, ps, σ)
    isnothing(out) && return ()
    
    ss, ps = out
    # fₐ is  commutative, maybe associative
    isassociative(fc) && return ((ss, ps, σ),)

    ps_reg, ps′′ = tuplesplit(_is_Wild, ps)
    isempty(ps_reg) && return ((ss, ps, σ),)

    if length(ps_reg) < length(ss)
        if ps_reg == ps
            # can't match, not enough
            return ∅
        end
    end

    dp = _countmap(ps_reg)
    ds = _countmap(ss)

    i = _split_take(ds, dp)
    _isc(ab, σ) = iscompatible(first(ab), σ)
    ii = Iterators.filter(Base.Fix2(_isc, σ), i)
    iii = Iterators.map(ii) do (σ′, ds)
        σ′ = union_match(σ, σ′)
        ss′′ = _uncountmap(ds)
        (ss′′, ps′′, σ′)
    end

    return iii

end

# counting function
# different ways to grab the pie
function _split_take(ds, dp)
    n = length(ds)
    
    k = length(dp)
    i = Iterators.product((1:n for _ in 1:k)...)
    ii = Iterators.map(i) do inds
        ds′ = copy(ds)
        σ = ()
        for (i, (p, np)) ∈ zip(inds, (dp))
            s, ns = ds′[i]
            np > ns && (σ = (); break) # won't fit
            ds′[i] = s => (ns - np)
            σ = union_match(σ, ((p => s),))
        end
        σ == () && return nothing
        (σ, ds′)
    end
    iii = Iterators.filter(!isnothing, ii)
end


# return iterator of matches, σ
function _match_sequence_variables(ss, ps, fc=nothing, σ = ())
    out =  _match_matched_variables(ss, ps, σ)
    isnothing(out) && return ()
    ss, ps = out
    
    if !isassociative(fc)
        !isempty(filter(_is_Wild, ps)) && return ()
    end
    λ = x -> (_is_Wild(x) || _is_Plus(x))
    vs = tuplesplit(λ, ps)
    length(first(vs)) > length(ss) && return () # too many plus variables

    ds = _countmap(ss)
    dplus, dstar = _countmap(first(vs)), _countmap(last(vs))

    vars = TupleTools.vcat(tuple(first.(dplus)...), tuple(first.(dstar)...))
    svars = tuple(first.(ds)...)

    pluses = tuple((v for (k,v) in dplus)...) # unique
    stars = tuple((v for (k,v) in dstar)...)  # uniqe

    n1, n2 = length(pluses), length(stars)
    n = n1 + n2
    ks = TupleTools.vcat(pluses, stars)
    i = ntuple((a) -> 0, Val(n))

    Θ = ()
    h = isnothing(fc) ? identity :
        ((as) -> _maketerm(fc, as))

    # rename
    ssᵥ = tuple((v for (k,v) in ds)...) # times in ss
    ii = Iterators.filter(Iterators.product(
        (Iterators.product((0:s for _ in 1:n)...) for s in ssᵥ)...)) do u
            all(sum(ui .* ks) == si for (ui,si) in zip(u, ssᵥ)) &&
                all(sum(ui[i] for ui in u) > 0 for i in 1:n1)
        end
    
    iii = Iterators.map(ii) do u
        σ′ = σ
        for (j, v) ∈ enumerate(vars)
            vv = ()
            for (i,s) in enumerate(svars)
                vi = ntuple((_) -> s, Val(u[i][j]))
                vv = TupleTools.vcat(vv, vi)
            end
            if vv != ()
                σ′′ = (v => h(vv),)
                iscompatible(σ′, σ′′) || break
                σ′ = TupleTools.vcat(σ′, σ′′)
            end
        end
        iscompatible(σ, σ′) || return nothing
        σ′
    end

    iv = Iterators.filter(!isnothing, iii)

    iv
end

# need unit here
function _maketerm(fa, xs)
    isempty(xs) && return
    fa == (*) ? one(ExpressionType) :
        fa == (+) ? zero(ExpressionType) :
        ()
    maketerm(ExpressionType, fa, xs, nothing)
end

## -----

"""
    map_matched(ex, is_match, f)

Traverse expression. If `is_match` is true, apply `f` to that part of expression tree and reassemble.

Basically `CallableExpressions.expression_map_matched`.

Not exported.
"""
map_matched(ex, is_match, f) = map_matched(Val(iscall(ex)), ex, is_match, f)
map_matched(::Val{false}, x, is_match, f)  = is_match(x) ? f(x) : x
function map_matched(::Val{true}, x, is_match, f)
    # copy of  CallableExpressions.expression_map_matched(pred, mapping, u)
    # but in SimpleExpressions domain
    is_match(x) && return f(x)
    iscall(x) || return x
    children = map_matched.(arguments(x), is_match, f)
    maketerm(ExpressionType, operation(x), children, metadata(x))
end



## ----- Replace -----
## exact replacement
function _replace_exact(ex, p, q)
    map_matched(ex, ==(p), _ -> q)
end

# replace expression head u with v
function _replace_expression_head(ex, u, v)
    !iscall(ex) && return ex
    args′ = (_replace_expression_head(a, u, v) for a ∈ arguments(ex))
    op = operation(ex)
    λ = op == u ? v : op
    ex = maketerm(ExpressionType, λ, args′, nothing)
end

## Replacement of arguments
function _replace_arguments(ex, u, v)
    iscall(ex) || return (ex == u ? v : ex)

    σ = match(u, ex)
    if !isnothing(σ)
        σ == () && return v
        return v(σ...)
    end

    # peel off
    op, args = operation(ex), arguments(ex)
    args′ = _replace_arguments.(args, (u,), (v,))

    return maketerm(ExpressionType, op, args′, nothing)
end


## -----

## utils
function _countmap(x)
    d = IdDict()
    [(d[xi] = get(d, xi, 0) + 1) for xi in x]
    return [k => v for (k,v) ∈ d]
end
function _uncountmap(dx)
    TupleTools.vcat((tuple((k for _ in 1:v)...) for (k,v) in dx)...)
end

tuplesplit(pred, t) = (t = filter(pred,t), f=filter(!pred, t))

# take b out of a, error if b has elements not in a or too many
function tuplediff(as, bs)
    for b in bs
        i = findfirst(==(b), as)
        as = tuple((as[j] for j in eachindex(as) if j != i)...)
    end
    as
end
