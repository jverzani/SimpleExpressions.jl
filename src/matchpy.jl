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

# XXX still shaky
function MatchCommutativeSequence(ss, ps, fₐ=nothing, Θ=((),))
    debug = false
    debug && @show :matchcomm, ss, ps, fₐ, Θ

    # constant patterns
    out = _match_constant_patterns(ss, ps)
    isnothing(out) && return ∅
    ss, ps = out

    debug && @show :constant, ss, ps, Θ

    # matched variables first
    # for each σ we might get a different set of ss, ps after
    # this needs to branch out
    Θc = ∅

    for σ ∈ Θ
        # XXX reduce xx, ps
        out = _match_matched_variables(ss, ps, σ)
        out == ∅ && return ∅
        ss, ps = out

        debug && @show :matched, ss, ps, σ

        out = _match_non_variable_patterns(ss, ps, fₐ, σ)
        out == ∅ && return ∅
        ss, ps, Θ′ = out

        out = _mnvp(ss, ps, fₐ, σ)
        ss, ps, Θ′ = out

        debug && @show :non_variable, ss, ps, Θ′
        @show collect(Θ′)
        
        for σ′ ∈ Θ′
            ## then repeat matched variable ...
            @show σ′
            out  = _match_matched_variables(ss, ps, σ′)
            #out  = _match_matched_variables(ss, ps, first(σ′)            )
            out == ∅ && return out
            ss, ps = out

            debug && @show :matched2, ss, ps, σ′

            Θ′ = (σ′,)
            # regular variables p ∈ 𝑋₀ and then sequence variables
            if isempty(ps)
                σ′ != () && (Θc = union_match_sets(Θc, Θ′))
            else
                for out in _match_regular_variables(ss, ps, fₐ, σ′)
                    debug && @show :regular, out
                    ss, ps, σ = out # XX \sigma or Θ
                    Θ′ = (σ, )
                    if length(ps) > 0
                        #Θ′ = _match_sequence_variables(ss, ps, fₐ, σ)
                        Θ′ = _msv(ss, ps, fₐ, σ)                        
                    end
                    Θc = union_match_sets(Θc, Θ′)
                end
            end
        end
    end

    return Θc

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

function _match_constant_patterns(ss, ps)
    pred(a) = any(any(_is_𝑋(u) for u in s) for s in free_symbols(a))
    Pconst = filter(!pred, ps)
    for p ∈ Pconst
        p in ss || return nothing
        ss = filter(!=(p), ss)
    end
    ps = filter(p -> p ∉ Pconst, ps)
    (ss, ps)
end

# trims down ss, ps
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

# return () or (ss, ps, Θ)
# XXX THIS IS WRONG
function _match_non_variable_patterns(ss, ps, fc=nothing, σ=())
    ps′′, ps′ = tuplesplit(!iscall, ps)
    length(ps′) == 0 && return (ss, ps, (σ,))

    ss′′, ss′ = tuplesplit(!iscall, ss)
    length(ps′) == length(ss′) || return ∅

    Θᵣ = ∅
    for inds ∈ Combinatorics.permutations(1:length(ss′))
        ss′′′ = ss′[inds]
        Θ′ = (σ,)
        for (s,p) ∈ zip(ss′′′, ps′)
            operation(s) == operation(p) || continue
            Θ′ = MatchSequence(arguments(s), arguments(p), fc, Θ′)
            Θ′ == ∅ && continue
        end
        Θ′ == ∅ && continue
        Θᵣ = union_match_sets(Θᵣ, Θ′)
    end
    Θᵣ == ∅ && return ∅
    ss′′, ps′′, Θᵣ
end

# match non_variable_patterns
# return iterator of (ss, ps, σ)
function _mnvp(ss, ps, fc=nothing, σ=())
    ps′′, ps′ = tuplesplit(!iscall, ps)
    length(ps′) == 0 && return Iterators.map(identity, (ss, ps, (σ,)))
    ss′′, ss′ = tuplesplit(!iscall, ss)
    length(ps′) == length(ss′) || return ∅
    
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
    ss′′, ps′′, Iterators.filter(!isnothing, ii)
end

    

# return container of ss, ps, sigma
function _match_regular_variables(ss, ps, fc=nothing, σ = ())
    # fₐ is  commutative, maybe associative
    #isassociative(fc) && return ((ss, ps, σ),)
    isassociative(fc) && return Iterators.map(identity, ((ss, ps, σ),))
    

    ps_reg, ps′′ = tuplesplit(_is_Wild, ps)
    #isempty(ps_reg) && return ((ss, ps, σ),)
    isempty(ps_reg) && Iterators.map(identity, ((ss, ps, σ),))

    if length(ps_reg) < length(ss)
        if ps_reg == ps
            # can't match, not enough
            return ()
        end
    end

    dp = _countmap(ps_reg)
    ds = _countmap(ss)

    out = _st(ds, dp)

    out = Iterators.filter(ab -> iscompatible(first(ab), σ), out)
    return Iterators.map(out) do (σ′, ds)
        σ = union_match(σ, σ′)
        (_uncountmap(ds), ps′′, σ)
    end

    #out = _split_take(ds, dp)
    out = [(union_match(σ, σ′), ds) for (σ′,ds) ∈ out]
    # return ss, ps, σ for each in out
    tuple(
        ((_uncountmap(ds), ps′′, σ) for (σ, ds) ∈ out)...
    )

end

# different ways to grab the pie
function _split_take(ds, dp)
    out = []
    n = length(ds)
    k = length(dp)
    for inds in Iterators.product((1:n for _ in 1:k)...)
        ds′ = copy(ds)
        σ = ()
        for (i, (p, np)) ∈ zip(inds, (dp))
            s, ns = ds′[i]
            np > ns && (σ = (); break) # won't fit
            ds′[i] = s => (ns - np)
            σ = union_match(σ, ((p => s),))
        end
        σ == () && continue
        push!(out, (σ, ds′))
    end
    out
end

# XXX make an interator, not ..
function _st(ds, dp)
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


function _match_sequence_variables(ss, ps, fc=nothing, σ = ())
    if isassociative(fc)
        !isempty(filter(_is_Wild, ps)) && return ()
    end
    λ = (x -> _is_Wild(x) ||  _is_Plus(x)) 
    vs = tuplesplit(λ, ps)
    length(first(vs)) > length(ss) && return () # too many plus variables

    ds = _countmap(ss)
    dplus, dstar = _countmap(first(vs)), _countmap(last(vs))

    Θ = brute_force_enumeration(ds, dplus, dstar, fc, σ)

    return Θ
end



# bruteforce enumeration of possible values (defn 3.1)
# working with tuples likely an issue
function brute_force_enumeration(ds, dplus, dstar, fₐ, σ′=())
    pluses = tuple((v for (k,v) in dplus)...)
    stars = tuple((v for (k,v) in dstar)...)
    ss = tuple((v for (k,v) in ds)...)

    vars = TupleTools.vcat(tuple(first.(dplus)...), tuple(first.(dstar)...))
    svars = tuple(first.(ds)...)

    n1, n2 = length(pluses), length(stars)
    n = n1 + n2
    ks = TupleTools.vcat(pluses, stars)
    i = ntuple((a) -> 0, Val(n))

    Θ = ()
    h = isnothing(fₐ) ? identity :
        ((as) -> _maketerm(fₐ, as))
    for u ∈ Iterators.product(
        (Iterators.product((0:s for _ in 1:n)...) for s in ss)...)
        all(sum(ui .* ks) == si for (ui,si) in zip(u, ss)) || continue
        all(sum(ui[i] for ui in u) > 0 for i in 1:n1) || continue
        σ = ()
        for (j, v) ∈ enumerate(vars)
            vv = ()
            for (i,s) in enumerate(svars)
                vi = ntuple((_) -> s, Val(u[i][j]))
                vv = TupleTools.vcat(vv, vi)
            end
            if vv != ()
                σ = TupleTools.vcat(σ, (v => h(vv),))
            end
        end
        if iscompatible(σ′, σ)
            σ = union_match(σ′, σ)
            Θ = TupleTools.vcat(Θ, (σ,))
        end
    end
    Θ
end

function _msv(ss, ps, fc=nothing, σ′ = ())
    if !isassociative(ps)
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
    ss = tuple((v for (k,v) in ds)...)
    ii = Iterators.filter(Iterators.product(
        (Iterators.product((0:s for _ in 1:n)...) for s in ss)...)) do u
            all(sum(ui .* ks) == si for (ui,si) in zip(u, ss)) &&
                all(sum(ui[i] for ui in u) > 0 for i in 1:n1)
        end

    iii = Iterators.map(ii) do u
        σ = ()
        for (j, v) ∈ enumerate(vars)
            vv = ()
            for (i,s) in enumerate(svars)
                vi = ntuple((_) -> s, Val(u[i][j]))
                vv = TupleTools.vcat(vv, vi)
            end
            if vv != ()
                σ = TupleTools.vcat(σ, (v => h(vv),))
            end
        end
        if iscompatible(σ′, σ)
            σ = union_match(σ′, σ)
        else
            nothing
        end
    end

    Iterators.filter(!isnothing, iii)

    #=
    for u ∈ Iterators.product(
        (Iterators.product((0:s for _ in 1:n)...) for s in ss)...)
        all(sum(ui .* ks) == si for (ui,si) in zip(u, ss)) || continue
        all(sum(ui[i] for ui in u) > 0 for i in 1:n1) || continue
        σ = ()
        for (j, v) ∈ enumerate(vars)
            vv = ()
            for (i,s) in enumerate(svars)
                vi = ntuple((_) -> s, Val(u[i][j]))
                vv = TupleTools.vcat(vv, vi)
            end
            if vv != ()
                σ = TupleTools.vcat(σ, (v => h(vv),))
            end
        end
        if iscompatible(σ′, σ)
            σ = union_match(σ′, σ)
            Θ = TupleTools.vcat(Θ, (σ,))
        end
    end
    Θ
    =#
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

    m = match(u, ex)
    if !isnothing(m)
        m == () && return v

        σ = first(m)
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
