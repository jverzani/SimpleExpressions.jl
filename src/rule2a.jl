#=
This ends up being more general than rule2.jl *but* is about 4 times slower

For one test suite, we have these results:

julia> @btime RR(ts′); <-- rule2.jl
  26.541 μs (869 allocations: 32.77 KiB)

julia> @btime R2a(ts′); <--- rule2a.jl
  179.750 μs (2706 allocations: 106.19 KiB)

julia> @btime ACm($ts′); <--- AssociativeCommutativePatternMatching._match
872.417 μs (13155 allocations: 569.59 KiB)

## After changes
Rule2 (27/30) -- all failures okay
julia> @btime R2a($ts′);
  171.834 μs (2165 allocations: 82.77 KiB)

Rule1 (21/30)
julia> @btime RR($ts′)
  24.167 μs (870 allocations: 32.80 KiB) <--- half the allocations

Rule
=#

#using SimpleExpressions
#using SimpleExpressions: unwrap_const
using TermInterface
using Combinatorics

## ---- note -----
#=
Using Krebber, we have
* substitution (match) is σ a map between pattern terms and subject terms such that a substitution of the pattern terms returns the subject (σ(t) = s). This uses an ImmutableDict{Symbol, Any} to store a match or a partial match
* An empty substitution is partial match and a possible match, when there are not wild cards
* A sentinel is used to indicate *no possible subsitution* and is here a FAIL_DICT
* A set of matches (`θ` of `σs` allows for different matches due to commutivity/associativity. This set is empty if there are no matches. We use a vector to store this: MatchDict[MatchDict()] is an initial set with no initial match specified.

We have:
~x: match one argument
~!x: match one argument, possibly through a default
~~x: match 0, 1, or more arguments (returns a tuple)
~~~x: match 1 or more arguments (returns a tuple)


# test cases                       # ACPM  | Rule2 | Rule1 (match only)
eachmatch(:(~x), a + b + c)        # 1 | 1 | yes
eachmatch(:(~x + ~y), a + b + c)   # 6 | 0 | no
eachmatch(:(~x + ~!y), a + b + c)  # 6 | 1 | yes
eachmatch(:(~x + ~~y), a+b+c)      # 7^ | 4 | no # ^~x accts like ~~~x
eachmatch(:(~x + ~~~y), a+b+c)     # 6 | 3 | not

# for rule2
eachmatch(:(~x + ~~y), a+b+c) gives
 Base.ImmutableDict(:y => (b, c), :x => a)
 Base.ImmutableDict(:y => (a, c), :x => b)
 Base.ImmutableDict(:y => (a, b), :x => c)
 Base.ImmutableDict(:x => a + b + c, :y => ()) # is correct if ~x is just ~x

eachmatch(:(~x + ~~~y), a+b+c)     # 6 | 0 | no  # Rule2a is wrong here
should give 3 matches, not 0      # <---- ERROR IS HERE
=#

## ---- utils.jl -----


const M = MatchDict = Base.ImmutableDict{Symbol, Any}
const FAIL_DICT = MatchDict(:_fail, 0)

const NO_MATCH = ∅ = MatchDict[]
𝑀 = Vector{MatchDict}


const PREDICATE_FN_CACHE = IdDict{Any, Any}()

##
_unwrap_const(x) = unwrap_const(x)

##
match_dict() = MatchDict()

function match_dict(kvs::Pair...)::M
    σ = MatchDict()
    match_dict(σ, kvs...)
end

function match_dict(σ::MatchDict, kvs::Pair...)::M
    for (k,v) ∈ kvs
        v = isa(v,Number) ? unwrap_const(v) : v
        if haskey(σ, k)
            σk = σ[k]
            σk != v && error("repeated key with different value: $k => $v ($σk)")
        else
            σ = MatchDict(σ, k, v)
        end
    end
    σ
end

#  σ △ σ′ (\bigtriangleup) for every x in the intersection of the domains has same value
function iscompatible(σ::MatchDict, σ′::MatchDict)::Bool
    isempty(σ) && return true
    isempty(σ′) && return true
    for (k, v) ∈ σ
        if haskey(σ′, k) # intersect(keys(σ), keys(σ′)) allocates
            isequal(v, σ′[k]) || return false
        end
    end
    return true
end

# σ ⊔ σ′ (\sqcup) is union of two compatible matches
function merge_match(σ::MatchDict, σ′::MatchDict)::M
    # assume compatible
    for (k,v) ∈ σ′
        σ = match_dict(σ, k => v)
    end
    σ
end
## XXXmerge_match(σ::Tuple, σ′::MatchDict) = σ′

function union_merge(θ::𝑀, σ′::MatchDict)::𝑀
    σ′ == FAIL_DICT && return NO_MATCH
    MatchDict[merge_match(σ, σ′) for σ ∈ θ if iscompatible(σ, σ′)]
end

## AI generated union_merge
function _merge_single(σs::𝑀, σ′::M)::𝑀
    σ′ == FAIL_DICT && return NO_MATCH
    σs == NO_MATCH &&  return NO_MATCH
    merged = MatchDict[]
    sizehint!(merged, length(σs))
    for σ ∈ σs
        iscompatible(σ, σ′) || continue
        push!(merged, merge_match(σ, σ′))
    end
    return isempty(merged) ? MatchDict[] : merged
end

## AI generated union_merge with k=>v
function _merge_single_expr(σs, k, v)
    σ = match_dict(k => v)
    _merge_single(σs, σ)
    #=
    σs == FAIL_DICT && return FAIL_DICT
    merged = MatchDict[]
    sizehint!(merged, length(σs))
    for σ ∈ σs
        if haskey(σ, k)
            isequal(σ[k], v) && push!(merged, σ)
        else
            push!(merged, match_dict(σ, k => v))
        end
    end
    return isempty(merged) ? FAIL_DICT : merged
    =#
end


function union_merge(θ::𝑀, θ′::𝑀)::𝑀
    [merge_match(σ, σ′) for σ ∈ θ for σ′ ∈ θ′ if iscompatible(σ, σ′)]
end

## utils
_isone(x) = isequal(x, 1)
_groupby(pred, t) = (t = filter(pred,t), f=filter(!pred, t))

function _split_segments(arg_rule)
    seg = Expr[]
    notseg = Any[]
    seg_positions = Int[]
    for (i, pat) ∈ enumerate(arg_rule)
        if is_segment(pat) || is_plus(pat)
            push!(seg, pat)
            push!(seg_positions, i)
        else
            push!(notseg, pat)
        end
    end
    return seg, notseg, seg_positions
end

function _tuple_without_indices(arg_data, ind, n)
    vals = Any[]
    sizehint!(vals, n - length(ind))
    k = 1
    nextskip = first(ind)
    @inbounds for i ∈ 1:n
        if k <= length(ind) && i == nextskip
            k += 1
            nextskip = k <= length(ind) ? ind[k] : 0
        else
            push!(vals, arg_data[i])
        end
    end
    return tuple(vals...)
end


## Expression related methods
_is_operation(op) = ex -> iscall(ex) && operation(ex) ∈ (op, Symbol(op))

# need to compare x and p when p is from an expression
# trick -- SymEngine.Basic <: Number
# compare Number, Expr, Irrational, Symbol

eq_expr(a::Any, b::Any) = isequal(unwrap_const(a), unwrap_const(b))
eq_expr(a::Expr, b::Expr) = !isnothing(syntactic_match(unwrap_const(a), unwrap_const(b)))

## to evaluate a guard. (Where is the question?)
function _resolve_predicate(pred)
    haskey(PREDICATE_FN_CACHE, pred) && return PREDICATE_FN_CACHE[pred]

    pred_fn = pred
    if pred isa Symbol
        for M ∈ (@__MODULE__, Main, Base)
            if isdefined(M, pred)
                pred_fn = getfield(M, pred)
                break
            end
        end
        isa(pred_fn, Symbol) && (pred_fn = Core.eval(Main, pred))
    elseif pred isa Expr
        for M ∈ (@__MODULE__, Main, Base)
            try
                pred_fn = Core.eval(M, pred)
                break
            catch err
                # fall through if this expression is not yet resolvable in this module
            end
        end
    end

    PREDICATE_FN_CACHE[pred] = pred_fn
    return pred_fn
end

function _evalguard(pred, data)
    pred_fn = _resolve_predicate(pred)
    if pred_fn isa Function
        return try
            Base.invokelatest(pred_fn, _unwrap_const(data))
        catch err
            false
        end
    end

    try
        Base.invokelatest(eval(pred), _unwrap_const(data))
    catch err
        try
            return invokelatest(Main.eval(pred), _unwrap_const(data))
        catch err
            false
        end
    end
end


# create a term for a pattern (pterm) or a subject (sterm)
# the former is only for expressions
# the latter might involve a symbolic type
function pterm(op::Union{Expr,Symbol}, args; elide=true)
    if elide && length(args) == 1 && op ∈(:+, :*, :^, :/)
        return only(args)
    else
        return maketerm(Expr, :call, (op, args...), nothing)
    end
end

# symbolic type

# to pass to maketerm (sterm)
# Might want to do something like
# AssociativeCommutativePatternMatching.symtype(::SymEngine.Basic) = SymEngine.Basic
symtype(::Real) = Expr
symtype(::Symbol) = Expr
symtype(::Expr) = Expr
symtype(::T) where T = T

# create a term on the subject side
function sterm(op, args)
    S = symtype(first(args))
    sterm(S, op, args)
end

# construct term of abstract type S from op and args
function sterm(S, op, args)
    if S == Expr
        !isa(op, Union{Expr, Symbol}) && (op = nameof(op))
        return pterm(op, args)
    end

    if isa(op, Symbol)
        for M ∈ (@__MODULE__, Main, Base)
            if isdefined(M, op)
                op = getfield(M, op)
                break
            end
        end
        isa(op, Symbol) && (op = Core.eval(Main, op))
    elseif isa(op, Expr)
        for M ∈ (@__MODULE__, Main, Base)
            try
                op = Core.eval(M, op)
                break
            catch err
                # fall through if the expression is not yet resolvable in this module
            end
        end
    end

    maketerm(S, op, args, nothing)
end


# invert an expr to regularize a/b --> a*b^{-1}
function _invert_expr(pat)
    if isa(pat, Integer)
        return pterm(:^, (pat, -1.0))
    elseif is_operation(:(//))(pat)
        u,v = arguments(pat)
        u′ = isa(u, Number) ? -u : pterm(:*, (u,-1))
        return pterm(:(//), (u′, v))
    else
        return pterm(:^, (pat, -1))
    end
end

# --- basic total order, can override for other types
<ₑ(x::Symbol, y::Symbol) = x < y
<ₑ(x::Any, y::Any) = <ₑ(Symbol(x), Symbol(y))


# ----- predicates
_is_rational(x) = isa(_unwrap_const(x), Rational)

# can override, say with :Symbol
iscommutative(op) = op ∈ (:+, :*, +, *)
isassociative(op) = op ∈ (:+, :*, +, *)

isassociative(::typeof(+)) = true
isassociative(::typeof(*)) = true

iscommutative(::typeof(+)) = true
iscommutative(::typeof(*)) = true

# check for wildcard variables
is_𝑋(x::Any) = false
has_𝑋(x::Any) = false
is_slot(x::Any) = false
is_defslot(x::Any) = false
is_segment(x::Any) = false
is_plus(x::Any) = false
is_op(x::Any) = false

# Expr
is_𝑋(x::Expr) = (iscall(x) && operation(x) === :(~))  ||
    ((!iscall(x) && isexpr(x)) && head(x) != :... && is_𝑋(first(x.args)))

function has_𝑋(x::Expr)
    is_𝑋(x) && return true
    !iscall(x) && return false
    is_𝑋(operation(x)) && return true
    any(has_𝑋, arguments(x))
end

function is_slot(x::Expr)
    is_𝑋(x) || return false
    _, x = x.args
    iscall(x) && return false
    return true
end

function is_segment(x::Expr)
    is_𝑋(x) || return false # first is ~
    h,x = x.args
    is_𝑋(h) && return false # an op
    is_𝑋(x) || return false # second is ~
    _, x = x.args
    is_𝑋(x) && return false
    return true
end

# ~~~x (1 or more)
function is_plus(x::Expr)
    is_𝑋(x) || return false
    _,x = x.args
    is_𝑋(x) || return false
    _,x = x.args
    is_𝑋(x) || return false
    return true
end

# (~G)(~x)
function is_op(x::Expr)
    is_𝑋(x) && iscall(x) && is_𝑋(operation(x))
end

function is_defslot(x::Expr)

    is_𝑋(x) || return false
    _, arg = x.args
    is_operation(:(!))(arg) && return true

    return false
end

has_defslot(::Any) = false
function has_defslot(x::Expr)
    return is_defslot(x) ||
        (is_operation(:^)(x) && is_defslot(last(arguments(x))))
end

is_slot_or_defslot(x) = is_slot(x) || is_defslot(x)


## ------
const defslot_op_map = Dict(:+ => 0, :* => 1, :^ => 1, :/ => 1)

# return symbol holding variable name
varname(x::Symbol) = x
function varname(x::Expr)
    iscall(x) && !(x.args[1] ∈ (:~, :!)) && throw(ArgumentError("$x is not a wild card variable"))
    if x.args[1] ∈ (:~, :!)
        varname(x.args[2])
    else
        varname(x.args[1])
    end
end

## -- work with guards
# return true *if* either var has no predicate or
# predicate(data) is true
# use like pass_any_guard(var, data) || return ∅
function pass_any_guard(var, data)
    !has_predicate(var) && return true

    pred = get_predicate(var)
    pred_fn = _resolve_predicate(pred)

    # Avoid re-evaluating user predicates repeatedly when they are already callables.
    if pred_fn isa Function
        return try
            Base.invokelatest(pred_fn, _unwrap_const(data))
        catch err
            false
        end
    end

    # Fallback for unusual dynamic forms.
    try
        return Base.invokelatest(eval(pred), _unwrap_const(data))
    catch err
        try
            return invokelatest(Main.eval(pred), _unwrap_const(data))
        catch err
            false
        end
    end
end

# Does wildcard have a predicate?
has_predicate(::Any)::Bool = false
has_predicate(x::Symbol)::Bool = false
function has_predicate(x::Expr)::Bool
    if x.args[1] ∈ (:~, :!)
        has_predicate(x.args[2])
    else
        length(x.args) == 2 && x.head==:(::)
    end
end

# get_predicate. Assumes user has called `has_predicate` and got TRUE
get_predicate(x::Symbol) = :nothing
function get_predicate(x::Expr)
    if x.args[1] ∈ (:~, :!)
        get_predicate(x.args[2])
    else
        x.args[2]
    end
end


## ----- rule2.jl -------

# This is derived from https://github.com/JuliaSymbolics/SymbolicIntegration.jl/tree/main/src/methods/rule_based/rule2.jl
# Licensed under MIT with Copyright (c) 2022 Harald Hofstätter, Mattia Micheletta Merlin, Chris Rackauckas, and other contributors


# TODO ~a*(~b*~c) currently will not match a*b*c . a fix is possible

# for when the rule contains a symbol, like ℯ, or a literal number
function check_expr_r(data, rule::Real, σs::𝑀)::𝑀
    eq_expr(rule, data) && return σs
    return NO_MATCH
end

function check_expr_r(data, rule::Symbol, σs::𝑀)::𝑀
    eq_expr(data, rule) && return σs
    return NO_MATCH
end

# main function
check_expr_r(data, rule::Expr)::𝑀 = check_expr_r(data, rule, [MatchDict()])
function check_expr_r(data, rule::Expr, σs::𝑀)::𝑀
    # @show :cer, data, rule
    if !iscall(rule)
        #@show :what_is, rule
    end

    opᵣ = operation(rule)

    if is_𝑋(opᵣ)
        # @show :is_𝑋, data, rule
        # peel off hope for single argument!
        !iscall(data) && return MatchDict[] # XXX <---

        value = iscall(data) ? operation(data) : identity
        σ′ = match_dict(varname(opᵣ) => value)
        σs = union_merge(σs, σ′)
        arg_data, arg_rule = arguments(data), arguments(rule)
        if length(arg_data) > 1
            if iscommutative(opᵣ)
                return check_commutative(arg_data, arg_rule, σs)
            else
                return ceoaa(arg_data, arg_rule, σs)
            end
        else
            data, rule = (isempty(arg_data) ? arg_data : only(arg_data)), only(arg_rule)
        end
    end

    # rule is a single variable
    if is_𝑋(rule)
        return just_variable(data, rule, σs)
    end

    # if there is a deflsot in the arguments
    i = findfirst(is_defslot, arguments(rule))
    if i !== nothing
        return has_defslot(i, data, rule, σs)
    end

    # if there is a segment in the (only) argument
    if (iscall(rule) &&
        length(arguments(rule)) == 1 &&
        is_segment(first(arguments(rule)))
#        (is_segment(first(arguments(rule))) ||
#         is_plus(first(arguments(rule))))
        )
        # @show :hi
        return only_argument_is_segment(data, rule, σs)
    end

    # rule is a normal call, check operation and arguments
    if (operation(rule) == ://) && _is_rational(data)
        return has_rational(data, rule, σs)
    end

    !iscall(data) && return NO_MATCH

    # check opᵣ for special cases where
    # powers are represented differently
    opᵣ, 𝑜𝑝ₛ = operation(rule), Symbol(operation(data))
    opₛ = Symbol(𝑜𝑝ₛ)
    if opᵣ ∈ (:^, :sqrt, :exp) ||
        (opᵣ, opₛ) ∈ ((:/,:^),
                      (:/,:*),
                      )
        return different_powers(data, rule, σs)
    end


    # gimmick to make Neim work in some cases:
    # * if data is a division transform it to a multiplication
    # (the final solution would be remove divisions form rules)
    # * if the rule is a product, at least one of the factors is a power, and data is a division
    neim_pass, arg_data, arg_rule = neim_rewrite(data, rule)
    opₛ != opᵣ && !neim_pass && return NO_MATCH

    # segments variables means number of arguments might not match
    if (any(is_segment, arg_rule))
        # @show :has_any
        return has_any_segment(𝑜𝑝ₛ, arg_data, opᵣ, arg_rule,  σs)
    end

    if (any(is_plus, arg_rule))
        # @show :has_plus, arg_data, arg_rule
        return has_any_plus(𝑜𝑝ₛ, arg_data, opᵣ, arg_rule,  σs)
    end


    (length(arg_data) != length(arg_rule)) && return MatchDict[]
    if iscommutative(opᵣ)
        σ′s = check_commutative(arg_data, arg_rule, σs)
        return isempty(σ′s) ? NO_MATCH : σ′s
    end
    # normal checks
    return ceoaa(arg_data, arg_rule, σs)
end

# check expression of all arguments
# elements of arg_rule can be Expr or Real
function ceoaa(arg_data, arg_rule, σs::𝑀)::𝑀
    if all(is_𝑋, arg_rule) && !any(is_op, arg_rule)
        nseg = count(is_segment, arg_rule) # no segment? need same wild
        iszero(nseg) && count(is_slot, arg_rule) != length(arg_data) &&
            return NO_MATCH
    end
    σs == NO_MATCH && return NO_MATCH
    if (any(is_segment, arg_rule))
        return has_any_segment(nothing, arg_data, nothing, arg_rule,  σs)
    end
    if (any(is_plus, arg_rule))
        return has_any_plus(nothing, arg_data, nothing, arg_rule,  σs)
    end
    σ′s = σs
    for (a, b) in zip(arg_data, arg_rule)
        σ′s = check_expr_r(a, b, σ′s)
        σ′s == NO_MATCH && return NO_MATCH
    end
    return σ′s
end

# match a single variable
function just_variable(data, rule, σs::𝑀)::𝑀
    # @show :jv, data, rule
    @assert is_𝑋(rule)
    var = varname(rule)
    val = is_segment(rule) ? (data,) : data
    isempty(σs) && return NO_MATCH
    ms = MatchDict[]
    for σ ∈ σs
        if haskey(σ, var) # if the slot has already been matched
            σvar = σ[var]
            isequal(σvar, val) && push!(ms, σ)
        else
            # if never been matched
            if has_predicate(rule)
                pred = get_predicate(rule)
                !_evalguard(pred, val) && continue
            end
            push!(ms, match_dict(σ, var=> val))
        end
    end
    return isempty(ms) ? NO_MATCH : ms
end

# expression has defslot
function has_defslot(i, data, rule, σs)
    # @show :has_defslot, data, rule
    op = operation(rule)
    if op ∈ (:^, :/)
        i == 1 && return MatchDict[]
    end
    ps = copy(arguments(rule))
    pᵢ = ps[i]
    qᵢ = :(~$(pᵢ.args[2].args[2]))
    ps[i] = qᵢ

    # build rule expr without defslot and check it
    newr = Expr(:call, operation(rule), ps...) # not pterm here!
    σ′s = check_expr_r(data, newr, σs)
    σ′s != MatchDict[] && return σ′s # had a match

    # if no normal match, check only the non-defslot part of the rule
    deleteat!(ps, i)
    tmp = pterm(operation(rule), ps)
    σs = check_expr_r(data, tmp, σs)
    σs == MatchDict[] && return MatchDict[]

    var = varname(qᵢ)
    value = get(defslot_op_map, operation(rule), -1)
    _merge_single_expr(σs, var, value)

end

function only_argument_is_segment(data, rule, σs, op=nothing)
    # @show :only_argument_is_segment, data, rule
    !iscall(data) && return MatchDict[]
    opₛ, opᵣ = Symbol(operation(data)), operation(rule)
    opₛ == opᵣ || return MatchDict[]

    # return the whole data (not only vector of arguments as in rule1)
    var = varname(only(arguments(rule)))
    _merge_single_expr(σs, var, data)
end

function has_rational(data, rule, σs)
    # @show :has_rational, data, rule
    # rational is a special case, in the integration rules is present only in between numbers, like 1//2
    as = arguments(rule)
    data = _unwrap_const(data)
    data.num == first(as) && data.den == last(as) && return σs
    # r.num == rule.args[2] && r.den == rule.args[3] && return matches::MatchDict
    return MatchDict[]
end


# make powers equivalent for checking
# e.g. sqrt(x) --> x^(1//2)
function different_powers(data, rule, σs::𝑀)::𝑀
    # @show :different_powers, data, rule
    opᵣ, opₛ = operation(rule), Symbol(operation(data))
    arg_data = arguments(data)
    arg_rule = arguments(rule)
    b = first(arg_data)

    if opᵣ === :^
        # try first normal checks
        if (opₛ === :^)
            σ′s = ceoaa(arg_data, arg_rule, σs)
            !isempty(σ′s) && return σ′s
        end

        # try building frankestein arg_data (fad)
        fad = Vector{Any}(undef, 2)
        is1divsmth = (opₛ == :/) && isequal(1, _unwrap_const(first(arg_data)))
        if is1divsmth && _is_operation(^)(arg_data[2]) #iscall(arg_data[2]) && (Symbol(operation(arg_data[2])) == :^)

            # if data is of the alternative form 1/(...)^(...)
            m = arg_data[2]
            fad[1] = arguments(m)[1]
            fad[2] = -1*arguments(m)[2]

        elseif is1divsmth && _is_operation(sqrt)(arg_data[2]) #iscall(arg_data[2]) && (Symbol(operation(arg_data[2])) == :sqrt)
            # if data is of the alternative form 1/sqrt(...),
            # it might match with exponent -1//2
            m = arg_data[2] # like b^m
            fad[1] = arguments(m)[1]
            fad[2] = -1//2

        elseif is1divsmth && _is_operation(exp)(arg_data[2]) #iscall(arg_data[2]) &&
            #(Symbol(operation(arg_data[2])) === :exp)
            # if data is of the alternative form 1/exp(...),
            # it might match ℯ ^ -...
            m = arg_data[2] # like b^m
            pow = first(arguments(m))

            fad[1] = ℯ
            fad[2] = sterm(-, (pow,))

        elseif is1divsmth
            # if data is of the alternative form 1/(...),
            # it might match with exponent = -1
            m = arg_data[2] # like b^m
            fad[1] = m
            fad[2] = -1
        elseif (opₛ  === :^) && iscall(b) &&
            (Symbol(operation(b)) === :/) &&
            _isone(arguments(b)[1])

            # if data is of the alternative form (1/...)^(...)
            m = arg_data[2] # like b^m
            fad[1] = arguments(b)[2]
            fad[2] = -1*m

        elseif opₛ === :exp

            # if data is a exp call, it might match with base e
            fad[1] = ℯ
            fad[2] = b

        elseif opₛ === :sqrt
            # if data is a sqrt call, it might match with exponent 1//2
            fad[1] = b
            fad[2] = 1//2
#        elseif opₛ === :/
#            # rule is ^ we have /, turn into ^-1
#            #push!(fad, arguments(m)[1], -1*arguments(m)[2])
        else
            return MatchDict[]

        end
        return ceoaa(fad, arg_rule, σs)

    elseif opᵣ === :sqrt
        if (opₛ === :sqrt)
            tocheck = arg_data # normal checks
        elseif (opₛ === :^) && (_unwrap_const(arg_data[2]) ∈ (1//2, :(1//2))) #1//2)
            tocheck = (b,)
        else
            return MatchDict[]
        end

        return ceoaa(tocheck, arg_rule, σs)

    elseif opᵣ === :exp
        if (opₛ === :exp)
            tocheck = arg_data # normal checks
        elseif (opₛ === :^) && (_unwrap_const(b) ∈ (ℯ,:ℯ))
            m = arg_data[2]
            tocheck = (m,)
        else
            return MatchDict[]
        end

        return ceoaa(tocheck, arg_rule, σs)
    elseif (opᵣ, opₛ) == (:/, :*)
        # rule is / but may be canonicalized to
        # turn rule into ^-1 terms and check commutatively

        u,v = arguments(rule)
        vs = _is_operation(*)(v) ? arguments(v) : (v,)
        vs′ = map(_invert_expr, vs)
        arg_rule′ = if u == 1
            vs′
        else
            Any[u, vs′...]
        end
        return check_commutative(arg_data, arg_rule′, σs)

    elseif (opᵣ, opₛ) == (:/, :^)
        # :(1/~x^~n) ~ x^(-n)
        # rewrite rule as a * b^(-1)
        a, b = arguments(rule)
        if is_operation(:^)(b) # combine exponents
            u, v = arguments(b)
            if is_operation(:(//))(v)
                n,d = arguments(v)
                v′ = pterm(:(//), (-n, d))
            elseif !isa(u, Integer) && isa(v, Number)
                v′ = -v
            else
                v′ = pterm(:*, (v, -1.0))
            end

            b′ = pterm(:^, (u, v′))
            if a == 1
                rule′ = b′
            else
                rule′ = pterm(:*, (a, b′))
            end
        else
            rule′ = Expr(:call, :^, b, -1)
        end
        if !(isa(a, Number) && isone(a))
            rule′ = Expr(:call, :*, a, rule′)
        end
        return check_expr_r(data, rule′, σs)

    #end
    elseif (opᵣ, opₛ) == (:*, :/)
        u, v = arg_data
        v′ = sterm(^, [v, -1])
        return check_commutative((u, v′), arg_rule,  σs)
    end
end

function neim_rewrite(data, rule)
    neim_pass = false

    arg_rule, arg_data = arguments(rule), arguments(data)
    opᵣ, opₛ = operation(rule), Symbol(operation(data))
    if (opᵣ === :*) && opₛ === :/ && any(is_operation(:^), arg_rule)

        neim_pass = true

        n = arg_data[1]
        d = arg_data[2]
        # then push the denominator of data up with negative power
        sostituto = Any[]
        if iscall(d) && opₛ == :^ #(operation(d)==^)

            a, b, c... =  arg_data
            val = sterm(^, (a,b))
            push!(sostituto, val)

        elseif iscall(d) && opₛ == :*
            # push!(sostituto, map(x->x^-1,arguments(d))...)
            for factor in arguments(d)
                val = sterm(^, (factor, -1))
                push!(sostituto, val)
            end
        elseif iscall(d) && Symbol(operation(d)) == :^
            a,b = arguments(d)
            m = sterm(-, (b,))
            val = sterm(^, (a, m))
            push!(sostituto, val)
        else
            val = sterm(^, (d, -1))
            push!(sostituto, val)
        end

        new_arg_data = Any[]

        if iscall(n)
            if Symbol(operation(n)) === :*
                append!(new_arg_data, arguments(n))
            else
                push!(new_arg_data, n)
            end
        elseif !_isone(n)
            push!(new_arg_data, n)
            # else dont push anything bc *1 gets canceled
        end

        append!(new_arg_data, sostituto)

        arg_data = new_arg_data

        # printdb(4,"Applying neim trick, new arg_data is $arg_data")
    end
    return (neim_pass, arg_data, arg_rule)

end

function has_any_segment(𝑜𝑝ₛ, arg_data,
                         opᵣ, arg_rule, σs)
    # @show :has_any_segment, arg_data, arg_rule
    σs == NO_MATCH && return NO_MATCH
    seg, notseg, seg_positions = _split_segments(arg_rule)
    # @show seg, notseg, seg_positions
    n,m = length(arg_data), length(notseg)
    if m > n
        return MatchDict[]
    elseif m == 0
        # assign all to the first!
        σ′s = MatchDict[]

        var′, vars... = seg
        var = varname(var′)
        val = tuple(arg_data...)
        for σ ∈ σs
            if haskey(σ, var)
                σvar = σ[var]
                val == σvar && push!(σ′s,σ)
            else
                σ′ = match_dict(σ, var => val)
                for v ∈ vars
                    σ′ = match_dict(σ′, varname(v) => ())
                end
                push!(σ′s,σ′)
            end
        end# XXX?
        return σ′s
    elseif 0 < m ≤ n
        σ′′s = MatchDict[]
        if iscommutative(opᵣ)
            for ind ∈ combinations(1:n, m)
                # take m of the values and match
                sub′ = sterm(𝑜𝑝ₛ, arg_data[ind])
                pat′ = pterm(opᵣ, notseg) # can be an issue!
                for σ ∈ σs
                    σ′s = check_expr_r(sub′, pat′, [σ])
                    if σ′s != NO_MATCH
                        # we found a match, assign the rest to first segment
                        for σ′ ∈ σ′s
                            v = first(seg)
                            var = varname(v)
                            val = length(ind) < n ?
                                _tuple_without_indices(arg_data, ind, n) :
                                ()
                            if haskey(σ′, var)
                                val == σ′[var] && push!(σ′′s, σ)
                            else
                                if !has_predicate(v) ||
                                    (has_predicate(v) && _evalguard(get_predicate(v), val) )
                                    σ′ = match_dict(σ′, var=>val)
                                    push!(σ′′s, σ′)
                                end
                            end
                        end
                    end
                end
            end
        else
            # march over, use segment to slurp rest
            # this takes some thinking.
            # match ~a,~~b,~c,~~d against say l,m,n,o,p,q
            # has l|()|m|(nopq) # n - nontsegs + 1 choices for first
            #     l|(m)|n|(opq) # then ,,, + 1 for second (if more)
            #     l|(mn)|o|(pq) # then ... + 1 for third (if more)
            #     l|(mno)|p|(q)
            #     l|(mnop)|q|()
            nsegs = length(seg_positions)
            k = length(arg_rule) - nsegs
            n = length(arg_data) - k

            # non-performant partition iterator
            σ′′s =  MatchDict[]
            ranges = ntuple(_ -> 0:n, nsegs)
            for α ∈ Iterators.product(ranges...)
                sum(α) == n || continue
                σ′s = σs
                j = 1 # index in data_rule
                l = 1 # index in itr,
                nomatch = false
                for (i,pat) ∈ enumerate(arg_rule)
                    nomatch && continue
                    if l > nsegs || i != seg_positions[l]
                        σ′s = check_expr_r(arg_data[j], pat, σ′s)
                        σ′s == NO_MATCH && (nomatch = true)
                        j = j + 1
                    else
                        a = α[l]
                        l = l + 1
                        var = varname(pat)
                        #value = view(arg_data,j:(j+a-1))
                        value = arg_data[j:(j+a-1)]
                        σ′ = match_dict(var => value)
                        σ′s = _merge_single(σ′s, σ′)
                        σ′s == NO_MATCH && (nomatch = true)
                        j = j + a
                    end
                end
                σ′s == NO_MATCH && continue
                !nomatch && append!(σ′′s, σ′s)
            end
            return isempty(σ′′s) ? NO_MATCH : σ′′s
        end
        if length(seg) > 0
            # match all segments with (), then match the rest
            σ′′′ = match_dict()
            for v ∈ seg
                if is_plus(v)
                    σ′′′ = FAIL_DICT
                    break
                else
                    σ′′′ = match_dict(σ′′′, varname(v) => ())
                end
            end
            σ′′′s = union_merge(σs, σ′′′)
            sub′ = sterm(𝑜𝑝ₛ, arg_data)
            pat′ = pterm(opᵣ, notseg)
            σ′′′s = check_expr_r(sub′, pat′, σ′′′s)
            σ′′′s != NO_MATCH && append!(σ′′s, σ′′′s)
        end

        return isempty(σ′′s) ? NO_MATCH : σ′′s
    end
end

function has_any_plus(𝑜𝑝ₛ, arg_data,
                      opᵣ, arg_rule, σs)
    # @show :has_any_plus, arg_data, arg_rule
    has_any_segment(𝑜𝑝ₛ, arg_data,
                      opᵣ, arg_rule, σs)
end
@inline function _commutative_priority(pat)
    is_defslot(pat) && return 3
    has_predicate(pat) && return 2
    is_slot(pat) && return 4
    has_𝑋(pat) && return 1
    return 0
end

function _append_unique!(out, σs)
    for σ ∈ σs
        σ ∈ out || push!(out, σ)
    end
    return out
end

function _check_commutative!(out, used, arg_data, arg_rule, order, k, σs::𝑀)::𝑀
    # @show :check_commutative
    σs == NO_MATCH && return out
    k > length(order) && return _append_unique!(out, σs)

    pat = arg_rule[order[k]]
    for j ∈ eachindex(arg_data)
        used[j] && continue
        σ′s = check_expr_r(arg_data[j], pat, σs)
        σ′s == NO_MATCH && continue
        used[j] = true
        _check_commutative!(out, used, arg_data, arg_rule, order, k + 1, σ′s)
        used[j] = false
    end
    return out
end

function check_commutative(arg_data, arg_rule, σs::𝑀)::𝑀
    # commutative checks
    length(arg_data) != length(arg_rule) && return NO_MATCH

    order = sortperm(collect(eachindex(arg_rule)); by = i -> _commutative_priority(arg_rule[i]))
    used = falses(length(arg_data))
    σ′′s = MatchDict[]
    _check_commutative!(σ′′s, used, arg_data, arg_rule, order, 1, σs)
    return isempty(σ′′s) ? NO_MATCH : σ′′s
end

#=
# with vendor
julia> @time ACm()
  0.001601 seconds (13.70 k allocations: 690.031 KiB)
julia> @time R2();
0.005675 seconds (13.61 k allocations: 585.531 KiB)

# with main
julia> @time ACm(); @time ACm();
  4.766989 seconds (20.53 M allocations: 1.085 GiB, 2.54% gc time, 99.83% compilation time: <1% of which was recompilation)
  0.000964 seconds (13.70 k allocations: 690.031 KiB)

julia> @time R2(); @time R2();
  2.073399 seconds (12.07 M allocations: 650.931 MiB, 2.99% gc time, 99.92% compilation time)
  0.000170 seconds (1.29 k allocations: 48.156 KiB)

Tuple{Int64, Bool}[(1, 1), (2, 1), (3, 1), (4, 1), (5, 0), (6, 1), (7, 1), (8, 0), (9, 0), (10, 1), (11, 1), (12, 0), (13, 0), (14, 0), (15, 1), (16, 0), (17, 1), (18, 1), (19, 1), (20, 1), (21, 1), (22, 1), (23, 1), (24, 1), (25, 1), (26, 0), (27, 1), (28, 0), (29, 1), (30, 1)]

# with symbolic
julia> @time R2a(); @time R2a();
  1.765936 seconds (11.38 M allocations: 598.700 MiB, 3.66% gc time, 99.89% compilation time)
0.000190 seconds (2.97 k allocations: 116.016 KiB)

Tuple{Int64, Bool}[(1, 1), (2, 1), (3, 1), (4, 1), (5, 0), (6, 1), (7, 1), (8, 1), (9, 0), (10, 1), (11, 1), (12, 1), (13, 1), (14, 0), (15, 1), (16, 0), (17, 1), (18, 1), (19, 1), (20, 1), (21, 1), (22, 1), (23, 1), (24, 1), (25, 1), (26, 0), (27, 1), (28, 0), (29, 1), (30, 1)]
julia>

julia> @time R2a(); @time R2a();
  1.395543 seconds (10.01 M allocations: 523.788 MiB, 3.90% gc time, 99.88% compilation time)
  0.000196 seconds (2.97 k allocations: 115.969 KiB)


## apply_rules
ACMP
julia> @time SimpleExpressions.__apply_rules(ex, trigsimp)
  0.001960 seconds (9.68 k allocations: 384.828 KiB)
rule2a

julia> @time __apply_rules2(ex, trigsimp);
  0.000263 seconds (636 allocations: 24.812 KiB)
rule2

julia> @time SimpleExpressions.__apply_rules(ex, trigsimp);
  1.166126 seconds (7.17 M allocations: 385.129 MiB, 7.03% gc time, 99.95% compilation time)

julia> @time SimpleExpressions.__apply_rules(ex, trigsimp)
  0.000192 seconds (385 allocations: 14.703 KiB)

using Revise
using AssociativeCommutativePatternMatching
using SimpleExpressions
SimpleExpressions.@symbolic_variables a b c x y z

function __apply_rules2(x, rs)
    for r ∈ rs
        pat, rhs = r
        σs = (AssociativeCommutativePatternMatching.MatchDict(),)
        σs′ = AssociativeCommutativePatternMatching.check_expr_r(x, pat, σs)
        @show σs′, x, pat
        #σ = match(pat, x)
        if !isempty(σs′) #σ != FAIL_DICT
            σ = first(σs′)
            ex =  SimpleExpressions.rewrite(σ, rhs)
            return ex
        end
    end
    return x
end


ts = [
# single variables
(pat = :(~x),
 sub = :(a + b + c),
 len = 1),
(pat = :(~!x),
 sub = :(a + b + c),
 len = 1),
(pat = :(~~x),
 sub = :(a + b + c),
 len = 1),
(pat = :(~~~x),
 sub = :(a + b + c),
 len = 1),

# multiple variables
(pat = :(~x + ~y),
 sub = :(a + b + c),
 len = 6),
(pat = :(~x + ~!y),
 sub = :(a),
         len = 1),
        (pat = :(~x + ~!y),
         sub = :(a + b + c),
         len = 6),
        (pat = :(~x + ~~y),
         sub = :(a + b + c),
         len = 7),
        (pat = :(~x + ~~~y),
         sub = :(a + b + c),
         len = 6),
        (pat = :(~!x + ~~y),
         sub = :(a + b + c),
         len = 7),
        (pat = :(~!x + ~~~y),
         sub = :(a + b + c),
         len = 6),
        (pat = :(~~x + ~~y),
         sub = :(a + b + c),
         len = 8),
        (pat = :(~~x + ~~~y),
         sub = :(a + b + c),
         len = 7),
        (pat = :(~~~x + ~~~y),
         sub = :(a + b + c),
         len = 6),

        # def slot with ^
        (pat = :((~x)^(~!y)),
         sub = :(a),
         len = 1),
        (pat = :((~x)^(~y)),
         sub = :(a),
         len = 0),
        (pat = :((~x)^(~!y)),
         sub = :(a^2),
         len = 1),
         (pat = :(~x + (~y)^(~!z)),
         sub = :(a + b),
         len = 2),
        (pat = :(~!x + (~y)^(~!z)),
         sub = :(a + b),
         len = 2),

        # defslot combos
        (pat = :((~!a)*(~x)),
         sub = :(x),
         len = 1),
        (pat = :((~!a)*(~x) + (~!b)),
         sub = :(x),
         len = 1),


        # wrapped in functions

        (pat = :(log(~x) + log(~y)),
         sub = :(log(a) + log(b)),
         len = 2),
        (pat = :(log(~x) + ~!y),
         sub = :(log(a) + log(b)),
         len = 2),
        (pat = :(log(~x) + log(~y) + log(~z)),
         sub = :(log(a) + log(b) + log(c)),
         len = 6),



        (pat = :(log(1 + ~x)),
         sub = :(log(1 + x^2)),
         len = 1),
        (pat = :(log(1 + ~x)),
         sub = :(log(1 + x) + log(1 + x^2)),
         len = 0),
        (pat = :(log(1 + ~x) + ~!y),
         sub = :(log(1 + x) + log(1 + x^2)),
         len = 2),


        (pat = :(log(log(~~~x + ~~~y))),
         sub = :(log(log(a + b + c))),
         len = 6),
        (pat = :(log(log(~~~x + ~!y))),
         sub = :(log(log(a + b + c))),
         len = 6),
        (pat = :(~!x + log(log(~y))),
         sub = :(log(log(a)) + log(log(b))),
         len = 2),
]

ts′ = [(;pat, sub, len, sub′ = eval(sub)) for (pat, sub, len) ∈ ts]


# check Ac
function AC(ts)
    Ac = Any[]
    for (i, (pat, sub, len, sub′)) ∈ enumerate(ts)
        σs = AssociativeCommutativePatternMatching._eachmatch(pat, sub′)
        push!(Ac, (length(collect(σs)), len))
    end
    Ac
end

function ACm(ts′)
    Ac = Any[]
    for (i, (pat, sub, len, sub′)) ∈ enumerate(ts′)
        σ = AssociativeCommutativePatternMatching._match(pat, sub′)
        push!(Ac, (;success = !isnothing(σ)))
    end
    Ac
end

# check rule2
function RR(ts′)
    r2 = Any[]
    for (i, (pat, sub, len, sub′)) ∈ enumerate(ts′)
        σ = match(pat, sub′)
        #push!(r2, (;succes = σ != nothing))#SimpleExpressions.FAIL_DICT))
        push!(r2, (;succes = σ != SimpleExpressions.FAIL_DICT))
    end
    r2
end

function R2a(ts)
    r2 = Any[]
    for (i, (pat, sub, len, sub′)) ∈ enumerate(ts)
        out = SimpleExpressions.check_expr_r(sub′, pat)
        #@show out
        #push!(r2, (;succes = σ != nothing))#SimpleExpressions.FAIL_DICT))
        push!(r2, (;succes = !isempty(out)))
    end
    r2
end


## simplify tests
@symbolic x y
tests = [10*sin(x)^2 + 10 * cos(x)^2 + 10,
         sin(x^2)/cos(x^2),
         x - x,
         x^0,
         x^1,
         sqrt(x),
         cbrt(x),
         x^2 * x^3,
         (x^2)^3,
         exp(x) * exp(2x),
         exp(x)^2,
         4log(x) + 4log(x + y),
         3log(x),
         ]

function st()
    [tests SimpleExpressions.simplify.(tests)]
end


full match
julia> @time st(); @time st();
 32.813199 seconds (130.58 M allocations: 6.836 GiB, 2.51% gc time, 99.83% compilation time: <1% of which was recompilation)
0.014219 seconds (246.94 k allocations: 9.016 MiB)

rule2a
 20.495310 seconds (247.41 M allocations: 12.462 GiB, 6.16% gc time, 99.91% compilation time)
  0.006428 seconds (101.24 k allocations: 3.620 MiB)

current



#
current
julia> @symbolic x; pat = :((~!a) * sin(~x)^2 + (~!a) * cos(~x)^2 + ~!b); ex = sin(2x)^2 + cos(2x)^2; @time match(pat, ex)
0.000127 seconds (184 allocations: 6.922 KiB)

AC
julia> @symbolic x; pat = :((~!a) * sin(~x)^2 + (~!a) * cos(~x)^2 + ~!b); ex = sin(2x)^2 + cos(2x)^2; @time match(pat, ex)
  3.428782 seconds (20.78 M allocations: 1.080 GiB, 3.66% gc time, 99.92% compilation time)
Base.ImmutableDict{Symbol, Any} with 3 entries:
  :x => 2 * x
  :a => 1
  :b => 0

julia> @symbolic x; pat = :((~!a) * sin(~x)^2 + (~!a) * cos(~x)^2 + ~!b); ex = sin(2x)^2 + cos(2x)^2; @time match(pat, ex)
  0.000982 seconds (2.31 k allocations: 128.781 KiB)
Base.ImmutableDict{Symbol, Any} with 3 entries:
  :x => 2 * x
  :a => 1
  :b => 0

#rule2
julia> @symbolic x; pat = :((~!a) * sin(~x)^2 + (~!a) * cos(~x)^2 + ~!b); ex = sin(2x)^2 + cos(2x)^2; @time AssociativeCommutativePatternMatching.check_expr_r(ex, pat, (AssociativeCommutativePatternMatching.MatchDict(),))
  7.877769 seconds (131.67 M allocations: 6.585 GiB, 9.66% gc time, 99.99% compilation time)
1-element Vector{Base.ImmutableDict{Symbol, Any}}:
 Base.ImmutableDict(:b => 0, :a => 1, :x => 2 * x)

julia> @symbolic x; pat = :((~!a) * sin(~x)^2 + (~!a) * cos(~x)^2 + ~!b); ex = sin(2x)^2 + cos(2x)^2; @time AssociativeCommutativePatternMatching.check_expr_r(ex, pat, (AssociativeCommutativePatternMatching.MatchDict(),))
  0.000437 seconds (376 allocations: 14.312 KiB)
1-element Vector{Base.ImmutableDict{Symbol, Any}}:
 Base.ImmutableDict(:b => 0, :a => 1, :x => 2 * x)


=#
