# implementation specific definitions needed for matching in matchpy

const ExpressionType = SymbolicExpression

#=
_is_𝐿(x::AbstractSymbolic) = isa(x, 𝐿)
_is_𝐹₀(x::AbstractSymbolic) = all(isempty(u) for u in free_symbols(x))


function _is_Wild(x::𝑉) # 1
    𝑥 = string(Symbol(x))
    endswith(𝑥, "__") && return false
    endswith(𝑥, "_")
end

function _is_Plus(x::𝑉) # 1 or more
    𝑥 = string(Symbol(x))
    endswith(𝑥, "___") && return false
    endswith(𝑥, "__")
end

function _is_Star(x::SymbolicVariable) # 0, 1, or more
    𝑥 = string(Symbol(x))
    endswith(𝑥, "___")
end

function _is_𝑋(x::SymbolicVariable)
    𝑥 = string(Symbol(x))
    endswith(𝑥, "_")
end

# keep ⋯ as match so as not breaking
_is_Wild(x::SymbolicVariable{:⋯}) = true
_is_𝑋(x::SymbolicVariable{:⋯}) = true
=#

## ---- match, replace
"""
    match(pattern::Expr, subject::AbstractSymbolic)::MatchDict

For a pattern specified through an expression, return a dictionary of matches or a dictionary signaling failure

Uses vendored `rule2.jl` from `SymbolicIntegration` as this is more performant than `AssociativeCommutativePatternMatching`.

## Examples
```julia
julia> @symbolic x p
(x, p)

julia> match(:(~x * cos(~y)), p*cos(x))
Base.ImmutableDict{Symbol, SimpleExpressions.AbstractSymbolic} with 2 entries:
  :y => x
  :x => p
```


"""
function Base.match(pattern::Expr, subject::AbstractSymbolic)
    σ = MatchDict()
    check_expr_r(subject, pattern, σ)
end

function Base.match(pat::AbstractSymbolic, ex::AbstractSymbolic)
    return match(convert(Expr, pat), ex)
end


"""
    replace(ex::SymbolicExpression, args::Pair...)

Replace parts of the expression with something else.

Returns a symbolic object.

The pattern/replacement is specified using `variable => value`; these are processed left to right.

There are different methods depending on the type of key in the the `key => value` pairs specified:

* A symbolic variable is replaced by the right-hand side, like `ex(val,:)`, though the latter is more performant
* A symbolic parameter is replaced by the right-hand side, like `ex(:,val)`
* A function is replaced by the corresponding specified function, as the head of the sub-expression
* A sub-expression is replaced by the new expression.
* sub-expression containing a wildcard is replaced by the new expression. The specification of the replacements can use the wildcards. Wildcards expressions may be specified within `SimpleExpressions` or as `Expr` objects.

# Extended help

The first two styles are straightforward.

```@repl replace
julia> using SimpleExpressions

julia> @symbolic x p
(x, p)

julia> ex = cos(x) - x*p
cos(x) + (-1 * x * p)

julia> replace(ex, x => 2) == ex(2, :)
true

julia> replace(ex, p => 2) == ex(:, 2)
true
```

The third, is illustrated by:

```@repl replace
julia> replace(sin(x + sin(x + sin(x))), sin => cos)
cos(x + cos(x + cos(x)))
```

The fourth is similar to the third, only an entire expression (not just its head) is replaced

```@repl replace
julia> ex = cos(x)^2 + cos(x) + 1
(cos(x) ^ 2) + cos(x) + 1

julia> @symbolic u
(u,)

julia> replace(ex, cos(x) => u)
(u ^ 2) + u + 1
```

Replacements occur only if an entire node in the expression tree is matched:

```@repl replace
julia> u = 1 + x
1 + x

julia> replace(u + exp(-u), u => x^2)
1 + x + exp(-1 * (x ^ 2))
```

(As this addition has three terms, `1+x` is not a subtree in the expression tree.)


The fifth needs more explanation, as there can be wildcards in the expression. Wildcards can be specified as `SimpleExpression` expressions or as `Expr` objects.


First, we describe the use of symbolic wildcards.

Wildcards have a naming convention using trailing underscores. One matches a single subexpression or term; two matches one or more subexpressions. In addition, the **special** symbol `⋯` (entered with `\\cdots[tab]` is wild.

```@repl replace
julia> @symbolic x p; @symbolic x_
(x_,)

julia> replace(cos(pi + x^2), cos(pi + x_) => -cos(x_))
-1 * cos(x ^ 2)

```

```@repl replace
julia> ex = log(sin(x)) + tan(sin(x^2))
log(sin(x)) + tan(sin(x ^ 2))

julia> replace(ex, sin(x_) => tan((x_) / 2))
log(tan(x / 2)) + tan(tan((x ^ 2) / 2))

julia> replace(ex, sin(x_) => x_)
log(x) + tan(x ^ 2)

julia> replace(x*p, (x_) * x => x_)
p
```

Pattern and replacements can also be specified with Julia expressions. The basic wildcard is prefaced with `~`, a segment is specified with two `~`.

```@repl replace
julia> ex = log(sin(x)) + tan(sin(x^2))
log(sin(x)) + tan(sin(x ^ 2))

julia> replace(ex, :(sin(~x)) => :(tan(~x/2)))
log(tan(x / 2)) + tan(tan((x ^ 2) / 2))

julia> replace(ex, :(sin((~x)^2)) => :(tan(~x)))
log(sin(x)) + tan(tan(x))
```

Unlike symbolic wildcards, `Expr` objects can have *default slot* (specified as `~!x`) and predicates (specified after `::` to test.

```@repl replace
julia> pat, replacement = :(~!a * (~x)^(~n::(!=(-1)))), :(~a * (~x)^(~n+1)/(~n + 1))
(:(~(!a) * (~x) ^ ~(n::(!=)(-1))), :((~a * (~x) ^ (~n + 1)) / (~n + 1)))

julia> replace(x^2, pat => replacement)
(x ^ 3) / 3

julia> replace(x^(-1), pat => replacement)
1 / x

```



## Picture

The `AbstractTrees` package can print this tree-representation of the expression `ex = sin(x + x*log(x) + cos(x + p + x^2))`:

```
julia> print_tree(ex;maxdepth=10)
sin
└─ +
   ├─ x
   ├─ *
   │  ├─ x
   │  └─ log
   │     └─ x
   └─ cos              <--
      └─ +             ...
         ├─ x          <--
         ├─ p          ...
         └─ ^          ...
            ├─ x       ...
            └─ 2       ...
```

The command wildcard expression `cos(x + ...)` looks at the part of the tree that has `cos` as a node, and the lone child is an expression with node `+` and child `x`. The `⋯` then matches `p + x^2`.


"""
function Base.replace(ex::AbstractSymbolic, args::Pair...)
    for pr in args
        k,v = pr
        ex = _replace(ex, k, ↑(v))
    end
    ex
end
(𝑥::SymbolicVariable)(args::Pair...) = replace(𝑥, args...)
(𝑝::SymbolicParameter)(args::Pair...) = replace(𝑝, args...)
(ex::SymbolicExpression)(args::Pair...) = replace(ex, args...)

(𝑥::SymbolicVariable)(eq::SymbolicEquation) = replace(𝑥, eq.lhs => eq.rhs)
(𝑝::SymbolicParameter)(eq::SymbolicEquation) = replace(𝑝, eq.lhs => eq.rhs)
(ex::SymbolicExpression)(eq::SymbolicEquation) = replace(ex, eq.lhs => eq.rhs)

# For the pattern/replacement pair match expression against pattern. If a match, rewrite replacement using match dictionary.
function Base.replace(ex::AbstractSymbolic, pat_rhs::Pair{S,T}) where {
    S <: Expr,
    T <: Union{Number, Symbol, Expr}}
    pat, rhs = pat_rhs

    ## need to walk the walk
    σ = match(pat, ex)
    if σ == FAIL_DICT
        iscall(ex) || return ex
        args′ = replace.(arguments(ex), pat_rhs)
        return maketerm(AbstractSymbolic, operation(ex), args′, nothing)
    else
        return rewrite(σ, rhs)
    end
    return ex
end


# _replace: basic dispatch in on `u` with (too) many methods
# for shortcuts based on typeof `ex`

## u::SymbolicVariable **including** a wild card

function _replace(ex::SymbolicExpression, u::SymbolicVariable,  v)
    ## intercept wildcards!!!
    ex′, u′, v′ = map(↓, (ex, u, v))
    pred = ==(u′)
    mapping = _ -> v′
    SymbolicExpression(expression_map_matched(pred, mapping, ex′))
end

## u::SymbolicParameter
function _replace(ex::SymbolicExpression, u::SymbolicParameter,  v)
    ex′, u′, v′ = map(↓, (ex, u, v))
    pred = ==(u′)
    mapping = _ -> v′
    SymbolicExpression(expression_map_matched(pred, mapping, ex′))
end


_replace(ex::SymbolicVariable, u::SymbolicVariable, v) =  ex == u ? ↑(v) : ex
_replace(ex::SymbolicParameter, u::SymbolicParameter, v) = ex == u ? ↑(v) : ex


## u::Function (for a head, keeping in mind this is not for SymbolicExpression)
# replace old head with new head in expression
function _replace(ex::AbstractSymbolic, u::𝐹, v) where
    {𝐹 <: Union{Function, SymbolicFunction}}
    _replace_expression_head(ex, u, v)
end

## u::SymbolicExpression, quite possibly having a wildcard

#
# u is symbolic expression possibly wild card
_replace(ex::AbstractSymbolic, u::SymbolicExpression, v) =
    _replace_arguments(ex, u, v)


function _replace(ex::AbstractSymbolic, u::Union{Symbol, Expr}, v)
    iscall(ex) || return (ex == u ? v : ex)

    σ = match(u, ex) # sigma is nothing, (), or a substitution
    if σ != FAIL_DICT
        isempty(σ) && return v # no substitution
        return v(σ...) # XXX <---
    end

    # peel off
    op, args = operation(ex), arguments(ex)
    args′ = _replace_arguments.(args, (u,), (v,))

    return maketerm(ExpressionType, op, args′, nothing)
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
    #iscall(x) || return x
    children = map_matched.(arguments(x), is_match, f)
    maketerm(ExpressionType, operation(x), children, metadata(x))
end

function _ismatch(ex, pred)
    pred(ex) && return true
    iscall(ex) && return any(Base.Fix2(_ismatch, pred), arguments(ex))
    return false
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
function is_wildcard(x::Union{SymbolicVariable, SymbolicParameter})
    𝑥 = string(x)
    endswith(𝑥, "_") || 𝑥 == "⋯"
end
is_wildcard(x::AbstractSymbolic) = false

function _replace_arguments(ex, u, v)
    if _ismatch(u, is_wildcard)
        return replace(ex, convert(Expr, u) => convert(Expr, v))
    else
        return map_matched(ex, ==(u), x -> v)
    end



    iscall(ex) || return (ex == u ? v : ex)

    σ = match(u, ex) # sigma is nothing, (), or a substitution
    if !isnothing(σ)
        σ == () && return v # no substitution
        return v(σ...)
    end

    # peel off
    op, args = operation(ex), arguments(ex)
    args′ = _replace_arguments.(args, (u,), (v,))

    return maketerm(ExpressionType, op, args′, nothing)
end

# _rewrite pattern using dictionary
rewrite(σ::Base.ImmutableDict, rhs::Number) = rhs
rewrite(σ::Base.ImmutableDict, rhs::Symbol) = maketerm(AbstractSymbolic, identity, (rhs,), nothing)
function rewrite(σ::Base.ImmutableDict, rhs::Expr)
    if rhs.head == :call && rhs.args[1] == :(~)
        var_name = varname(rhs.args[2])
        if haskey(σ, var_name)
            return unwrap_const(σ[var_name]) # XXX
        else
            error("No match found for variable $(var_name)") #it should never happen
        end
    end

    # otherwise call recursively on arguments and then reconstruct expression
    op, args... = rhs.args
    args′ = [rewrite(σ, a) for a in rhs.args[2:end]]
    op′ = if isdefined(@__MODULE__, op)
        getproperty(@__MODULE__, op)
    elseif isdefined(Main, op)
        getproperty(Main, op)
    else
        getproperty(Base, op)
    end
    return maketerm(AbstractSymbolic, op′, args′, nothing)
end

rewrite(σ::Base.ImmutableDict, rhs::AbstractSymbolic) = rewrite(σ, convert(Expr, rhs))
