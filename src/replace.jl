# implementation specific definitions needed for matching in matchpy

const ExpressionType = SymbolicExpression

## ---- match, replace
"""
    match(pattern::Expr, subject::AbstractSymbolic)::Union{MatchDict, Nothing}

Match `subject` against a `pattern` given as a Julia expression containing wildcards.

Return a dictionary mapping wildcard names (as symbols) to the matched values for the first match found, or `nothing` if there is none. Use `eachmatch` to get all identified matches.

## Examples
```julia
julia> @symbolic x p
(x, p)

julia> match(:(~x * cos(~y)), p*cos(x))
Base.ImmutableDict{Symbol, SimpleExpressions.AbstractSymbolic} with 2 entries:
  :y => x
  :x => p
```

# Extended help

## The algorithm

The basic algorithm comes from that of `rule2.jl` from `SymbolicIntegration` and `[Krebber](https://arxiv.org/pdf/1705.00907)`.

The pattern and the subject are walked together, top down.

* A literal in the pattern (a number, a symbol, a constant such as `ℯ`) matches only an equal value in the subject.

* A wildcard is bound to the part of the subject it is compared with. A wildcard that appears more than once must be bound to equal values each time, so `:(~a + ~a)` matches `x + x` but not `x + y`.

* A call in the pattern, such as `cos(~y)`, matches a call in the subject with the same operation, whose arguments are then matched in turn. The pattern's operation can itself be a wildcard, see `(~f)(~x)` below.

* For a `+` or `*` pattern (which are commutative) the arguments of the subject may be matched in any order, so `:(~x + 2)` matches `2 + y`. Different assignments can give different matches, which is why there can be more than one; `match` returns the first and `eachmatch` returns them all.

* Powers are matched up to their representation: for example `sqrt(x)` and `x^(1//2)` can match each other, and a pattern `~a / ~b` matches `x * (1 / y)`.

* Matching proceeds through a list of candidate bindings. A binding that conflicts with one already made, or that fails a predicate, is discarded; if no candidates remain the match fails.

## Wildcards

A wildcard is written with a leading `~`. A bare variable in the pattern is *not* a wildcard: it must match literally.

| Pattern            | Name                | Matches                                                      |
|:-------------------|:--------------------|:-------------------------------------------------------------|
| `~x`               | slot                | exactly one subexpression, bound to `x`                      |
| `~x::pred`         | slot with predicate | one subexpression for which `pred(value)` is `true`          |
| `~!x`              | default slot        | one subexpression, or a default value when absent|
| `~~x`              | segment/plus        | zero or more arguments of a call |
| `~~~x`             | star                | one or more arguments of a call |
| `(~f)(~x)`         | operation wildcard  | any call; `f` is bound to the operation                      |

### Slots and predicates

`~x` matches one subexpression. A predicate restricts the match; it is any function (or expression naming one) that returns a `Bool`, called on the value to be bound:

```julia
julia> match(:(~a::iseven * ~b), 2x)    # :a => 2, :b => x
julia> match(:(~a::iseven * ~b), 3x)    # nothing
```

An error thrown by a predicate counts as `false`.

### Default slots

`~!x` is a slot that may be absent. In a sum its default is `0`, in a product, a
power, or a division it is `1`. Thus `:(~!a * ~b)` matches `x` with `a => 1` and `b => x`, and `:((~b)^(~!n))` matches `x` with `n => 1`. If the term is present it is bound as usual. When the same default slot appears several times in a pattern, all occurrences must agree. Within a replacement, a default slot is written `~a` once bound.

### Segments

`~~x` and `~~~x` stand for several arguments of a call, and are bound to a *tuple*
of those arguments. `~~x` allows none, `~~~x` requires at least one:

```julia
julia> match(:(~x + ~~~y), x + y + z)   # :x => x, :y => (y, z)
julia> match(:(~x + ~~~y), x)           # nothing
```

If a segment is the only argument of the call, it is bound to all the arguments,
so `:(*(~~a))` matches `(x + y) * z` with `a => (x + y, z)`. When there are several segments in one call the remaining arguments are divided among them, each `~~~` segment receiving at least one; several divisions may be possible, but not all are enumerated.

### Operation wildcards

`(~f)(~x)` matches any call with one argument, binding `f` to the operation and `x`
to the argument:

```julia
julia> match(:((~f)(~x)), sin(y))       # :f => sin, x => y
```

## Caveats

* Matching is syntactic up to commutativity and the power equivalences above. It does not use other algebraic identities, so `:(~x * ~x)` does not match `x^2`.

* Matching does not consider associativity. For example, `:(~x + ~y)` does not match `a + b + c`, even though it could be argued to match `(a+b) + c` or `a + (b + c)`. That matching is to expensive. The `AssociativeCommutativePatternMatching` implements an algorithm that does this matching.

* Pattern constants are compared by value after unwrapping, so `2` matches the symbolic number `2` (and `2.0`).

!!! note
    Extended help initially drafted by co-pilot
"""
function Base.match(pattern::Expr, subject::AbstractSymbolic)
    σs = eachmatch(pattern, subject)
    isempty(σs) && return nothing
    first(σs)
end

function Base.match(pat::AbstractSymbolic, ex::AbstractSymbolic)
    return match(convert(Expr, pat), ex)
end
function Base.eachmatch(pattern::Expr, subject::AbstractSymbolic)
    σs = [MatchDict()]
    check_expr_r(subject, pattern, σs)
end

Base.eachmatch(pattern::AbstractSymbolic, subject::AbstractSymbolic) =
    eachmatch(convert(Expr, pattern), subject)



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
    if σ == nothing #FAIL_DICT
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
rewrite(σ::Base.ImmutableDict, rhs::Function) = rhs
function rewrite(σ::Base.ImmutableDict, rhs::Symbol)
    maketerm(AbstractSymbolic, identity, (rhs,), nothing)
end
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
