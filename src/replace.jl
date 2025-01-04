"""
    replace(ex::SymbolicExpression, args::Pair...)

Replace parts of the expression with something else.

Returns a symbolic object.

The replacement is specified using `variable => value`; these are processed left to right.

There are different methods depending on the type of key in the the `key => value` pairs specified:

* A symbolic variable is replaced by the right-hand side, like `ex(val,:)`
* A symbolic parameter is replaced by the right-hand side, like `ex(:,val)`
* A function is replaced by the corresponding specified function, as the head of the sub-expression
* A sub-expression is replaced by the new expression.
* A sub-expression containing a wildcard is replaced by the new expression, possibly containing a wildcard, in which the arguments are called.


The first two are straightforward.

```julia
julia> ex = cos(x) - x*p
cos(x) - (x * p)

julia> replace(ex, x => 2) == ex(2, :)
true

julia> replace(ex, p => 2) == ex(:, 2)
true
```

The third, is illustrated by:

```julia
julia> replace(x + sin(x), sin => cos)
x + cos(x)

```

The fourth is similar to the third, only an entire expression (not just its head) is replaced

```{julia}
julia> ex = cos(x)^2 + cos(x) + 1
(cos(x) ^ 2) + cos(x) + 1

julia> @symbolic u
(u,)

julia> replace(ex, cos(x) => u)
(u ^ 2) + u + 1
```

Replacements occur only if an entire node in the expression tree is matched:

```julia
julia> u = 1 + x
1 + x

julia> replace(u + exp(-u), u => x)
1 + x + exp(-1 * x)
```

(As this addition has three terms, `1+x` is not a subtree in the expression tree.)


The fifth needs more explanation, as there can be wildcards in the expression.

The symbolic variable `⋯` (created with `@symbolic ⋯`, where `⋯` is formed by `\\cdots[tab]`) can be used as a wild card that matches the remainder of an expression tree. The replacement value can have `⋯` as a variable, in which case the identified values will be substituted.

```julia
julia> @symbolic x p; @symbolic ⋯
(⋯,)

julia> replace(cos(pi + x^2), cos(pi + ⋯) => -cos(⋯))
-1 * cos(x^2)
```

```julia
julia> ex = log(sin(x)) + tan(sin(x^2))
log(sin(x)) + tan(sin(x ^ 2))

julia> replace(ex, sin(⋯) => tan((⋯) / 2))
log(tan(x / 2)) + tan(tan(x ^ 2 / 2))

julia> replace(ex, sin(⋯) => ⋯)
log(x) + tan(x ^ 2)

julia> replace(x*p, (⋯) * x => ⋯)
p

```

(The wrapping of `(⋯)` in the last example is needed as the symbol parses as an infix operator.)

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
        ex = _replace(ex, k, v)
    end
    ex
end
(𝑥::SymbolicVariable)(args::Pair...) = replace(𝑥, args...)
(𝑝::SymbolicParameter)(args::Pair...) = replace(𝑝, args...)
(ex::SymbolicExpression)(args::Pair...) = replace(ex, args...)

(𝑥::SymbolicVariable)(eq::SymbolicEquation) = replace(𝑥, eq.lhs => eq.rhs)
(𝑝::SymbolicParameter)(eq::SymbolicEquation) = replace(𝑝, eq.lhs => eq.rhs)
(ex::SymbolicExpression)(eq::SymbolicEquation) = replace(ex, eq.lhs => eq.rhs)



# _replace: basic dispatch in on `u` with (too) many methods
# for shortcuts based on typeof `ex`

## u::SymbolicVariable

function _replace(ex::SymbolicExpression, u::SymbolicVariable,  v)
    pred = ==(↓(u))
    mapping = _ -> ↓(v)
    ex = SymbolicExpression(expression_map_matched(pred, mapping, ↓(ex)))
end

## u::SymbolicParameter
function _replace(ex::SymbolicExpression, u::SymbolicParameter,  v)
    pred = ==(↓(u))
    mapping = _ -> ↓(v)
    ex = SymbolicExpression(expression_map_matched(pred, mapping, ↓(ex)))
end


_replace(ex::SymbolicVariable, u::SymbolicVariable, v) =  ex == u ? ↑(v) : ex
_replace(ex::SymbolicParameter, u::SymbolicParameter, v) = ex == u ? ↑(v) : ex


## u::Function (for a head, keeping in mind this is not for SymbolicExpression)

# replace old head with new head in expression
_replace(ex::SymbolicNumber, u::Function,  v) = ex
_replace(ex::SymbolicParameter, u::Function,  v) = ex
_replace(ex::SymbolicVariable, u::Function,  v) = ex

function _replace(ex::SymbolicExpression, u::Function, v)
    op, args = operation(ex), children(ex)
    if op == u
        op = v
    end

    args′ = (_replace(a, u, v) for a ∈ args)

    ex = maketerm(SymbolicExpression,op, args′, nothing)
end

## u::SymbolicExpression, quite possibly having a wildcard

## We use ⋯ (`\\cdots[tab]`) for a single wildcard that should
## * take up remaining terms in `+` or `*` expressions
## * represent branches of an expression tree.
const WILD = SymbolicVariable(:(⋯))

has_WILD(ex::AbstractSymbolic) = has_WILD(↓(ex)) # a bit faster to work lower level
has_WILD(ex::Any) = false
has_WILD(ex::typeof(↓(WILD))) = true
function has_WILD(ex::StaticExpression)
    for a ∈ ex.children
        has_WILD(a) && return true
    end
    false
end


# u is symbolic expression possibly wild card
_replace(ex::SymbolicNumber,    u::SymbolicExpression, v) = ex
_replace(ex::SymbolicParameter, u::SymbolicExpression, v) = ex
_replace(ex::SymbolicVariable,  u::SymbolicExpression, v) = ex

function _replace(ex::SymbolicExpression, u::SymbolicExpression, v)
    if !has_WILD(u)
        # no wildcard so we must match expression tree completely
        return _exact_replace(ex, u, v)
    end
    ## ⋯ There is a *wild* card for an expression match
    m = match(u, ex)
    !isnothing(m) && return has_WILD(v) ? _replace(v, WILD, m) : ↑(v)

    # peel off
    op, args = operation(ex), children(ex)
    args′ = _replace.(args, (u,), (v,))

    return maketerm(AbstractSymbolic, op, args′, nothing)

end

# return arguments fill out ⋯ or nothing if not a
# match in the expression tree
# this seems like the correct use of the generic
function Base.match(pat::AbstractSymbolic, ex::AbstractSymbolic)
    has_WILD(pat) || return (pat == ex ? ex : nothing)
    m = _ismatch(ex, pat)
    return m
end

# ismatch wildcard
# return hasmatch: this matches or contains a match
# and expression/missing expression if a match, nothing if not
_ismatch(ex::AbstractSymbolic, u::SymbolicVariable) = ex == u ? u : nothing
_ismatch(ex::AbstractSymbolic, u::typeof(WILD)) = ex

_ismatch(ex::SymbolicNumber, u::SymbolicExpression) = nothing
_ismatch(ex::SymbolicVariable, u::SymbolicExpression) = nothing
_ismatch(ex::SymbolicParameter, u::SymbolicExpression) = nothing

function _ismatch(ex::SymbolicExpression, u::SymbolicExpression)
    opₓ, opᵤ = operation(ex), operation(u)
    opₓ == opᵤ || return nothing
    argsₓ, argsᵤ = children(ex), children(u)
    if opₓ == (+) || opₓ == (*)
        asₓ, asᵤ = sort(collect(argsₓ)), sort(collect(argsᵤ))
        if WILD ∈ asᵤ
            for a ∈ asᵤ
                a == WILD && continue
                a ∈ asₓ || return nothing
            end
            ex′ = maketerm(AbstractSymbolic, opₓ, _diff!(asₓ, asᵤ), nothing)
            return ex′
        else
            length(asₓ) == length(asᵤ) || return nothing
            for (a,b) ∈ zip(asₓ, asᵤ)
                a == b && continue
                (!has_WILD(b) && a != b) && return nothing
                matched, m = _ismatch(a, b)
                matched && !isnothing(m) && return m
                matched || return nothing
            end
        end
    else
        for (a,b) ∈ zip(argsₓ, argsᵤ)
            if !(has_WILD(b))
                a == b || return nothing
            end
        end
        for (a,b) ∈ zip(argsₓ, argsᵤ)
            m = _ismatch(a, b)
            return m
        end
    end
    @show :shouldnt_be_here, ex, u
    return missing
end

# remove elements in xs′ that appear in xs but only once!
function _diff!(xs, xs′)
    for i in eachindex(xs′)
        i = only(indexin(xs′[i:i], xs))
        !isnothing(i) && deleteat!(xs, i)
    end
    xs
end

## replace exact piece of tree with something else
_exact_replace(ex::SymbolicNumber, p, q) = ex == p ? ↑(q) : ex
_exact_replace(ex::SymbolicVariable, p, q) = ex == p ? ↑(q) : ex
_exact_replace(ex::SymbolicParameter, p, q) = ex == p ? ↑(q) : ex
function _exact_replace(ex::SymbolicExpression, p, q)
    ex == p && return ↑(q)
    op, args = operation(ex), children(ex)
    args′ = ((a == p ? q : _exact_replace(a, p, q)) for a in args)
    maketerm(SymbolicExpression, op, args′, nothing)
end
