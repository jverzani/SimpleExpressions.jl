# implementation specific definitions needed for matching in matchpy
const ExpressionType = SymbolicExpression

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

## ----


"""
    replace(ex::SymbolicExpression, args::Pair...)

Replace parts of the expression with something else.

Returns a symbolic object.

The replacement is specified using `variable => value`; these are processed left to right.

There are different methods depending on the type of key in the the `key => value` pairs specified:

* A symbolic variable is replaced by the right-hand side, like `ex(val,:)`, though the latter is more performant
* A symbolic parameter is replaced by the right-hand side, like `ex(:,val)`
* A function is replaced by the corresponding specified function, as the head of the sub-expression
* A sub-expression is replaced by the new expression.
* A sub-expression containing a wildcard is replaced by the new expression, possibly containing a wildcard, in which the arguments are called.


The first two are straightforward.

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


The fifth needs more explanation, as there can be wildcards in the expression.

Wildcards have a naming convention using trailing underscores. One matches one value; two matches one or more values; three match 0, 1, or more values. In addition, the **special** symbol `⋯` (entered with `\\cdots[tab]` is wild.

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

"""
    match(pattern, expression)

Match expression using a pattern with possible wildcards. Uses a partial implementation of *Non-linear Associative-Commutative Many-to-One Pattern Matching with Sequence Variables* by Manuel Krebber.

If there is no match: returns `nothing`.

If there is a match: returns a collection of substitutions (σ₁, σ₂, …) -- possibly empty -- with the property `pattern(σ...) == expression` is true.

Wildcards are just symbolic variables with a naming convention: use one trailing underscore to indicate a single match, two trailing underscores for a match of one or more, and three trailing underscores for a match on 0, 1, or more.

## Examples

```@repl
julia> using SimpleExpressions

julia> SimpleExpressions.@symbolic_variables a b x_ x__ x___
(a, b, x_, x__, x___)

julia> p, s= x_*cos(x__), a*cos(2 + b)
(x_ * cos(x__), a * cos(2 + b))

julia> Θ = match(p, s)
((x__ => 2 + b, x_ => a),)

julia> σ = only(Θ)
(x__ => 2 + b, x_ => a)

julia> p(σ...) == s
true

julia> p, s =  p = x_ + x__ + x___,  a + b + a + b + a
(x_ + x__ + x___, a + b + a + b + a)

julia> Θ = match(p, s);

julia> length(Θ)   # 37 matches
37

julia> σ = last(Θ)
(x_ => b, x__ => b, x___ => a + a + a)

julia> p(σ...) # a + a + (a + b + b)
b + b + (a + a + a)
```

"""
function Base.match(pat::AbstractSymbolic, ex::AbstractSymbolic)
    pred(a) = any(any(_is_𝑋(u) for u in s) for s in free_symbols(a))
    if pred(pat)
        out = MatchOneToOne((ex,), pat)
        out == () && return nothing
        return out
    else
        out = SyntacticMatch(ex, pat)
    end
    out
end
