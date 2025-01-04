## ---- call

## Evaluate or substitute 
##
## We can either evaluate (to return a number)
## or substitute (returning a symbolic value)
##
## Evaluation can be achieved by specifying
##
## * `u(x)` evaluates the expression with the variable having the
##    value of `x`. If there is a parameter in the `u` expression this
##    will error
## * `u(x,p)`  evaluates the expression with the variable having the
##    value of `x` and the parameter having the variable `p`.
##    If there is no parameter, the value of `p` is ignored
## * `u(*, p)` evaluates the expression with the parameter having the
##    variable `p`. If the expression has a variable, this will error.
##    If the expression has just a parameter any value for the first
##    argument besides `nothing`, `missing` or `:` can be passed,
##    `*` is just visually appealing and is always defined
## * `u()` if after substitution the expression has no free symbols,
##    this will evaluate it. This works with SymbolicNumbers also

## evaluation
(𝑥::SymbolicVariable)(x) = x
(𝑥::SymbolicVariable)(x,p) = x

(𝑝::SymbolicParameter)(x) = 𝑝
(𝑝::SymbolicParameter)(x,p) = p

(𝑐::SymbolicNumber)(args...; kwargs...) = CallableExpressions.constant_value(↓(𝑐))


function (ex::SymbolicExpression)(x)
    𝑥,𝑝 = 𝑥𝑝!(ex)
    _call(ex, operation(ex), (𝑥,), x)
end

function (ex::SymbolicExpression)(x,p)
    𝑥,𝑝 = 𝑥𝑝!(ex)
    _call(ex, operation(ex), (𝑥,𝑝), x, p)
end




_call(ex, ::Any, 𝑥, x) =  (↓(ex))(NamedTuple{𝑥}((x,)))
_call(ex, ::Any, 𝑥𝑝, x, p) =  (↓(ex))(NamedTuple{𝑥𝑝}((x,p)))

function _call(ex, ::typeof(Base.broadcasted), 𝑥, x)
    (↓(ex))(NamedTuple{𝑥}((x,))) |> Base.materialize
end

function _call(ex, ::typeof(Base.broadcasted), 𝑥𝑝, x, p)
    (↓(ex))(NamedTuple{tuple(𝑥𝑝...)}((x,p)))  |> Base.materialize
end

# directly call with kwargs.
# direct call can be quite more performant but requires
# specification of the variable/parameter name in the call.
(𝑥::SymbolicVariable)(;kwargs...) = (↓(𝑥))(NamedTuple(kwargs))
(𝑝::SymbolicParameter)(;kwargs...) = (↓(𝑝))(NamedTuple(kwargs))
## This handles case of symbolic expressions which are numeric
## have value given by ex()
(ex::SymbolicExpression)(;kwargs...) = (↓(ex))(NamedTuple(kwargs))



## --- substitution ---
## Substitution leaves as a symbolic value
##
## To substitute use one of nothing, missing or `:` in either the x or p
## position
## * `u(x, :)` substitute for `x, leaves expression with parameter
## * `u(:, p)` substitute for `p`, leaves expression with variable
## The result can be evaluated 

const MISSING = Union{Nothing, Missing, typeof(:)}

## we have substitution (using :) or evaluate
(𝑥::SymbolicVariable)(::MISSING, p) = 𝑥
(𝑥::SymbolicVariable)(x, ::MISSING) = ↑(x)
(𝑥::SymbolicVariable)(::Missing, ::MISSING) = 𝑥

(𝑝::SymbolicParameter)(::MISSING, p) = ↑(p)
(𝑝::SymbolicParameter)(x,::MISSING) = 𝑝
(𝑝::SymbolicParameter)(::Missing,::MISSING) = 𝑝

(ex::SymbolicExpression)(::MISSING, p) = substitutep(ex, p)
(ex::SymbolicExpression)(x,::MISSING) = substitutex(ex, x)
(ex::SymbolicExpression)(::MISSING, ::Missing) = ex

(X::SymbolicEquation)(::MISSING,p) = tilde(X.lhs(:, p),  X.rhs(:, p))
(X::SymbolicEquation)(x,::MISSING) = tilde(X.lhs(x, :),  X.rhs(x, :))
(X::SymbolicEquation)(::Missing,::MISSING) = X


# these **assume** no more than one SymbolicVariable or SymbolicParameter
# are in expression. See `replace` for more general substitution
# substitute for x
function substitutex(ex, x)
    pred = x -> isa(x, StaticVariable)
    mapping = _ -> DynamicConstant(x)
    SymbolicExpression(expression_map_matched(pred, mapping, ↓(ex)))
end

# substitute for p
function substitutep(ex, p)
    pred = p -> isa(p, DynamicVariable)
    mapping = _ -> DynamicConstant(p)
    SymbolicExpression(expression_map_matched(pred, mapping, ↓(ex)))
end



