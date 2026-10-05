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


function (ex::SymbolicCall)(x)
    _call(ex, operation(ex), x)
end

function (ex::SymbolicCall)(x,p)
    _call(ex, operation(ex), x, p)
end

# these **assume** no more than one SymbolicVariable or SymbolicParameter
# are in expression. See `replace` for more general substitution
# substitute for 𝑥
function _substitutex(u, x)
    pred = x -> isa(x, StaticVariable)
    mapping = _ -> DynamicConstant(x)
    expression_map_matched(pred, mapping, u)
end

# substitute for 𝑝
function _substitutep(u, p)
    pred = p -> isa(p, DynamicVariable)
    mapping = _ -> DynamicConstant(p)
    expression_map_matched(pred, mapping, u)
end

function _call(ex, ::Any, x::T) where T
    # allocates less than creating u((𝑥=x,))
    u = ↓(ex)
    u₁ = _substitutex(u, x)
    return u₁(NamedTuple{}())
end

function _call(ex, ::Any, x::T, p::S) where {T,S}
    # allocates less than creating u((𝑥=x,𝑝=p))
    u = ↓(ex)
    u₁ = _substitutex(u, x)
    u₂ = _substitutep(u₁, p)
    return u₂(NamedTuple{}())
end


function _call(ex, ::typeof(Base.broadcasted), x::T) where T
    Base.materialize(_call(ex, nothing, x))
end

function _call(ex, ::typeof(Base.broadcasted),  x::T, p::S) where {T,S}
    Base.materialize(_call(ex, nothing, x, p))
end

# directly call with kwargs.
# direct call can be quite more performant but requires
# specification of the variable/parameter name in the call.
(𝑥::SymbolicVariable)(;kwargs...) = (↓(𝑥))(NamedTuple(kwargs))
(𝑝::SymbolicParameter)(;kwargs...) = (↓(𝑝))(NamedTuple(kwargs))
## This also handles case of symbolic expressions which are numeric
## have value given by ex()
(ex::SymbolicCall)(;kwargs...) = (↓(ex))(NamedTuple(kwargs))

# like Symbolics
unwrap_const(x::Any) = x
function unwrap_const(x::Symbol)
    if hasproperty(Base.MathConstants, x)
        return getproperty(Base.MathConstants, x)
    end
    x == :twoπ && return 2π
    x == :fourπ && return      4π
    x == :halfπ && return      π / 2
    x == :quartπ && return     π / 4
    x == :invπ && return       1 / π
    x == :twoinvπ && return    2 / π
    x == :fourinvπ && return   4 / π
    x == :inv2π && return      1 / (2π)
    x == :inv4π && return      1 / (4π)
    x == :sqrt2 && return      √2
    x == :sqrt3 && return      √3
    x == :sqrtπ && return      √π
    x == :sqrt2π && return     √2π
    x == :sqrt4π && return     √4π
    x == :sqrthalfπ && return  √(π / 2)
    x == :invsqrt2 && return   1 / √2
    x == :invsqrtπ && return   1 / √π
    x == :invsqrt2π && return  1 / √2π
    x == :loghalf && return    log(1 / 2)
    x == :logtwo && return     log(2)
    x == :logten && return     log(10)
    x == :logπ && return       log(π)
    x == :log2π && return      log(2π)
    x == :log4π       # log(4π)
    return x
end
unwrap_const(x::SymbolicNumber) = x()
function unwrap_const(x::SymbolicCall)
    is_number(x) && return x()
    return x
end


## --- substitution ---
## Substitution leaves as a symbolic value
##
## To substitute use one of nothing && return missing or `:` in either the x or p
 position
## * `u(x, :)` substitute for `x, leaves expression with parameter
## * `u(:, p)` substitute for `p`, leaves expression with variable
## The result can be evaluated

const MISSING = Union{Nothing, Missing, typeof(:)}

## we have substitution (using :) or evaluate
(𝑥::SymbolicVariable)(::MISSING, p) = 𝑥
(𝑥::SymbolicVariable)(x, ::MISSING) = ↑(x)
(𝑥::SymbolicVariable)(::MISSING, ::MISSING) = 𝑥

(𝑝::SymbolicParameter)(::MISSING, p) = ↑(p)
(𝑝::SymbolicParameter)(x,::MISSING) = 𝑝
(𝑝::SymbolicParameter)(::MISSING,::MISSING) = 𝑝

function (ex::SymbolicCall)(::MISSING, p)
    u = ↓(ex)
    u₁ = _substitutep(u, p)
    SymbolicExpression(u₁)
end
function (ex::SymbolicCall)(x,::MISSING)
    u = ↓(ex)
    u₁ = _substitutex(u, x)
    SymbolicExpression(u₁)
end

(ex::SymbolicCall)(::MISSING, ::MISSING) = ex

(X::SymbolicEquation)(::MISSING,p) = tilde(X.lhs(:, p),  X.rhs(:, p))
(X::SymbolicEquation)(x,::MISSING) = tilde(X.lhs(x, :),  X.rhs(x, :))
(X::SymbolicEquation)(::MISSING,::MISSING) = X
