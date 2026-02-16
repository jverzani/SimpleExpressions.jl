# can we solve equations?
# this shows *easy* ones can be solved.

# ---> not sure this is worth having! <----
import CommonSolve
import CommonSolve: solve

# tidier code? \sqsubset[tab]
⊏(ops::Tuple, expr) = any(is_operation(op)(expr) for op in ops)
⊏(op, expr) = is_operation(op)(expr)

# these are applied non-rigorously
inverses = ((sin, asin), (cos, acos), (tan, atan),
            (sec, asec), (csc, acsc), (cot, acot),
            (sind, asind), (cosd, acosd), (tand, atand),
            (exp, log),
            (sqrt, x -> x^2)
            )
inverse_functions = Dict()
for (u,v) ∈ inverses
    inverse_functions[u] = v
    inverse_functions[v] = u
end


## --------
## "Solve" an equation
## very limited!
## currently just moves terms to each side and takes inverse functions
## in a non-rigorous manner
"""
    solve(eq::SymboliclEquation, x)

Very *simple* symbolic equations can be solved with the unexported `solve` method. This example shows a usage.

```{julia}
@symbolic w p; @symbolic h  # two variables, one parameter
import SimpleExpressions: solve, D
constraint = p ~ 2w + 2h
A = w * h

u = solve(constraint, h)
A = A(u) # use equation in replacement
v = solve(D(A, w) ~ 0, w)
```
"""
CommonSolve.solve(eq::SymbolicEquation, x::𝑉) = _solve(eq.lhs, eq.rhs, x)

CommonSolve.init(eq::SymbolicEquation) = throw(ArgumentError("Must specify variable to solve for"))



function _solve(l, r, x::𝑉)
    (contains(l, x) || contains(r, x)) || return nothing

    l, r = _distribute_over_plus(l,x), _distribute_over_plus(r,x)

    l′, r′ = l, r
    # r_to_l move x terms to left
    # l_to_r move non-x terms to right
    # also applies inverse functions *non-rigorously* as possible
    if contains(r, x)
        l,r = isolate_x(Val(:←), l, r, x)
    end
    if contains(l, x)
        l, r = isolate_x(Val(:→), l, r, x)
    else
        l, r = zero(l), r - l
    end
    l == l′  && return _final_solve(l, r, x)
    _solve(l, r, x) # recurse
end

# add in any tricks here
function _final_solve(l::𝑋,r,x::𝑋) where {𝑋 <: 𝑉}
    l ~ _combine_numbers(r)
end

function _final_solve(l,r,x)
    # try some things
    ## polynomials?
    cs = coefficients(l,x)
    if !isnothing(cs)
        if length(cs) == 1
            return l ~ r
        elseif length(cs) == 2
            a0,a1 = cs
            return x ~ _combine_numbers((r - a0)  / a1)
        end
        p = sum(aᵢ * x^i for (i, aᵢ) ∈ enumerate(Iterators.rest(cs,2)))
        # could solve, but ...
        return p ~ _combine_numbers(r - first(cs))
    end
    l ~ _combine_numbers(r)
end


## _distribute_over_plus out to + terms
_distribute_over_plus(ex::Number, x; __cnt=1) = error(ex)
_distribute_over_plus(ex::𝐿, x) = ex
function _distribute_over_plus(ex, x; __cnt=1)
    contains(ex, x) || return ex
    __cnt > 50 && return ex
    ex′ = _distribute_over_plus(operation(ex), ex, x)
    ex′ != ex && return _distribute_over_plus(ex′, x; __cnt= __cnt + 1)
    ex′
end

# a*(b+c) --> a*b + a*c (flatten?)
# work of distribute_over_plus is op by op
function _distribute_over_plus(::typeof(+), ex, x)
    reduce(+, _distribute_over_plus.(sorted_arguments(ex), x), init=zero(x))
end

function _distribute_over_plus(::typeof(*), ex, x)
    a = one(x)
    b = nothing
    for c ∈ arguments(ex)
        if isnothing(b) & is_operation(+)(c)
            b = c
            continue
        else
            a = a * _distribute_over_plus(c, x)
        end
    end
    isnothing(b) && return a
    return mapreduce(Base.Fix1(*, a), +, sorted_arguments(b), init=zero(x))
end

function _distribute_over_plus(::typeof(-), ex, x)
    reduce(-, _distribute_over_plus.(arguments(ex), x), init=zero(x))
end


function _distribute_over_plus(::typeof(/), ex, x)
    a, b = arguments(ex)
    contains(b, x) && return ex
    a * (1 / b)
end

function _distribute_over_plus(::typeof(^), ex, x)
    a, b = arguments(ex)
    a == x && return ex
    𝑥, 𝑝 = free_symbolx(b)
    if isempty(𝑥) && isempty(𝑥)
        !isinteger(b) && return ex
        n = b()
        n < 0 && return ex
        l = one(x)
        for i in 1:n
            l = l * a
        end
        return l
    end
    ex
end
_distribute_over_plus(::Any, ex, x) = ex # nothing to do?



## clean up constants by sorting arguments to +, &
_combine_numbers(ex::𝐿) = ex
_combine_numbers(ex) = _combine_numbers(operation(ex), ex)

function _combine_numbers(::typeof(+), ex)
    args = _combine_numbers.(sorted_arguments(ex))
    foldl(+, args, init=zero(ex))
end

function _combine_numbers(::typeof(*), ex)
    args = _combine_numbers.(sorted_arguments(ex))
    foldl(*, args, init=one(ex))
end

function _combine_numbers(::Any, ex)
    args = _combine_numbers.(arguments(ex))
    maketerm(typeof(ex), operation(ex), args, nothing)
end

## ---- isolate_x x to lhs

function isolate_x(::Val{:→}, l::𝑉, r, x)
    l, r
end

function isolate_x(::Val{:←}, l, r::𝑉, x)
    if r == x
        l = l / r
        r = one(x)
    end
    l, r
end

isolate_x(v::Val{:→}, l::SymbolicExpression, r, x) = isolate_x(v, operation(l), l, r, x)
isolate_x(v::Val{:←}, l, r::SymbolicExpression, x) = isolate_x(v, operation(r), l, r, x)

## ---- /

function isolate_x(::Val{:→}, ::typeof(/), l, r, x)
    a, b, = arguments(l)
    l′ = one(l)
    if contains(a, x)
        l′ = a
    else
        r = r / a
    end
    if contains(b, x)
        if !contains(l′, x)  # take reciprocal
            l′ = b / l′
            r = one(x) / r
        else
            l′ = l′ / b
        end
    else
        r = r * b
    end

    l′, r
end

function isolate_x(::Val{:←}, ::typeof(/), l, r, x)
    a, b, = arguments(r)
    r′ = one(r)
    if contains(a, x)
        l = l / a
    else
        r′ = a
    end

    if contains(b, x)
        l = l * b
    else
        r′ = r′ / b
    end

    l, r′
end

## ---- -

function isolate_x(::Val{:→}, ::typeof(-), l, r, x)
    a, b, = arguments(l)
    l′ = zero(l)
    if !contains(a, x)
        r = r - a
    else
        l′ = a
    end

    if !contains(b, x)
        r = r + b
    else
        l′ = l′ - b
    end

    l, r′
end

function isolate_x(::Val{:←}, ::typeof(-), l, r, x)
    a, b, = arguments(r)
    r′ = zero(r)
    if contains(a, x)
        l = l - a
    else
        r′ = a
    end

    if contains(b, x)
        l = l + b
    else
        r′ = r′ - b
    end

    l, r′
end

## ----- ^

function isolate_x(::Val{:→}, ::typeof(^), l, r, x)
    a, b = arguments(l)
    if !contains(b, x)
        if !isvariable(b)
            bb = b()
            bb == 0 && return one(x), r
            bb == 1 && return a, r
            bb == 2 && return a, sqrt(r)
            bb == 3 && return a, cbrt(r)
        end
        l,r = a, r^(one(x) / b)
    end
    return l, r
end

function isolate_x(::Val{:←}, ::typeof(^), l, r, x)
    a, b, = arguments(r)

    if !contains(b, x)
        if !isvariable(b)
            bb = b()
            bb == 0 && return l, one(x)
            bb == 1 && return l, a
            bb == 2 && return sqrt(l), a
            bb == 3 && return cbrt(l), a
        end
        l,r = l^(one(x)/b), a
    end
    return l, r
end

## ----- +

function isolate_x(::Val{:→}, ::typeof(+), l, r, x)
    l′ = zero(l)
    for c ∈ arguments(l)
        if contains(c, x)
            l′ = l′ + c
        else
            r = r - c
        end
    end
    return l′, r
end


function isolate_x(::Val{:←}, ::typeof(+), l, r, x)
    r′ = zero(r)
    for c ∈ arguments(r)
        if contains(c, x)
            l = l - c
        else
            r′ = r′ + c
        end
    end
    l, r′
end

## ----- *

function isolate_x(::Val{:→}, ::typeof(*), l, r, x)
    l′ = one(l)
    for c ∈ arguments(l)
        if contains(c, x)
            l′ = l′ * c
        else
            r = r / c
        end
    end
    l′, r
end

function isolate_x(::Val{:←}, ::typeof(*), l, r, x)
    r′ = one(r)
    for c ∈ arguments(r)
        if contains(c, x)
            l = l / c
        else
            r′ = r′ * c
        end
    end
    l, r′
end

## ---- inverse
# apply inverse?
function isolate_x(::Val{:→}, ::Any, l, r, x)
    op = operation(l)
    op⁻¹ = get(inverse_functions, op, nothing)

    if !isnothing(op⁻¹)
        l = only(arguments(l))
        r = op⁻¹(r)
    end

    return l, r
end

function isolate_x(::Val{:←}, ::Any, l, r, x)
    !contains(r, x) && return l, r  # leave as is if no x

    op = operation(r)
    op⁻¹ = get(inverse_functions, op, nothing)

    if !isnothing(op⁻¹)
        r = only(arguments(r))
        l = op⁻¹(l)
    end

    return l, r
end


## l to r: leave x terms, move others








# apply inverse?
