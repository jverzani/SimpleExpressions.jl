# can we solve equations?
# this shows *easy* ones can be solved.

# ---> not sure this is worth having! <----
import CommonSolve
import CommonSolve: solve

𝑉 = Union{SymbolicVariable, SymbolicParameter}
𝐿 = Union{𝑉, SymbolicNumber}

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
    l, r = _expand(l,x), _expand(r,x)
    l′, r′ = l, r
    # r_to_l move x terms to left
    # l_to_r move non-x terms to right
    # also applies inverse functions *non-rigorously* as possible
    if contains(r, x)
        l,r = r_to_l(l,r,x)
    end
    if contains(l, x)
        l, r = l_to_r(l,r,x)
    else
        l, r = zero(l), r ⊖ l
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
        if length(cs) == 2
            a0,a1 = cs
            return x ~ _combine_numbers((r ⊖ a0)  ⨸ a1)
        end
        p = sum(aᵢ * x^i for (i, aᵢ) ∈ enumerate(Iterators.rest(cs,2)))
        # could solve, but ...
        return p ~ _combine_numbers(r ⊖ first(cs))
    end
    l ~ _combine_numbers(r)
end


# polynomial in x?
function _ispolynomial(ex, x)
    !contains(ex, x) && return true
    x == ex && return true
    (+,-,*,/,^) ⊏ ex || return false
    for c ∈ children(ex)
        out = _ispolynomial(c, x)
        out || return false
        if is_operation(^)(c)
            a, b = children(c)
            isconstant(b) || return false
            #contains(a, x) || false
            𝑥, 𝑝 = free_symbols(b)
            (!isempty(𝑥) || !isempty(𝑝)) && return false
            bb = b()
            (isinteger(bb) && bb >= 0) || return false
        elseif is_operation(/)(c)
            a,b = children(c)
            contains(b, x) && return false
        end
    end
    return true
end


"""
    coefficients(ex, x)

If expression or equation is a polynomial in `x`, return the coefficients. Otherwise return `nothing`.

## Example

```
julia> @symbolic x p;

julia> eq = x*(x+2)*(x-p) ~ 2;

julia> a0, as... = cs = SimpleExpressions.coefficients(eq, x)
(a₀ = -2, a₁ = -2 * p, a₂ = 2 + (-1 * p), a₃ = 1)

julia> a0 + sum(aᵢ*x^i for (i,aᵢ) ∈ enumerate(Iterators.rest(cs,2)) if !iszero(aᵢ))
-2 + (-2 * p * (x ^ 1)) + ((2 + (-1 * p)) * (x ^ 2)) + (1 * (x ^ 3))
```
"""
coefficients(ex::SymbolicEquation, x) = coefficients(ex.lhs - ex.rhs, x)
function coefficients(ex, x)
    # x is variable? expression?
    _ispolynomial(ex, x) || return nothing
    ex = _expand(ex, x)
    cs = is_operation(+)(ex) ? children(ex) : (ex,)
    d = Dict{Any, Any}()
    for c in cs
        (aᵢ, i) = _monomial(c, x)
        d[i] = aᵢ ⊕ get(d, i, zero(x))
    end

    n = maximum(collect(keys(d)))
    coeffs = tuple((_combine_numbers(get(d,i,zero(x))) for i in 0:n)...)
    nms = tuple((SimpleExpressions._aᵢ(i) for i in 0:n)...)

    NamedTuple{nms}(coeffs)
    
end

function _aᵢ(i)
    aᵢs = ("₀","₁","₂","₃","₄","₅","₆","₇","₈","₉")
    io = IOBuffer()
    print(io, "a")
    for j in Iterators.reverse(digits(i))
        print(io, aᵢs[1 + j])
    end
    Symbol(take!(io))
end


# take monomial and return aᵢ,i where c = aᵢ ⋅ xⁱ
_monomial(c::𝐿, x) = c == x ? (one(x), 1) : (c, 0)
function _monomial(c, x)

    @assert TermInterface.isexpr(c)
    isconstant(c) && return (c, 0)

    if is_operation(*)(c)
        ps = _monomial.(children(c), x)
        aᵢ = reduce(⊗, first.(ps), init=one(x))
        i  = sum(last.(ps))

        return (aᵢ, i)
    elseif is_operation(^)(c)
        a, b = children(c) # b is symbolic integer
        u, v = _monomial(a,x) # v is integer
        return (u^(v*b), (b()^v))
    else
        error("$(operation(c)) ")
    end
end
    
## _expand out to + terms
_expand(ex::Number, x; __cnt=1) = error(ex)
_expand(ex::𝐿, x) = ex
function _expand(ex, x; __cnt=1)
    contains(ex, x) || return ex
    __cnt > 50 && return ex
    ex′ = _expand(operation(ex), ex, x)
    ex′ != ex && return _expand(ex′, x; __cnt= __cnt + 1)
    ex′
end

# work of expand is op by op
function _expand(::typeof(+), ex, x)
    reduce(⊕, _expand.(sort(children(ex)), x), init=zero(x))
end

function _expand(::typeof(*), ex, x)
    a = one(x)
    b = nothing
    for c ∈ children(ex)
        if isnothing(b) & is_operation(+)(c)
            b = c
            continue
        else
            a = a ⊗ _expand(c, x)
        end
    end
    isnothing(b) && return a
    return mapreduce(Base.Fix1(⊗, a), ⊕, sort(children(b)), init=zero(x))
end

function _expand(::typeof(-), ex, x)
    reduce(⊖, _expand.(children(ex), x), init=zero(x))
end


function _expand(::typeof(/), ex, x)
    a, b = children(ex)
    contains(b, x) && return ex
    a ⊗ (1 / b)
end

function _expand(::typeof(^), ex, x)
    a, b = children(ex)
    a == x && return ex
    𝑥, 𝑝 = free_symbolx(b)
    if isempty(𝑥) && isempty(𝑥)
        !isinteger(b) && return ex
        n = b()
        n < 0 && return ex
        l = one(x)
        for i in 1:n
            l = l ⊗ a
        end
        return l
    end
    ex
end

_expand(::Any, ex, x) = ex # nothing to do?

## clean up constants
_combine_numbers(ex::𝐿) = ex
_combine_numbers(ex) = _combine_numbers(operation(ex), ex)

function _combine_numbers(::typeof(+), ex)
    args = _combine_numbers.(sort(children(ex)))
    foldl(⊕, args, init=zero(ex))
end

function _combine_numbers(::typeof(*), ex)
    args = _combine_numbers.(sort(children(ex)))
    foldl(⊗, args, init=one(ex))
end

function _combine_numbers(::Any, ex)
    args = _combine_numbers.(children(ex))
    maketerm(typeof(ex), operation(ex), args, nothing)
end




show_types(x, sp="") = println(sp, typeof(x))
function show_types(x::SymbolicExpression, sp="")
    println(operation(x))
    for c in children(x)
        show_types(c, sp * "  ")
    end
end

## ---- r_to_l and l_to_r move x terms to l non-x to r
function r_to_l(l, r::𝑉, x)
    if r == x
        l = l /r
        r = one(x)
    end
    l, r
end

r_to_l(l, r::SymbolicExpression, x) = r_to_l(operation(r), l, r, x)

function r_to_l(::typeof(/), l, r, x)
    a, b, = children(r)
    r′ = one(r)
    if contains(a, x)
        l = l / a
    else
        r′ = a
    end

    if contains(b, x)
        l = l ⊗ b
    else
        r′ = r′ / b
    end

    l, r′
end

function r_to_l(::typeof(-), l, r, x)
    a, b, = children(r)
    r′ = zero(r)
    if contains(a, x)
        l = l ⊖ a
    else
        r′ = a
    end

    if contains(b, x)
        l = l ⊕ b
    else
        r′ = r′ ⊖ b
    end

    l, r′
end

function r_to_l(::typeof(^), l, r, x)
    a, b, = children(r)

    if !contains(b, x)
        if !isvariable(b)
            bb = b()
            bb == 0 && return l, one(x)
            bb == 1 && return l, a
            bb == 2 && return sqrt(l), a
            bb == 3 && return cbrt(l), a
        end
        l,r = l^(1/b), a
    end
    return l, r
end

function r_to_l(::typeof(+), l, r, x)
    r′ = zero(r)
    for c ∈ children(r)
        if contains(c, x)
            l = l ⊖ c
        else
            r′ = r′ ⊕ c
        end
    end
    l, r′
end
        
function r_to_l(::typeof(*), l, r, x)
    r′ = one(r)
    for c ∈ children(r)
        if contains(c, x)
            l = l ⨸ c
        else
            r′ = r′ ⊗ c
        end
    end
    l, r′
end

# apply inverse?
function r_to_l(::Any, l, r, x)
    !contains(r, x) && return l, r  # leave as is if no x
    
    op = operation(r)
    op⁻¹ = get(inverse_functions, op, nothing)

    if !isnothing(op⁻¹)
        r = only(children(r))
        l = op⁻¹(l)
    end

    return l, r
end
    

## l to r: leave x terms, move others
function l_to_r(l::𝑉, r, x)
    l, r
end

l_to_r(l::SymbolicExpression, r, x) = l_to_r(operation(l), l, r, x)


function l_to_r(::typeof(/), l, r, x)
    a, b, = children(l)
    l′ = one(l)
    if contains(a, x)
        l′ = a
    else
        r = r ⨸ a
    end
    if contains(b, x)
        if !contains(l′, x)  # take reciprocal
            l′ = b ⨸ l′
            r = 1 ⨸ r
        else
            l′ = l′ ⨸ b
        end
    else
        r = r ⊗ b
    end

    l′, r
end

function l_to_r(::typeof(-), l, r, x)
    a, b, = children(l)
    l′ = zero(l)
    if !contains(a, x)
        r = r ⊖ a
    else
        l′ = a
    end

    if !contains(b, x)
        r = r ⊕ b
    else
        l′ = l′ ⊖ b
    end

    l, r′
end

function l_to_r(::typeof(^), l, r, x)
    a, b = children(l)
    if !contains(b, x)
        if !isvariable(b)
            bb = b()
            bb == 0 && return one(x), r
            bb == 1 && return a, r
            bb == 2 && return a, sqrt(r)
            bb == 3 && return a, cbrt(r)
        end
        l,r = a, r^(one(x) ⨸ b)
    end
    return l, r
end

function l_to_r(::typeof(*), l, r, x)
    l′ = one(l)
    for c ∈ children(l)
        if contains(c, x)
            l′ = l′ ⊗ c            
        else
            r = r ⨸ c
        end
    end
    l′, r
end


# apply inverse?
function l_to_r(::Any, l, r, x)
    op = operation(l)
    op⁻¹ = get(inverse_functions, op, nothing)

    if !isnothing(op⁻¹)
        l = only(children(l))
        r = op⁻¹(r)
    end

    return l, r
end


function l_to_r(::typeof(+), l, r, x)
    cs = children(l)
    l′ = zero(l)
    for c ∈ cs
        if contains(c, x)
            l′ = l′ ⊕ c
        else
            r = r ⊖ c
        end
    end
    return l′, r
end



