"""
    SimpleExpressions

$(joinpath(@__DIR__, "..", "README.md") |>
  x -> join(Base.Iterators.drop(readlines(x), 5), "\n") |>
  u -> replace(u, "```julia" => "```jldoctest readme"))

"""
module SimpleExpressions
export @symbolic


## -----
"""
    @symbolic x [p]

Create a symbolic variable and optional symbolic parameter.

# Expressions and equations

Expressions created using these variables subclass `Function` so may be used where functions are expected.

The  `~` infix operator can be used to create equations, which, by default, are treated as `lhs - rhs` when used as functions.

# Extended help

# Example

```julia
using SimpleExpressions
@symbolics x
u = x^5 - x - 1
u(2) # 29 call is u(x)

@symbolic x p
u = sum(x .* p)
u(2, [1,2]) # 6  call is u(x, p)
```

Calling with `nothing` in a slot leaves the variable

```julia
@symbolic x p
u = cos(x) - p*x
u(nothing, 2)  # cos(x) - 2 * x
u(:, 2)        #  cos(x) - 2 * x, alternate calling form
u(pi, nothing) # -1.0 - p * π
```

The main use is as an easier-to-type replacement for anonymous functions, though with differences:

```julia
1 |> sin(x) |> x^2  # sin(1)^2
u = cos(x) - p*x
2 |> u(:, 3) # u(2,3) alternative
```

```julia
map(x^2, (1, 2)) # (1,4)
```

Can be used with other packages, to simplify some function calls at the expense of being non-idiomatic:

```julia
using Roots
@symbolic x p
find_zero(x^5 - x - 1, 1)     # 1.167...
find_zero(x^5 - x ~ p, 1, 4)  # 1.401...

using ForwardDiff
Base.adjoint(𝑓::Function) = x -> ForwardDiff.derivative(𝑓, x)
u = x^5 - x - 1
find_zero((u,u'), 1, Roots.Newton()) # 1.167...
```

Or

```julia
using Plots
plot(x^5 - x - 1, 0, 1.5)
```

Symbolic derivatives can be taken with respect to the symbolic value, symbolic parameters are treated as constant.

```julia
@symbolic x p
D = SimpleExpressions.D  # not exported
u = x^5 - p*x - 1
D(u)           # (5 * (x ^ 4)) - p
u = u(:, 1)    # set parameter
a, b = 1, 2
find_zeros(D(u) ~ (u(b)-u(a)) / (b-a), (a,b)) # [1.577…]
```


# Extended help

Using this is a convenience for *simple* cases. It is easy to run into idiosyncrasies.

## Expressions are not functions in terms of scope

Unlike functions, expressions are defined with variables at the time of definition, not when called. For example, with a clean environment:

```julia
@symbolic x
u = m*x + b    # errors, `m` not defined
f(x) = m*x + b # ok
m, b = 1, 2
u = m*x + b    # defined using `m` amd `b` at time of assignment
u(3)           # 1 * 3 + 2
f(3)           # 1 * 3 + 2 values of `m` and `b` when called
m, b = 3, 4
u(3)           # still computing 1 * 3 + 2
f(3)           # computing 3 * 3 + 4, using values of `m` and `b` when called
```

## Symbolic values are really singletons

Though one can make different symbolic variables, they are all indistinguishable for purposes of evaluation:

```julia
@symbolic x
@symbolic y    # both x, y are `Symbolic` type
u = x + 2y
u(3)           # 9 or 3 + 2*3
```

Similarly for symbolic parameters. The variables may be used as containers though, e.g. `u=sum(xi*pi for (xi, pi) in zip(x,p)); u((1,2),(3,4))`.


## Broadcasting as a function

There is a difference -- which needs to be corrected -- where it is best to wrap the expression in a container for broadcasting. We can see it here in this artificial example:

```julia
@symbolic x
map(x^2, [1,2])    # [1, 4]
map.(x^2, [1,2])   # map.(x^2, [1, 2]) ... not desirable
map.([x^2], [1,2]) # [1, 4]
```

"""
macro symbolic(x...)
    q=Expr(:block)
    push!(q.args, Expr(:(=), esc(x[1]), Expr(:call, Symbolic, Expr(:quote, x[1]))))
    if length(x) > 1
        push!(q.args, Expr(:(=), esc(x[2]), Expr(:call, SymbolicParameter, Expr(:quote, x[2]))))
    end
    push!(q.args, Expr(:tuple, map(esc, x)...))
    q
end

"""
    @symbolic_expression expr

Take a function call and return a symbolic (delayed) expression.

# Example
```
@symbolic x
u = @symbolic_expression quadgk(sin, 0, x)

# from ?foldl
u = @symbolic_expression foldl(=>, @symbolic_expression(1:x))
u(4) # ((1 => 2) => 3) => 4
```

Not exported
"""
macro symbolic_expression(expr)
    @assert expr.head === :call
    op = expr.args[1]
    args = expr.args[2:end]
    Expr(:call, SymbolicExpression, esc(op),  Expr(:tuple, map(esc,args)...))
end



# An AbstractSymbolic instance is like a thunk from Thunks.jl
# or a delayed function.
# We reify by calling with (x) or (x,p) specified
abstract type AbstractSymbolic <: Function end

# main indeterminate
struct Symbolic <: AbstractSymbolic
    x::Symbol
end
(X::Symbolic)(y, p=nothing) = subs(X,y,p)
(X::Symbolic)() = X(nothing)

# optional parameter
struct SymbolicParameter <: AbstractSymbolic
    p::Symbol
end
(X::SymbolicParameter)(y , p) = subs(X,y,p)

struct SymbolicNumber <: AbstractSymbolic
    x::Number
end
(X::SymbolicNumber)(y,p=nothing) = subs(X,y,p)
(X::SymbolicNumber)() = X(nothing)

# don't specialize for faster first usage
struct SymbolicExpression <: AbstractSymbolic
    op
    arguments
end

function (X::SymbolicExpression)(x, p=nothing)
    X = subs(X, x, p)
    if isa(X, AbstractSymbolic)
        X = subs(X, x, p)     # generators need to repeat...
    end
    X
end

(X::SymbolicExpression)() = X(nothing)

function (X::SymbolicExpression)(x::SymbolicNumber, p=nothing)
    X = subs(X, x, p)
end

struct SymbolicEquation
    lhs
    rhs
end
Base.:~(a::AbstractSymbolic, b::Number) = SymbolicEquation(a, SymbolicNumber(b))
Base.:~(a::Number, b::AbstractSymbolic) = SymbolicEquation(SymbolicNumber(a),b)
Base.:~(a::AbstractSymbolic, b::AbstractSymbolic) = SymbolicEquation(a,b)

(X::SymbolicEquation)(x, p=nothing) = subs(X.lhs, x,p) - subs(X.rhs,x, p)


## ----
assymbolic(x::AbstractSymbolic) = x
assymbolic(x::Any) = SymbolicNumber(x)

issymbolic(x::AbstractSymbolic) = true
issymbolic(::Any) = false

# has a Symbolic term in expression
hassymbolic(x::Number) = false
hassymbolic(x::Symbolic) = true
hassymbolic(x::SymbolicParameter) = false
hassymbolic(x::SymbolicNumber) = false
hassymbolic(x::SymbolicExpression) = any(hassymbolic.(x.arguments))

# find free symbol in an expression or nothing
free_symbol(u::SymbolicEquation) = free_symbol(u.lhs - u.rhs)
free_symbol(u::Symbolic) = u
free_symbol(::Any) = nothing
function free_symbol(u::SymbolicExpression)
    args = u.arguments
    for a ∈ u.arguments
        a′ = free_symbol(a)
        isa(a′, Symbolic) && return a′
        if isa(a′, SymbolicEquation)
            u = free_symbol(a′)
            isa(a′′, Symbolic) && return a′′
        end
    end
    return nothing
end


## ----

Base.show(io::IO, ::MIME"text/plain", x::AbstractSymbolic) = show(io, x)
Base.show(io::IO, x::Symbolic) = print(io, x.x)
Base.show(io::IO, p::SymbolicParameter) = print(io, p.p)
Base.show(io::IO, x::SymbolicNumber) = print(io, x.x)
function Base.show(io::IO, x::SymbolicExpression)
    broadcast = ""
    if x.op == Base.broadcasted
        broadcast= "."
        op, arguments... = x.arguments
    else
        op, arguments = x.op, x.arguments
    end

    infix_ops = (+,-,*,/,//,^, >=, >, ==, !=, <, <=) # infix
    if op ∈ infix_ops
        if length(arguments) == 1
            print(io, string(op), "(")
            show(io, only(arguments))
            print(io, ")")
        else
            a, b = arguments
            isa(a, SymbolicExpression) && a.op ∈ infix_ops && print(io, "(")
            show(io, first(arguments))
            isa(a, SymbolicExpression) && a.op ∈ infix_ops && print(io, ")")
            print(io, " ", broadcast, string(op), " ")
            isa(b, SymbolicExpression) && b.op ∈ infix_ops && print(io, "(")
            show(io, b)
            isa(b, SymbolicExpression) && b.op ∈ infix_ops && print(io, ")")
        end
    elseif op == ifelse
        p,a,b = arguments
        print(io, "𝕀(")
        show(io, p)
        print(io, ")")
    elseif op == getindex
        a, idx = arguments
        show(io, a)
        print(io, "[")
        show(io, idx)
        print(io, "]")
    else
        print(io, op, broadcast, "(")
        join(io, arguments, ", ", ", ")
        print(io, ")")

    end
end
Base.show(io::IO, ::MIME"text/plain", x::SymbolicEquation) = show(io, x)
function Base.show(io::IO, x::SymbolicEquation)
    show(io, x.lhs)
    print(io, " ~ ")
    show(io, x.rhs)
end


## -----
function subs(X::SymbolicExpression, y, p=nothing)
    y′ = (y == :) ? nothing : y
    _subs(X.op, X.arguments, y′, p)
end
function _subs(op::Any, args, y, p=nothing)
    op(subs.(args, Ref(y), Ref(p))...) # recurse
end

subs(x::Symbolic, y, p=nothing) = something((y == :) ? nothing : y, x)
subs(x::SymbolicParameter, y, p=nothing) = something(p, x)
subs(x::SymbolicNumber, y=nothing, p=nothing) = x.x
subs(x, y, p=nothing) = x

subs(x::Symbolic, y::SymbolicNumber, p) = y

## -----
# unary
Base.:-(x::AbstractSymbolic) = SymbolicExpression(-, (x, ))
#
function _commutative_op(op::typeof(+), x, y)
    iszero(x) && return y
    iszero(y) && return x
    SymbolicExpression(+, isless(x, y) ? (x,y) : (y,x))
end

function _commutative_op(op::typeof(*), x, y)
    isone(x) && return y
    isone(y) && return x
    (iszero(x) || iszero(y)) && return 0
    SymbolicExpression(*, isless(x, y) ? (x,y) : (y,x))
end

# commutative binary; slight canonicalization
# plans to incorporate simplify are WIP/DOA
for op ∈ (:+, :*)
    @eval begin
        import Base: $op
        Base.$op(x::AbstractSymbolic, y::Number) = _commutative_op($op, x, y)
        Base.$op(x::Number, y::AbstractSymbolic) = _commutative_op($op, x, y)
        Base.$op(x::AbstractSymbolic, y::AbstractSymbolic) =
            _commutative_op($op, x, y)
    end
end

# binary
for op ∈ (:-, :/, ://, :\, :^, :(==), :(!=), :<, :(<=), :>, :(>=), :≈)
    @eval begin
        import Base: $op
        Base.$op(x::AbstractSymbolic, y::Number) = SymbolicExpression($op, (x,y))
        Base.$op(x::Number, y::AbstractSymbolic) = SymbolicExpression($op, (x,y))
        Base.$op(x::AbstractSymbolic, y::AbstractSymbolic) = SymbolicExpression($op, (x,y))    end
end



# lists from AbstractNumbers.jl
for fn ∈ (
    #:~,
    :conj, :abs, :sin, :cos, :tan, :sinh, :cosh, :tanh, :asin, :acos, :atan,
    :asinh, :acosh, :atanh, :sec, :csc, :cot, :asec, :acsc, :acot, :sech, :csch,
    :coth, :asech, :acsch, :acoth, :sinc, :cosc, :cosd, :cotd, :cscd, :secd,
    :sind, :tand, :acosd, :acotd, :acscd, :asecd, :asind, :atand, :rad2deg,
    :deg2rad, :log, :log2, :log10, :log1p, :exponent, :exp, :exp2, :expm1,
    :cbrt, :sqrt, :ceil, :floor, :trunc, :round, :significand,
    :frexp, :ldexp, :modf, :real, :imag, :!, :identity,
    :zero, :one, :<<, :>>, :abs2, :sign, :sinpi, :cospi, :exp10,
    :iseven, :ispow2, :isfinite, :isinf, :isodd, :isinteger, :isreal,
    :isnan, :isempty, :iszero, :transpose, :copysign, :flipsign, :signbit,
    #:+, :-, :*, :/, :\, :^, :(==), :(!=), :<, :(<=), :>, :(>=), :≈,
    :min, :max,
    :div, :fld, :rem, :mod, :mod1, :cmp, :&, :|, :xor,
    :clamp,
)
    @eval begin
        import Base: $fn
        $fn(x::AbstractSymbolic, as...) = SymbolicExpression($fn, (x, as...))
    end
end
Base.log(a::Number, x::AbstractSymbolic) = log(x) / log(SymbolicNumber(a))

# for generic programming
for fn ∈ (:sum, :prod,:inv,
          :getindex,
          :eachindex, :enumerate, :zip,:length,
          :first, :last, :only,
          )
        @eval begin
        import Base: $fn
        $fn(x::AbstractSymbolic, as...) = SymbolicExpression($fn, (x, as...))
    end
end

Base.Generator(f, iter::AbstractSymbolic) = SymbolicExpression(Base.Generator, (f, iter))

Base.broadcastable(x::AbstractSymbolic) = Ref(x)

# not symmetrically defined so 1 .+ u might be different than
# u .+ 1
function Base.broadcasted(op, a::AbstractSymbolic, as...)
     SymbolicExpression(Base.broadcasted, (op, a, as...))
end

# handle integer powers
Base.literal_pow(::typeof(^), x::AbstractSymbolic, ::Val{0}) = 1
Base.literal_pow(::typeof(^), x::AbstractSymbolic, ::Val{1}) = x
Base.literal_pow(::typeof(^), x::AbstractSymbolic, ::Val{2}) = SymbolicExpression(^,(x,2))
Base.literal_pow(::typeof(^), x::AbstractSymbolic, ::Val{3}) = SymbolicExpression(^,(x,3))
Base.literal_pow(::typeof(^), x::AbstractSymbolic, ::Val{-1}) = 1/x
Base.literal_pow(::typeof(^), x::AbstractSymbolic, ::Val{-2}) = 1/x^2
function Base.literal_pow(::typeof(^), x::AbstractSymbolic, ::Val{p}) where {p}
    u = SymbolicExpression(^,(x,abs(p)))
    p < 0 ? 1 / u : u
end
function Base.broadcasted(::typeof(Base.literal_pow), u, a::AbstractSymbolic,
                          p::Val{N}) where {N}
    SymbolicExpression(Base.broadcasted, (^, a,N))
end


function Base.broadcasted(style::Base.Broadcast.BroadcastStyle, f::AbstractSymbolic, args...)
    subs.([f], args...)
end

function _subs(::typeof(Base.broadcasted), args, y, p=nothing)
    op, as... = args
    u = Base.broadcast(op, subs.(as, Ref(y), Ref(p))...)
    Base.materialize(u)
end

# only used for domain restrictions
Base.ifelse(p::AbstractSymbolic, a::Real, b::Real) = SymbolicExpression(ifelse, (p,a,b))

## utils?
Base.isequal(x::AbstractSymbolic, y::AbstractSymbolic) = hash(x) == hash(y)
Base.isequal(x::AbstractSymbolic, y::Real) = hash(x) == hash(y)
Base.isequal(x::Real, y::AbstractSymbolic) = hash(x) == hash(y)

# rough complexity count used in `isless`
nodes(x::Any) = 0
nodes(x::Real) = (atan(x) + pi/2)/pi
nodes(::Symbolic) = 1
nodes(::SymbolicParameter) = 1
nodes(::SymbolicNumber) = 1
nodes(ex::SymbolicEquation) = nodes(ex.lhs) + nodes(ex.rhs)
function nodes(ex::SymbolicExpression)
    op = ex.op
    n = op ∈ (+, *) ? 1 : 5
    n + sum(nodes(a) for a ∈ ex.arguments)
end

Base.isless(x::AbstractSymbolic, y::Real) = false
Base.isless(x::Real, y::AbstractSymbolic) = true
Base.isless(x::Symbolic, y::AbstractSymbolic) = true
Base.isless(x::AbstractSymbolic, y::Symbolic) = false
Base.isless(x::Symbolic, y::Symbolic) = isless(x.x, y.x)
Base.isless(x::AbstractSymbolic, y::AbstractSymbolic) = isless(nodes(x), nodes(y))

# convert to Expr
Base.convert(::Type{Expr}, x::Symbolic) = x.x
Base.convert(::Type{Expr}, x::SymbolicParameter) = x.p
Base.convert(::Type{Expr}, x::SymbolicNumber) = x.x
Base.convert(::Type{Expr}, x::SymbolicExpression) =
    Expr(:call, x.op, convert.(Expr, assymbolic.(x.arguments))...)

## includes
include("scalar-derivative.jl")

end
