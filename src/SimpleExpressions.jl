"""
    SimpleSymbolics

A *very* lightweight means to create callable functions using expressions.

The [`@symbolic`](@ref) macro, the lone export, can create a symbolic variable and optional symbolic parameter. When expressions are created with these variables, evaluation is deferred.

The expressions subtype `Function` so are intended to be useful with `Julia`'s higher-order functions. The expressions can be called either as `u(x)` or `u(x, p)` to substitute in for the symbolic value (and parameter).

There are no performance claims, this package is all about convenience. Similar convenience is available in some form with `SymPy`, `SymEngine, `Symbolics`, etc. As well, placeholder syntax is available in `Underscores.jl`, `Chain.jl`, `DataPipes.jl` etc., This package only has value in that it is very lightweight and, hopefully, intuitively simple.

An extension is provided for functions in `SpecialFunctions`.

An exension is provided for `TermInterface` which allows the use of `Metatheory` to rewrite terms.


"""
module SimpleExpressions

export @symbolic

## -----
"""
    @symbolic x [p]

Create a symbolic variable and optional symbolic parameter.

# Expressions and equations

Expressions created using these variables subclass `Function` so may be used where functions are expected.

The  `~` infix operator can be used to create equations, which are treated as `lhs - rhs` when used as functions.

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
u = cos(x) - p*x
u(nothing, 2)  # cos(x) - 2 * x
u(pi, nothing) # -1.0 - p * π
```

The main use is as an easier-to-type replacement for anonymous functions, though with differences:

```julia
1 |> sin(x) |> x^2  # sin(1)^2
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

```julia
using Plots
plot(x^5 - x - 1, 0, 1.5)
```

# Extended help

Using this is a convenience for *simple* cases. It is easy to run into idiosyncracies.

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

Similarly for symbolic parameters.

## Broadcasting with `literal_pow`

The treatment of literal integer powers is not caught properly by this package. A hack, whereby broadcasting is always used is introduced.

```julia
@symbolic x
u = x ^ 2      # prints as x.^2, as broadcasting will be used
v = x .^ 2     # explicitly uses broadcasting
u([1,2])       # broken, should throw an error, but [1,2] .^ 2 employed
v([1,2])       # [1, 4]
```

## Broadcasting as a function

There is a difference -- which needs to be corrected -- where it is best to wrap the expression in a container for broadcasting. We can see it here in this artificial example:

```julia
@symbolic x
map(x^2, [1,2])    # [1, 4]
map.(x^2, [1,2])   # map.(x .^ 2, [1, 2]) ... not desirable
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


# An AbstractSymbolic instance is like a thunk from Thunks.jl
# or a delayed function.
# We reify by calling with (x) or (x,p) specified
abstract type AbstractSymbolic <: Function end

# main indeterminate
struct Symbolic <: AbstractSymbolic
    x::Symbol
end
(X::Symbolic)(y, p=nothing) = y

# optional parameter
struct SymbolicParameter <: AbstractSymbolic
    p::Symbol
end
(X::SymbolicParameter)(y , p) = p

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

struct SymbolicEquation
    lhs
    rhs
end
Base.:~(a::AbstractSymbolic, b::Real) = SymbolicEquation(a,b)
Base.:~(a::Real, b::AbstractSymbolic) = SymbolicEquation(a,b)
Base.:~(a::AbstractSymbolic, b::AbstractSymbolic) = SymbolicEquation(a,b)

(X::SymbolicEquation)(x, p=nothing) = subs(X.lhs, x,p) - subs(X.rhs,x, p)

## ----

Base.show(io::IO, ::MIME"text/plain", x::AbstractSymbolic) = show(io, x)
Base.show(io::IO, x::Symbolic) = print(io, x.x)
Base.show(io::IO, p::SymbolicParameter) = print(io, p.p)
function Base.show(io::IO, x::SymbolicExpression)
    broadcast = ""
    if x.op == Base.broadcasted
        broadcast= "."
        op, arguments... = x.arguments
    else
        op, arguments = x.op, x.arguments
    end

    infix_ops = (+,-,*,/,//,^) # infix
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
    _subs(X.op, X.arguments, y, p)
end
function _subs(op::Any, args, y, p=nothing)
    op(subs.(args, Ref(y), Ref(p))...) # recurse
end

subs(x::Symbolic, y, p=nothing) = something(y, x)
subs(x::SymbolicParameter, y, p=nothing) = something(p, x)
subs(x, y, p=nothing) = x

## -----
# unary
Base.:-(x::AbstractSymbolic) = SymbolicExpression(-, (x, ))

# binary
for op ∈ (:+, :-, :*, :/, ://, :\, :^, :(==), :(!=), :<, :(<=), :>, :(>=), :≈)
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

# for generic programming
for fn ∈ (:sum, :prod,
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

include("scalar-derivative.jl")
end
