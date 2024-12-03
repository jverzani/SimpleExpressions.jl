"""
    SimpleExpressions

$(joinpath(@__DIR__, "..", "README.md") |>
  x -> join(Base.Iterators.drop(readlines(x), 5), "\n") |>
  u -> replace(u, "```julia" => "```jldoctest readme"))

"""
module SimpleExpressions
using CallableExpressions
import TermInterface

export @symbolic


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

Or using both positions, so that we call as a bivariate function:

```julia
@symbolic x y
xs = ys = range(-5, 5, length=100)
contour(xs, ys, x^2 - y^2 + 2x*y)
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


macro symbolic_expression(expr)
    @assert expr.head === :call
    op = expr.args[1]
    args = expr.args[2:end]
    Expr(:call, SymbolicExpression, esc(op),  Expr(:tuple, map(esc,args)...))
end

## ---- types ----
abstract type AbstractSymbolic <: Function end
struct Symbolic{T <: DynamicVariable} <: AbstractSymbolic
    u::T
    Symbolic(u::T) where {T <: DynamicVariable} = new{T}(u)
end

Symbolic(x::Symbolic) = x
Symbolic(x::Symbol) = Symbolic(DynamicVariable(x))
Symbolic(x::AbstractString) = Symbolic(Symbol(x))

struct SymbolicParameter{X, T <: StaticVariable{X}} <: AbstractSymbolic
    u::T
    SymbolicParameter(u::T) where {X, T <: StaticVariable{X}} = new{X,T}(u)
end
SymbolicParameter(p::SymbolicParameter) = p
SymbolicParameter(p::Symbol) = SymbolicParameter(StaticVariable{p}())
SymbolicParameter(p::AbstractString) = SymbolicParameter(Symbol(p))


struct SymbolicNumber{T <: DynamicConstant} <: AbstractSymbolic
    u::T
end
SymbolicNumber(c::SymbolicNumber) = c
SymbolicNumber(c::Number) = SymbolicNumber(DynamicConstant(c))
convert(::Type{<:AbstractSymbolic}, x::Number) = SymbolicNumber(DynamicConstant(x))

Δ = :nothing
struct SymbolicExpression{T <: StaticExpression} <: AbstractSymbolic
    u::T
    x::Base.RefValue{Symbol}
    p::Base.RefValue{Symbol}
end
SymbolicExpression(u) = SymbolicExpression(u, Ref(Δ), Ref(Δ))
function SymbolicExpression(op, children)
    u = StaticExpression(map(↓,children), op)
    SymbolicExpression(u)
end
# XXX need to check u is of proper type

# convert to symbolic
assymbolic(x::AbstractSymbolic) = x
assymbolic(x::Symbol) = Symbolic(x)
assymbolic(x::Number) = SymbolicNumber(x)
# convert from Expression to SimpleExpression
# all variables become `𝑥` except `p` becomes `𝑝`, a parameter
assymbolic(x::Expr) = eval(_assymbolic(x))
function _assymbolic(x)
    if !isexpr(x)
        isa(x, Symbol) && return x == :p ? :(SymbolicParameter(:𝑝)) : :(Symbolic(:𝑥))
        return x
    end

    op = operation(x)
    arguments = arguments(x)
    Expr(:call, op, _assymbolic.(arguments)...)
end

assymbolic(u::DynamicVariable) = Symbolic(u)
assymbolic(u::StaticVariable) = SymbolicParameter(u)
assymbolic(u::DynamicConstant) = SymbolicNumber(u)
assymbolic(u::StaticExpression) = SymbolicExpression(u)


## ----
struct SymbolicEquation{T,S}
    lhs::T
    rhs::S
end
Base.:~(a::AbstractSymbolic, b::AbstractSymbolic) = SymbolicEquation(a,b)
Base.:~(a::AbstractSymbolic, b::Number) = SymbolicEquation(a, SymbolicNumber(b))
Base.:~(a::Number, b::AbstractSymbolic) = SymbolicEquation(SymbolicNumber(a),b)

tilde(a::Number, b::Number) = a - b
tilde(a, b) = a ~ b

(X::SymbolicEquation)(x) =   tilde(X.lhs(x), X.rhs(x))
(X::SymbolicEquation)(x,p) = tilde(X.lhs(x, p),  X.rhs(x, p))


function Base.iterate(X::SymbolicEquation, state=nothing)
    isnothing(state) && return (X.lhs, 0)
    iszero(state) && return (X.rhs, 1)
    return nothing
end
Base.length(X::SymbolicEquation) = 2

## ----

↓(x) = x
↓(x::AbstractSymbolic) = x.u

Base.convert(::Type{<:AbstractSymbolic}, x::Number) = SymbolicNumber(DynamicConstant(x))
Base.convert(::Type{<:AbstractSymbolic}, x::Symbolic) = x
Base.convert(::Type{<:AbstractSymbolic}, x::SymbolicParameter) = x

Base.promote_rule(::Type{<:AbstractSymbolic}, x::Type{T}) where {T <: Number} = AbstractSymbolic

## ---- TermInterface
TermInterface.operation(x::AbstractSymbolic) = nothing
TermInterface.operation(x::SymbolicExpression) = (↓(x)).operation

TermInterface.arguments(x::AbstractSymbolic) = nothing
function TermInterface.arguments(x::SymbolicExpression)
    children = (↓(x)).children
    assymbolic.(children)
end

TermInterface.head(ex::SymbolicExpression) =  TermInterface.operation(ex)
TermInterface.children(ex::SymbolicExpression) = TermInterface.arguments(ex)

TermInterface.iscall(ex::SymbolicExpression) = true
TermInterface.iscall(ex::AbstractSymbolic) = false


TermInterface.isexpr(::Symbolic) = false
TermInterface.isexpr(::SymbolicParameter) = false
TermInterface.isexpr(::SymbolicNumber) = false
TermInterface.isexpr(::AbstractSymbolic) = true

function TermInterface.maketerm(T::Type{<:AbstractSymbolic}, head, children, metadata)
    head(assymbolic.(children)...)
end




## ---- operations
for op ∈ (:+, :-, :*, :/, ://, :^)
    @eval begin
        import Base: $op
        Base.$op(x::AbstractSymbolic, y::AbstractSymbolic) =
            SymbolicExpression(StaticExpression((↓(x), ↓(y)), $op))
        Base.$op(x::AbstractSymbolic, y::Number) = $op(promote(x,y)...)
        Base.$op(x::Number, y::AbstractSymbolic) = $op(promote(x,y)...)

end
end


# lists from AbstractNumbers.jl
unary_ops = (
    #:~,
    :-,
    :conj, :abs, :sin, :cos, :tan, :sinh, :cosh, :tanh, :asin, :acos, :atan,
    :asinh, :acosh, :atanh, :sec, :csc, :cot, :asec, :acsc, :acot, :sech, :csch,
    :coth, :asech, :acsch, :acoth, :sinc, :cosc, :cosd, :cotd, :cscd, :secd,
    :sind, :tand, :acosd, :acotd, :acscd, :asecd, :asind, :atand, :rad2deg,
    :deg2rad, :log, :log2, :log10, :log1p, :exponent, :exp, :exp2, :expm1,
    :cbrt, :sqrt, :ceil, :floor, :trunc, :round, :significand,
    :frexp, :ldexp, :modf, :real, :imag, :!, :identity,
    :zero, :one, :<<, :>>, :abs2, :sign, :sinpi, :cospi, :exp10,
    :iseven, :ispow2, :isfinite, :isinf, :isodd, :isinteger, :isreal,
    :isnan, :isempty,  :transpose, :copysign, :flipsign, :signbit,
    # :iszero,
    #:+, :-, :*, :/, :\, :^, :(==), :(!=), :<, :(<=), :>, :(>=), :≈,
    :inv,
    :min, :max,
    :div, :fld, :rem, :mod, :mod1, :cmp, :&, :|, :xor,
    :clamp,
)
for fn ∈ unary_ops
    @eval begin
        import Base: $fn
        $fn(x::AbstractSymbolic) = SymbolicExpression(StaticExpression((↓(x),), $fn))
    end
end
Base.log(a::Number, x::AbstractSymbolic) = log(x) / log(symbolicnumber(a))


# handle integer powers
Base.literal_pow(::typeof(^), x::AbstractSymbolic, ::Val{0}) = 1
Base.literal_pow(::typeof(^), x::AbstractSymbolic, ::Val{1}) = x
Base.literal_pow(::typeof(^), x::AbstractSymbolic, ::Val{2}) = SymbolicExpression(^,(x,SymbolicNumber(2)))
Base.literal_pow(::typeof(^), x::AbstractSymbolic, ::Val{3}) = SymbolicExpression(^,(x,SymbolicNumber(3)))
Base.literal_pow(::typeof(^), x::AbstractSymbolic, ::Val{-1}) = 1/x
Base.literal_pow(::typeof(^), x::AbstractSymbolic, ::Val{-2}) = 1/x^2
function Base.literal_pow(::typeof(^), x::AbstractSymbolic, ::Val{p}) where {p}
    p′ = SymbolicNumber(abs(p))
    u = SymbolicExpression(^, (x, p′))
    p < 0 ? 1 / u : u
end
function Base.broadcasted(::typeof(Base.literal_pow), u, a::AbstractSymbolic,
                          p::Val{N}) where {N}
    SymbolicExpression(Base.broadcasted, (^, a,N))
end

# XXX Generators



Base.Generator(f, iter::AbstractSymbolic) =
    SymbolicExpression(Base.Generator, (f, iter))

Base.broadcastable(x::AbstractSymbolic) = Ref(x)


# try enumerate
Base.enumerate(x::AbstractSymbolic) = SymbolicExpression(enumerate, (x,))

# TODO: binary ops (atan, ...)
#=
for fn ∈ (:sum, :prod,
          :getindex,
          :eachindex, :enumerate, :zip, :length,
          :first, :last, :only,
          )
        @eval begin
        import Base: $fn
        $fn(x::AbstractSymbolic, as) = SymbolicExpression($fn, as)
        end
end
=#
## ----- show
# show (Fix parens)
function Base.show(io::IO, x::AbstractSymbolic)
    u = ↓(x)
    _show(io, u)
end
Base.show(io::IO, ::MIME"text/plain", x::AbstractSymbolic) = show(io, x)

function Base.show(io::IO, eq::SymbolicEquation)
    show(io, eq.lhs)
    print(io, " ~ ")
    show(io, eq.rhs)
end

function _show(io::IO, u::StaticVariable{S}) where {S}
    print(io, S)
end

function _show(io::IO, u::DynamicVariable)
    print(io, u.sym)
end
function _show(io::IO, u::DynamicConstant)
    print(io, u.value)
end

# needs to have ()s managed better
function _show(io::IO, u::StaticExpression)
    op, children = u.operation, u.children
    op′ = string(op)
    n = length(children)
    if n == 1
        print(io, op′, "(")
        _show(io, first(children))
        print(io, ")")
    elseif length(children) > 1
        if op′ ∈ ("+","-","*","/","^")
            print(io, "(")
            for (i,c) ∈ enumerate(children)
                _show(io, c)
                i != n && print(io, op′)
            end
            print(io, ")")
        else
            # infix, prefix?
            print(io, string(op), "(")
            for c ∈ children
                _show(io, c)
                print(io, ",")
            end
            print(io, ")")
        end
    end
end

# catch others
_show(io::IO, x) = show(io, x)

## ---- call
# used to identify x, p
# error if more than one found
find_xp(x::AbstractSymbolic) = find_xp(↓(x))

_name(x::Symbolic) = _name(↓(x))
_name(x::SymbolicParameter) = _name(↓(x))
_name(x::DynamicVariable) = x.sym
_name(::StaticVariable{T}) where {T} = T

find_xp(x::DynamicVariable) = (x=_name(x), p=Δ)
find_xp(p::StaticVariable{T}) where {T} = (x=Δ, p=_name(p))
find_xp(x::DynamicConstant) = (x=Δ, p=Δ)
function find_xp(u::StaticExpression)
    x, p = Δ, Δ
    for c ∈ u.children
        o = find_xp(c)
        x′, p′ = o.x, o.p
        if x == Δ
            x = x′
        elseif !(x′ == Δ)
            x == x′ || error("more than one variable")
            x = x′
        end
        if p == Δ
            p = p′
        elseif p′ != Δ
            p == p′ || error("more than one variable")
            p = p′
        end
    end
    (;x, p)
end

# call (x,p), (:,p), (x,:), or (x) are patterns to support
# we have substitution if one variable is not given
# evaluation if all variables are specified

MISSING = Union{Nothing, Missing, typeof(:)}

(𝑥::Symbolic)(x) = x
(𝑥::Symbolic)(x,p) = x
(𝑥::Symbolic)(::MISSING, p) = 𝑥

(𝑝::SymbolicParameter)(x) = 𝑝
(𝑝::SymbolicParameter)(x,p) = p
(𝑝::SymbolicParameter)(x,::MISSING) = 𝑝

(𝑐::SymbolicNumber)(args...; kwargs...) = c.u.value

# add in MISSING, p; x,MISSING here
# we have substitution if one variable is not given
# evaluation if all variables are specified
function (ex::SymbolicExpression)(x,p=nothing)

    # cache symbols; check for doubles
    𝑥, 𝑝 = ex.x[], ex.p[]
    if 𝑥 == Δ && 𝑝 == Δ
        𝑥, 𝑝 = find_xp(ex)
        ex.x[] = 𝑥
        ex.p[] = 𝑝
    end

    # this is kinda eww
    # eval: if x,p are both specified
    doeval = true
    if (𝑥 != Δ && isa(x, MISSING))
        doeval = false
    elseif (𝑝 != Δ && isa(p, MISSING))
        doeval = false
    end

    if !doeval
        if !isa(x,MISSING)
            substitutex(ex, x)
        else
            substitutep(ex, p)
        end
    else
        if 𝑥 == Δ
            _call(ex, (𝑝,), p)
        elseif 𝑝 == Δ
            _call(ex, (𝑥,), x)
        else
            _call(ex, (𝑥, 𝑝), x, p)
        end
    end
end

_call(ex, 𝑥, x) =  (↓(ex))(NamedTuple{𝑥}((x,)))
_call(ex, 𝑥𝑝, x, p) =  (↓(ex))(NamedTuple{𝑥𝑝}((x,p)))

# subsitute for x
function substitutex(ex, x)
    pred = x -> isa(x, DynamicVariable)
    mapping = _ -> DynamicConstant(x)
    SymbolicExpression(expression_map_matched(pred, mapping, ↓(ex)))
end

# substitute for p
function substitutep(ex, p)
    pred = p -> isa(p, StaticVariable)
    mapping = _ -> DynamicConstant(p)
    SymbolicExpression(expression_map_matched(pred, mapping, ↓(ex)))
end


# directly call with kwargs.
(𝑥::Symbolic)(;kwargs...) = (↓(𝑥))(NamedTuple(kwargs))
(𝑝::SymbolicParameter)(;kwargs...) = (↓(𝑝))(NamedTuple(kwargs))
(ex::SymbolicExpression)(;kwargs...) = (↓(ex))(NamedTuple(kwargs))




## includes
include("scalar-derivative.jl")

end
