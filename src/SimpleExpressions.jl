#=
$(joinpath(@__DIR__, "..", "README.md") |>
  x -> join(Base.Iterators.drop(readlines(x), 5), "\n")) |>
u -> replace(u, "```julia" => "```jldoctest readme"))
=#
"""
    SimpleExpressions

$(joinpath(@__DIR__, "..", "README.md") |>
  x -> join(Base.Iterators.drop(readlines(x), 5), "\n"))

"""
module SimpleExpressions
using CallableExpressions
import TermInterface
import TermInterface: operation, children

export @symbolic


"""
    @symbolic x [p]

Create a symbolic variable and optional symbolic parameter.

# Expressions and equations

Expressions created using these variables subclass `Function` so may be used where functions are expected.

The  `~` infix operator can be used to create equations, which, by default, are treated as `lhs - rhs` when called as functions.

# Extended help

# Example

```julia
using SimpleExpressions
@symbolic x
u = x^5 - x - 1
u(2) # 29 call is u(x)

@symbolic x p
u = sum(x .* p)
u(2, [1,2]) # 6  call is u(x, p)
```

Calling with `nothing`, `missing`, or `:` in a slot *substitutes* in the specified value leaving a symbolic expression, possible with no variable or parameter.

```julia
@symbolic x p
u = cos(x) - p*x
u(nothing, 2)  # cos(x) - 2 * x
u(:, 2)        #  cos(x) - 2 * x, alternate calling form
u(pi, nothing) # -1.0 - p * π
v = u(1,:)(:,2)    # (cos(1)-(2*1)),
```

The latter can be evaluated using a zero-argument call, e.g. `v()`.

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
import SimpleExpressions: D
u = x^5 - p*x - 1
D(u)           # (5 * (x ^ 4)) - p
u = u(:, 1)    # set parameter
a, b = 1, 2
find_zeros(D(u) ~ (u(b)-u(a)) / (b-a), (a,b)) # [1.577…]
```


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

Though one can make different symbolic variables, evaluation will error

```julia
@symbolic x
@symbolic y    # both x, y are `Symbolic` type
u = x + 2y
u(3)           # Error: more than one variable
```

However, this is only to simplify the call interface. Using *keyword* arguments allows evaluation with different values:

```julia
u(;x=3, y=2)   # 7
```

The underlying `CallableExpressions` object is directly called in the above manner; that package does not have the narrowed design of this package.


The variables may be used as placeholders for containers, e.g.

```julia
u = sum(xi*pi for (xi, pi) in zip(x,p))
u((1,2),(3,4))  # 11
```


## Broadcasting as a function

There is a difference -- which needs to be corrected -- where it is best to wrap the expression in a container for broadcasting. We can see it here in this artificial example:

```julia
@symbolic x
map(x^2, [1,2])    # [1, 4]
map.(x^2, [1,2])   # map.(x^2, [1, 2]); map.(x->x^2, [1,2]) is [1,4]
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
Base.broadcastable(x::AbstractSymbolic) = Ref(x)

# not symmetrically defined so 1 .+ u might be different than
# u .+ 1
function Base.broadcasted(op, a::AbstractSymbolic, as...)
     SymbolicExpression(Base.broadcasted, (op, a, as...))
end


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

Base.zero(::AbstractSymbolic) = SymbolicNumber(0)
Base.one(::AbstractSymbolic)  = SymbolicNumber(1)


const Δ = :nothing
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


# broadcast
function Base.broadcasted(::typeof(Base.literal_pow), u, a::AbstractSymbolic,
                          p::Val{N}) where {N}
    SymbolicExpression(Base.broadcasted, (^, a,N))
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

# ↓ \downarrow[tab] returns something in `CallableExpressions.jl` language
↓(x::AbstractSymbolic) = x.u
↓(x::Number) = DynamicConstant(x)
↓(x::ExpressionTypeAliases.ExpressionLoosely) = x
↓(x) = DynamicConstant(x)

# ↑ \uparrow[tab; returns SimpleExpression
↑ = assymbolic

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
for op ∈ (:+, :-, :*, :/, ://, :^,  :(==), :(!=), :<, :(<=), :>, :(>=), :≈)
    @eval begin
        import Base: $op
        Base.$op(x::AbstractSymbolic, y::AbstractSymbolic) =
            SymbolicExpression(StaticExpression((↓(x), ↓(y)), $op))
        Base.$op(x::AbstractSymbolic, y::Number) = $op(promote(x,y)...)
        Base.$op(x::Number, y::AbstractSymbolic) = $op(promote(x,y)...)

end
end


## lists from AbstractNumbers.jl
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
    :<<, :>>, :abs2, :sign, :sinpi, :cospi, :exp10,
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

## generic functions
for fn ∈ (:eachindex,
          :enumerate,
          :length,
          :first, :last, :only,
          )
        @eval begin
            import Base: $fn
            $fn(x::AbstractSymbolic) = SymbolicExpression(StaticExpression((↓(x),), $fn))
        end
end


## binary operations
for op ∈ (:zip, :getindex, :atan,)
    @eval begin
        import Base: $op
        Base.$op(x::AbstractSymbolic, y::AbstractSymbolic) =
            SymbolicExpression(StaticExpression((↓(x), ↓(y)), $op))
        Base.$op(x::AbstractSymbolic, y) =
            SymbolicExpression(StaticExpression((↓(x), ↓(y)), $op))
        Base.$op(x, y::AbstractSymbolic) =
            SymbolicExpression(StaticExpression((↓(x), ↓(y)), $op))
    end
end

## special cases
Base.log(a::Number, x::AbstractSymbolic) = log(x) / log(symbolicnumber(a))

## handle integer powers
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



## ----- show
# show (Fix parens)
Base.show(io::IO, ::MIME"text/plain", x::AbstractSymbolic) = show(io, x)
Base.show(io::IO, x::AbstractSymbolic) = _show(io, ↓(x))

_show(io::IO, u::DynamicVariable) = print(io, Symbol(u))
_show(io::IO, u::StaticVariable{S}) where {S} = print(io, S)
_show(io::IO, u::DynamicConstant) = print(io, u.value)

function Base.show(io::IO, x::SymbolicExpression)
    broadcast = ""
    op, arguments = operation(x), children(x)
    if op == Base.broadcasted
        broadcast= "."
        op′, arguments... = arguments
        op = ↓(op′).value
    end

    infix_ops = (+, - , *, /, //, ^, >=, >, ==, !=, <, <=) # infix
    if op ∈ infix_ops
        if length(arguments) == 1
            print(io, broadcast, string(op), "(")
            show(io, only(arguments))
            print(io, ")")
        else
            n = length(arguments)
            for (i, a) ∈ enumerate(arguments)
                isa(a, SymbolicExpression) && operation(a) ∈ infix_ops && print(io, "(")
                show(io, a)
                isa(a, SymbolicExpression) && operation(a) ∈ infix_ops && print(io, ")")
                i != n && print(io, " ", broadcast, string(op), " ")
            end
            #=
            a, bs..., c = arguments
            isa(a, SymbolicExpression) && a.op ∈ infix_ops && print(io, "(")
            show(io, a)
            isa(a, SymbolicExpression) && a.op ∈ infix_ops && print(io, ")")
            print(io, " ", broadcast, string(op), " ")
            isa(b, SymbolicExpression) && b.op ∈ infix_ops && print(io, "(")
            show(io, b)
            isa(b, SymbolicExpression) && b.op ∈ infix_ops && print(io, ")")
            =#
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

function Base.show(io::IO, eq::SymbolicEquation)
    show(io, eq.lhs)
    print(io, " ~ ")
    show(io, eq.rhs)
end


# catch others
_show(io::IO, x) = show(io, x)

## ----
# convert to Expr

Base.convert(::Type{Expr}, x::Symbolic) = Symbol(x)
Base.convert(::Type{Expr}, p::SymbolicParameter) = Symbol(p)
Base.convert(::Type{Expr}, x::SymbolicNumber) = x()
function Base.convert(::Type{Expr}, x::SymbolicExpression)
    op, arguments = operation(x), children(x)
    Expr(:call,  op, convert.(Expr, assymbolic.(arguments))...)
end


## ---- call
Base.Symbol(x::Symbolic) = Symbol(↓(x))
Base.Symbol(x::SymbolicParameter) = Symbol(↓(x))
Base.Symbol(x::DynamicVariable) = x.sym
Base.Symbol(::StaticVariable{T}) where {T} = T

# used to identify x, p
# error if more than one found
find_xp(x::AbstractSymbolic) = find_xp(↓(x))
find_xp(x::DynamicVariable) = (x=Symbol(x), p=Δ)
find_xp(p::StaticVariable{T}) where {T} = (x=Δ, p=Symbol(p))
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
function find_xp(u::ExpressionTypeAliases.ExpressionLoosely)
    expression_is_constant(u) && return (;x=Δ, p=Δ)
    error("Shouldn't get here")
end

## Evaluate or substitute
##
## We can either evaluate (to return a number)
## or substitute (returning a symbolic value)
##
## To substitute use one of nothing, missing or `:` in either the x or p
## position
## * `u(x, :)` substitute for x, leaves expression with parameter
## * `u(:, p)` substitute for p, leaves expression with variable
## The result can be evaluated
##
## Evaluation can be achieved by specifying u(x), u(x,p), u(*,p), u()
## * `u(x)` evaluates the expression with the variable having the value of x. If there is a parameter in the `u` expression this will error
## * `u(x,p)`  evaluates the expression with the variable having the value of x and the parameter having the variable p. If there is no parameter, the value of p is ignored
## * `u(*, p)` evaluates the expression with the parameter having the variable p. If the expression has a variable, this will error. If the expression has just a parameter any value for the first argument besides `nothing`, `missing` or `:` can be passed, `*` is just visually appealing and is always defined
## * `u()` if after substitution the expression has no free symbols, this will evaluate it.

const MISSING = Union{Nothing, Missing, typeof(:)}

(𝑥::Symbolic)(x) = x
(𝑥::Symbolic)(x,p) = x
(𝑥::Symbolic)(::MISSING, p) = 𝑥

(𝑝::SymbolicParameter)(x) = 𝑝
(𝑝::SymbolicParameter)(x,p) = p
(𝑝::SymbolicParameter)(x,::MISSING) = 𝑝

(𝑐::SymbolicNumber)(args...; kwargs...) = 𝑐.u.value

## we have substitution (using :) or evaluate
function (ex::SymbolicExpression)(x,::MISSING)
    substitutex(ex, x)
end

function (ex::SymbolicExpression)(::MISSING, p)
    substitutep(ex, p)
end

# evaluation
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

# substitute for x
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

# replace SimpleExpression variable with a value (number, variable, or expression)
"""
    replace(ex::SymbolicExpression, args::Pair...)

Replace symbolic variables and parameters with another value. Returns a symbolic object. The replacement is specified using `variable => value`; these are processed left to right.
"""
function Base.replace(ex::SymbolicExpression, args::Pair...)
    for pr in args
        k,v = pr
        pred = ==(↓(k))
        mapping = _ -> ↓(v)
        ex = SymbolicExpression(expression_map_matched(pred, mapping, ↓(ex)))
    end
    ex
end
(ex::SymbolicExpression)(args::Pair...) = replace(ex, args...)

function 𝑥𝑝!(ex::SymbolicExpression)
    𝑥, 𝑝 = ex.x[], ex.p[]
    if 𝑥 == Δ && 𝑝 == Δ
        𝑥, 𝑝 = find_xp(ex)
        ex.x[] = 𝑥
        ex.p[] = 𝑝
    end
    𝑥,𝑝
end


# directly call with kwargs.
# direct call can be quite more performant but requires
# specification of the variable/parameter name in the call.
(𝑥::Symbolic)(;kwargs...) = (↓(𝑥))(NamedTuple(kwargs))
(𝑝::SymbolicParameter)(;kwargs...) = (↓(𝑝))(NamedTuple(kwargs))
(ex::SymbolicExpression)(;kwargs...) = (↓(ex))(NamedTuple(kwargs))


## ---- comparison, sorting
# only used for domain restrictions
Base.ifelse(p::AbstractSymbolic, a, b) = SymbolicExpression(ifelse, (p,a,b))

## utils?
Base.isequal(x::AbstractSymbolic, y::AbstractSymbolic) = hash(x) == hash(y)
Base.isequal(x::AbstractSymbolic, y::Real) = hash(x) == hash(y)
Base.isequal(x::Real, y::AbstractSymbolic) = hash(x) == hash(y)

# isless for sorting
Base.isless(x::Symbolic, y::Symbolic)          = isless(Symbol(x), Symbol(y))
Base.isless(x::Symbolic, y::SymbolicParameter) = isless(Symbol(x), Symbol(y))
Base.isless(x::SymbolicParameter, y::Symbolic) = isless(Symbol(x), Symbol(y))
Base.isless(x::SymbolicParameter, y::SymbolicParameter) =
    isless(Symbol(x), Symbol(y))

Base.isless(x::SymbolicNumber, y::AbstractSymbolic) = true
Base.isless(x::AbstractSymbolic, y::SymbolicNumber) = false
Base.isless(x::SymbolicNumber, y::SymbolicNumber) = isless(x(), y())

Base.isless(x::SymbolicExpression, y::Symbolic) = false
Base.isless(x::Symbolic, y::SymbolicExpression) = !isless(y,x)
Base.isless(x::SymbolicExpression, y::SymbolicParameter) = false
Base.isless(x::SymbolicParameter, y::SymbolicExpression) = !isless(y, x)
Base.isless(x::SymbolicExpression, y::SymbolicNumber) = false
Base.isless(x::SymbolicNumber, y::SymbolicExpression) = !isless(y,x)

Base.isless(x::AbstractSymbolic, y::Number) = false
Base.isless(x::Number, y::AbstractSymbolic) = true
op_val(f) = Base.operator_precedence(Symbol(f))
function Base.isless(x::SymbolicExpression, y::SymbolicExpression)
    xo, yo = op_val(operation(x)), op_val(operation(y))
    isless(xo,yo) && return true
    isless(yo, xo) && return false
    xc, yc = children(x), children(y)
    isless(length(xc), length(yc)) && return true
    isless(length(yc), length(xc)) && return false
    for (cx, cy) ∈ zip(xc, yc)
        isless(cx, cy) && return true
        isless(cy, cx) && return false
    end
    false
end

## ----- Generators
## This is a bit hacky
struct SymbolicGenerator{T <: StaticExpression} <: AbstractSymbolic
    u::T
end

Base.show(io::IO, ex::SymbolicGenerator) = print(io, "symbolic generator")
𝑥𝑝!(ex::SymbolicGenerator) = find_xp(ex)

for fn ∈ (:sum, #:prod by mapreduce
          :map, :filter,
          :Generator
          )
    @eval begin
        Base.$fn(f, iter::AbstractSymbolic) =
            SymbolicGenerator(StaticExpression((↓(f), ↓(iter)), Base.$fn))
        Base.$fn(iter::SymbolicGenerator) =
            SymbolicGenerator(StaticExpression((↓(identity), ↓(iter)), Base.$fn))

    end
end

Base.mapreduce(f, op, iter::AbstractSymbolic, iters...) =
    SymbolicGenerator(StaticExpression((↓(f), ↓(op), ↓(iter), map(↓,iters)...), mapreduce))

# no replace, specify both
# These are very fussy
# must substitute for iterator first then function, if need be
# but that still may not work
function (ex::SymbolicGenerator)(x, p=nothing)
    # two layers
    # iter substitute, then f,
    𝑥,𝑝 = 𝑥𝑝!(ex)
    u = ↓(ex)
    if 𝑥 != Δ && p != Δ
        xs = NamedTuple{(𝑥, 𝑝)}((x,p))
        u = ↓(u)(xs)

    elseif 𝑥 != Δ # iter is non Δ
        xs = NamedTuple{(𝑥,)}((x,))
        u = u(xs)
        if !isa(p, MISSING)
            𝑥,𝑝 = find_xp(u)
            if 𝑝 != Δ
                ps = NamedTuple{(𝑝,)}((p,))
                u = ↓(u)(ps)
            end
        end
    elseif 𝑝 != Δ
        ps = NamedTuple{(𝑝,)}((p,))
        u = u(ps)
        𝑥,𝑝 = find_xp(u)
        xs = NamedTuple{(𝑥,)}((x,))
        u = ↓(u)(xs)
    end
    if isa(u, AbstractSymbolic)
        expression_is_constant(↓(u)) && (u = u())
        u = u(x,p)
        !isa(u, Number) && expression_is_constant(↓(u)) && (u = u())
        return u
    else
        return u
    end
    u
end


## includes
include("scalar-derivative.jl")

end
