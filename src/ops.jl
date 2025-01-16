#=
Below, only a select few operations are defined for symbolic values. To add one could be done as follows.

```@example expressions
import SimpleExpressions: AbstractSymbolic, SymbolicExpression, D
import SimpleExpressions.CallableExpressions: StaticExpression
function Base.fourthroot(x::AbstractSymbolic)
    u = StaticExpression((x,), fourthroot)
	SymbolicExpression(u)
end
D(::typeof(fourthroot), args,x) = (𝑥 = only(args); D(𝑥,x) * fourthroot(x)^3 / 4)

fourthroot(x^2 + 2)
```
=#
## ---- operations
for op ∈ (://, :^,  :≈)
    @eval begin
        import Base: $op
        Base.$op(x::AbstractSymbolic, y::AbstractSymbolic) =
            SymbolicExpression(StaticExpression((↓(x), ↓(y)), $op))
        Base.$op(x::AbstractSymbolic, y::Number) = $op(promote(x,y)...)
        Base.$op(x::Number, y::AbstractSymbolic) = $op(promote(x,y)...)
    end
end

for op ∈ (:/, )
    @eval begin
        import Base: $op
        Base.$op(x::AbstractSymbolic, y::AbstractSymbolic) =
            SymbolicExpression(StaticExpression((↓(x), ↓(y)), $op))
        Base.$op(x::AbstractSymbolic, y::Number) = _isunit(*,y) ? x : $op(promote(x,y)...)
        Base.$op(x::Number, y::AbstractSymbolic) = $op(promote(x,y)...)
    end
end

## arrange for *, + to be n-ary
_isunit(::typeof(+), y::Number) = iszero(y)
_isunit(::typeof(*), y::Number) = isone(y)

for op ∈ (:*, :+)
    @eval begin
        import Base: $op
        Base.$op(x::AbstractSymbolic, y::AbstractSymbolic) =
            SymbolicExpression(StaticExpression(TupleTools.vcat(_arguments($op,x), _arguments($op,y)), $op))
        Base.$op(x::AbstractSymbolic, y::Number) = _isunit($op, y) ? x : $op(promote(x,y)...)
        Base.$op(x::Number, y::AbstractSymbolic) = _isunit($op, x) ? y : $op(promote(x,y)...)
    end
end


Base.:-(x::AbstractSymbolic, y::AbstractSymbolic) = x + (-1)*y
Base.:-(x::AbstractSymbolic, y::Number) = x + (-1)*y
Base.:-(x::Number, y::AbstractSymbolic) = x + (-1)*y


𝑄 = Union{Integer, Rational}
for op ∈ (:+, :-, :*, :^)
    @eval begin
        Base.$op(x::SymbolicNumber{DynamicConstant{T}},
                 y::SymbolicNumber{DynamicConstant{S}}) where {
                     T<:𝑄, S<:𝑄} = SymbolicNumber($op(x(),y()))
    end
end

for op ∈ (:/, ://)
    @eval begin
        Base.$op(x::SymbolicNumber{DynamicConstant{T}},
                  y::SymbolicNumber{DynamicConstant{S}}) where {
                      T<:𝑄, S<:𝑄} =
                          SymbolicNumber(x()//y())
        end
end

for op ∈ (:+, :-, :*, :^, :/ )
    @eval begin
        Base.$op(x::SymbolicNumber{DynamicConstant{T}},
                 y::SymbolicNumber{DynamicConstant{S}}) where {
                     T<:AbstractFloat, S<:AbstractFloat} = SymbolicNumber($op(x(),y()))
        Base.$op(x::SymbolicNumber{DynamicConstant{T}},
                 y::SymbolicNumber{DynamicConstant{S}}) where {
                     T<:AbstractFloat, S<:Number} = SymbolicNumber($op(x(),y()))
        Base.$op(x::SymbolicNumber{DynamicConstant{T}},
                 y::SymbolicNumber{DynamicConstant{S}}) where {
                     T<:Number, S<:AbstractFloat} = SymbolicNumber($op(x(),y()))
    end
end


# broadcasting
for op ∈ (:-, :*, :+, :/, ://, :^,  :≈)
    @eval begin
        function Base.broadcasted(::typeof($op), a::AbstractSymbolic,b::Number)
            SymbolicExpression(Base.broadcasted, ($op, a,b))
        end
        function Base.broadcasted(::typeof($op), a::Number, b::AbstractSymbolic)
            SymbolicExpression(Base.broadcasted, ($op, a,b))
        end
        function Base.broadcasted(::typeof($op), a::AbstractSymbolic,b::AbstractSymbolic)
            SymbolicExpression(Base.broadcasted, ($op, a, b))
        end
    end
end



_arguments(::Any, x::SymbolicNumber) = (↓(x),)
_arguments(::Any, x::SymbolicVariable) = (↓(x),)
_arguments(::Any, x::SymbolicParameter) = (↓(x),)

_arguments(op, x::SymbolicExpression) = _arguments(op, operation(x), x)
_arguments(::typeof(+), ::typeof(+), x::SymbolicExpression) = ↓(x).children
_arguments(::Any, ::typeof(+), x::SymbolicExpression) = (↓(x),)
_arguments(::typeof(*), ::typeof(*), x::SymbolicExpression) = ↓(x).children
_arguments(::Any, ::typeof(*), x::SymbolicExpression) = (↓(x),)
_arguments(::Any, ::Any, x::SymbolicExpression) = (↓(x),)

## comparison operators:
## The usual ==, !=, <, <=, >, >= operators are kept
## == and `isless` ares defined below to give meaning
## These, from `CommonEq` allow for symbolic equations/inequalities to be set up
## see 𝕀 for a use with domains of derivatives.
for (op, op′) ∈ ((:Eq, Symbol(==)),
                 (:Ne, Symbol(!=)),
                 (:Lt, Symbol(<)),
                 (:Le, Symbol(<=)),
                 (:Gt, Symbol(>)),
                 (:Ge, Symbol(>=)))
    @eval begin
        CommonEq.$op(x::AbstractSymbolic, y::AbstractSymbolic) =
            SymbolicExpression(StaticExpression((↓(x), ↓(y)), $op′))
    end
end

Base.:(==)(x::AbstractSymbolic, y::Number) = ==(promote(x,y)...)
Base.:(==)(x::Number, y::AbstractSymbolic) = ==(promote(x,y)...)
Base.:(==)(x::AbstractSymbolic, y::AbstractSymbolic) =
    ↓(x) == ↓(y)


## lists from AbstractNumbers.jl
unary_ops = (
    :conj, :abs, :sin, :cos, :tan, :sinh, :cosh, :tanh, :asin, :acos, :atan,
    :asinh, :acosh, :atanh, :sec, :csc, :cot, :asec, :acsc, :acot, :sech, :csch,
    :coth, :asech, :acsch, :acoth, :sinc, :cosc, :cosd, :cotd, :cscd, :secd,
    :sind, :tand, :acosd, :acotd, :acscd, :asecd, :asind, :atand, :rad2deg,
    :deg2rad, :log, :log2, :log10, :log1p, :exponent, :exp, :exp2, :expm1,
    :cbrt, :sqrt, :ceil, :floor, :trunc, :round, :significand,
    :frexp, :ldexp, :modf, :real, :imag, :!,# :identity,
    :<<, :>>, :abs2, :sign, :sinpi, :cospi, :exp10,
    :isempty,  :transpose, :copysign, :flipsign, :signbit,
    # :iszero,
    #:+, :-, :*, :/, :\, :^, :(==), :(!=), :<, :(<=), :>, :(>=), :≈,
    #:inv,
    :min, :max,
    :div, :fld, :rem, :mod, :mod1, :cmp, :&, :|, :xor,
    :clamp,
)
for fn ∈ unary_ops
    @eval begin
        import Base: $fn
        $fn(x::AbstractSymbolic) = SymbolicExpression(StaticExpression((↓(x),), $fn))
        function Base.broadcasted(::typeof($fn), a::AbstractSymbolic)
            SymbolicExpression(Base.broadcasted, ($fn, a))
        end
    end
end

Base.:-(x::AbstractSymbolic) = (-1)*x
Base.:+(x::AbstractSymbolic) = x
Base.:*(x::AbstractSymbolic) = x
Base.identity(x::AbstractSymbolic) = x

## predicates for numbers; return Boolean, not symbolic expression
for op in (:isinteger, :ispow2,
           :iszero, :isone,
           :iseven, :isodd,
           :isfinite, :isinf, :isnan)
    @eval begin
        import Base: $op
        Base.$op(::AbstractSymbolic) = false
        Base.$op(c::SymbolicNumber) = $op(c())
        function Base.$op(c::SymbolicExpression)
            x,p = free_symbols(c)
            (!isempty(x) || !isempty(p)) && return false
            return $op(c())
        end
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
## math one with broadcasting
for fn ∈ (:atan, )
    @eval begin
        import Base: $fn
        Base.$fn(x::AbstractSymbolic, y::AbstractSymbolic) =
            SymbolicExpression(StaticExpression((↓(x), ↓(y)), $fn))
        Base.$fn(x::AbstractSymbolic, y) =
            SymbolicExpression(StaticExpression((↓(x), ↓(y)), $fn))
        Base.$fn(x, y::AbstractSymbolic) =
            SymbolicExpression(StaticExpression((↓(x), ↓(y)), $fn))
        function Base.broadcasted(::typeof($fn), a::AbstractSymbolic, b)
            SymbolicExpression(Base.broadcasted, ($fn, a, b))
        end
    end
end

for op ∈ (:zip, :getindex,)
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
function Base.broadcasted(::typeof(log), a, b::AbstractSymbolic)
    SymbolicExpression(Base.broadcasted, (log, a, b))
end


Base.inv(a::AbstractSymbolic) = SymbolicExpression(inv, (a,))
Base.inv(a::SymbolicExpression) = _inv(operation(a), a)
_inv(::typeof(inv), a) = only(arguments(a))
function _inv(::typeof(^), a)
    u, v = arguments(a)
    isa(v, SymbolicNumber) && return u^(-v())
    u^(-v)
end
_inv(::Any, a) = SymbolicExpression(inv, (a,))

## handle integer powers
Base.literal_pow(::typeof(^), x::AbstractSymbolic, ::Val{0}) = one(x)
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
# broadcast
function Base.broadcasted(::typeof(Base.literal_pow), u, a::AbstractSymbolic,
                          p::Val{N}) where {N}
    SymbolicExpression(Base.broadcasted, (^, a,N))
end


# simplifying operations
# XXX These are really in need of removal
## plus
⊕(x::SymbolicNumber,y::SymbolicNumber) = SymbolicNumber(x() + y())
function ⊕(x,y)
    iszero(x) && return y
    iszero(y) && return x
    return x + y
end

## minus
⊖(x::SymbolicNumber,y::SymbolicNumber) = SymbolicNumber(x() - y())
function ⊖(x,y)
    iszero(x) && return -y
    iszero(y) && return x
    return x - y
end


## times
⊗(x::SymbolicNumber,y::SymbolicNumber) = SymbolicNumber(x() * y())
function ⊗(x,y)
    isone(x) && return y
    isone(y) && return x
    iszero(x) && return zero(x)
    iszero(y) && return zero(y)
    return x * y
end

## div
function ⨸(x::SymbolicNumber,y::SymbolicNumber)
    n, d = x(), y()
    # keep as rational?
    isa(n, Integer) && isa(d, Integer) && return SymbolicNumber(n // d)
    SymbolicNumber(x() / y())
end

function ⨸(x,y)
    x == y    && return one(x)
    isone(y)  && return x
    iszero(x) && return zero(x)
    !isinf(x) && isinf(y) && return zero(x)



    # can cancel?
    if is_operation(/)(y)
        a, b = arguments(y)
        return (x ⊗ b) ⨸ a
    end

    if is_operation(*)(x)
        if contains(x, y) # cancel y in x; return
            out = one(x)
            for c ∈ sorted_arguments(x)
                c == y && continue
                out = out ⊗ c
            end
            return out
        end
    end

    return x / y
end
