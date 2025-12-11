## ---- introspection
Base.Symbol(x::SymbolicVariable{T}) where {T} = T
Base.Symbol(x::SymbolicParameter) = ↓(x).sym
Base.nameof(x::SymbolicVariable{T}) where {T} = T
Base.nameof(x::SymbolicParameter) = ↓(x).sym

## ----
# convert to Expr

Base.convert(::Type{Expr}, x::SymbolicVariable) = Symbol(x)
Base.convert(::Type{Expr}, p::SymbolicParameter) = Symbol(p)
Base.convert(::Type{Expr}, x::SymbolicNumber) = x()
function Base.convert(::Type{Expr}, x::SymbolicExpression)
    op, args = operation(x), arguments(x)
    Expr(:call,  op, convert.(Expr, assymbolic.(args))...)
end

## ----
# used to identify x, p
# error if more than one found
# much faster than `free_symbols` as this is type stable

const Δ = :nothing # flag for missing symbols 𝑥, 𝑝

xp(::Any) =  (x=Δ, p=Δ)
xp(x::AbstractSymbolic) = xp(↓(x))
xp(x::StaticVariable{T}) where {T} = (x=T, p=Δ)
xp(p::DynamicVariable) = (x=Δ, p=p.sym)
xp(x::DynamicConstant) = (x=Δ, p=Δ)
function xp(u::StaticExpression)
    x, p = Δ, Δ
    us = map(xp, u.children)
    for (x′, p′) ∈ us
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
    (; x, p)
end

function xp(u::ExpressionTypeAliases.ExpressionLoosely)
    expression_is_constant(u) && return (;x=Δ, p=Δ)
    error("Shouldn't get here")
end


# free_symbols return set of all variables
# in expression split by variables and parameters
free_symbols(::SymbolicNumber)     = (x=(),   p=())
free_symbols(p::SymbolicParameter) = (x=(),   p=(p,))
free_symbols(x::SymbolicVariable)  = (x=(x,), p=())
function free_symbols(ex::SymbolicExpression)
    x,p = (), ()
    for c ∈ arguments(ex)
        𝑥, 𝑝 = free_symbols(c)
        x = _union(x, 𝑥)
        p = _union(p, 𝑝)
    end
    (;x, p)
end

# merge new elements of c′ with c
function _union(c, c′)
    for 𝑐 ∈ c′
        !(𝑐 ∈ c) && (c = TupleTools.vcat(c, 𝑐))
    end
    c
end


# f contains symbolic variable or expression x
Base.contains(f::AbstractSymbolic, x) = false
Base.contains(f::𝑉, x::𝑋) where 𝑋 = (f == x)

function Base.contains(f::SymbolicExpression, x::𝑋) where 𝑋
    f == x && return true
    for c ∈ arguments(f)
        (x == c || Base.contains(c, x)) && return true
    end
    return false
end


Base.occursin(x::AbstractSymbolic, f::AbstractSymbolic) = contains(f, x)

# does expression contain the operation
function contains_operation(ex::AbstractSymbolic, op)
    iscall(ex) || return false
    operation(ex) == op && return true
    any(contains_operation(op), arguments(ex)) && return true
    return false
end
contains_operation(op) = Base.Fix2(contains_operation, op)


# we have some means to query expressions
# isnumeric -- contains no SymbolicVariable or SymbolicParameter.
# isconstant -- contains no SymbolicVariable (possibly SymbolicParameter)
# isvariable -- is a SymbolicVariable or SymbolicConstant
#

# Tests whether a Symbolic value (character) is numeric.
Base.isnumeric(x::AbstractSymbolic) = false
Base.isnumeric(x::SymbolicNumber) = true
Base.isnumeric(x::SymbolicParameter) = false
Base.isnumeric(x::SymbolicVariable) = false
function Base.isnumeric(x::SymbolicExpression)
    return CallableExpressions.expression_is_constant(↓(x))
end

# predicate to see if expression contains a symbolic variable
# see also contains(expr, x) for a specific variable
isconstant(x::Number) = true
isconstant(x::AbstractSymbolic) = isconstant(↓(x))
isconstant(x::DynamicConstant) = true
isconstant(x::DynamicVariable) = true # parameters are constant here
isconstant(x::StaticVariable) = false
function isconstant(x::StaticExpression)
    for c ∈ x.children
        isconstant(c) || return false
    end
    return true
end

# isvariable
isvariable(expr) = false
isvariable(::SymbolicVariable) = true
isvariable(::SymbolicParameter) = true

# isnegative (used with `combine`)
isnegative(expr) = false
isnegative(x::𝑉) = false
isnegative(x::SymbolicNumber) = x() < 0
isnegative(x::Number) = x < 0
isnegative(x::SymbolicExpression) = isnumeric(x) && x() < 0
