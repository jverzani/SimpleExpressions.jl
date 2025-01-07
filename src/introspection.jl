## ---- introspection
Base.Symbol(x::SymbolicVariable) = Symbol(↓(x))
Base.Symbol(x::SymbolicParameter) = Symbol(↓(x))
Base.Symbol(x::DynamicVariable) = x.sym
Base.Symbol(::StaticVariable{T}) where {T} = T

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
xp(x::AbstractSymbolic) = xp(↓(x))
xp(x::StaticVariable{T}) where {T} = (x=Symbol(x), p=Δ)
xp(p::DynamicVariable) = (x=Δ, p=Symbol(p))
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




# free_symbols return unique collection of symbols for the
# existing symbolic variables and parameters in the expression
free_symbols(x) = (x=(), p=())
free_symbols(x::AbstractSymbolic) = free_symbols(↓(x))
free_symbols(x::DynamicConstant) = (x=(), p=())
free_symbols(x::DynamicVariable) = (x=(), p=(Symbol(x),))
free_symbols(x::StaticVariable) = (x=(Symbol(x),), p=())
function free_symbols(ex::StaticExpression)
    x,p = (), ()
    for c ∈ ex.children
        𝑥, 𝑝 = free_symbols(c)
        x = _mergetuple(x, 𝑥)
        p = _mergetuple(p, 𝑝)
    end
    (;x, p)
end

# f contains symbolic variable or expression x
Base.contains(f::AbstractSymbolic, x) = contains(↓(f), ↓(x))
Base.contains(f::Any, x::𝑋) where 𝑋 = false
Base.contains(f::_Variable, x::𝑋) where 𝑋 = (f == x)

function Base.contains(f::StaticExpression, x::𝑋) where 𝑋
    f == x && return true
    for c ∈ f.children
        (x == c || contains(c, x)) && return true
    end
    return false
end

Base.occursin(x::AbstractSymbolic, f::AbstractSymbolic) = contains(f, x)

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
    x, p = free_symbols(x)
    isempty(x) && isempty(p)
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
