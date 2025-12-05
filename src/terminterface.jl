## ---- TermInterface v2.0
#import TermInterface: is_operation, operation, arguments, iscall, isexpr, maketerm, head, children

function is_operation(λ)
    ex -> begin
        isexpr(ex) || return false
        operation(ex) == λ
    end
end
operation(x::AbstractSymbolic) = nothing
operation(x::SymbolicExpression) = (↓(x)).operation

arguments(x::AbstractSymbolic) = nothing
function arguments(x::SymbolicExpression)
    children = (↓(x)).children
    assymbolic.(children)
end
sorted_arguments(x::SymbolicExpression) = TupleTools.sort(arguments(x))



iscall(::SymbolicVariable) = false
iscall(::SymbolicParameter) = false
iscall(::SymbolicNumber) = false
iscall(::AbstractSymbolic) = true

isexpr(ex::SymbolicExpression) = true
isexpr(ex::AbstractSymbolic) = false


function maketerm(T::Type{<:AbstractSymbolic}, head, children, metadata)
    head(assymbolic.(children)...)
end

function maketerm(T::Type{<:SymbolicExpression}, head::SymbolicVariable, children, metadata)
    SymbolicExpression(head, children)
end

function maketerm(T::Type{<:SymbolicNumber}, ::Nothing, children, metadata)
    SymbolicNumber(DynamicConstant(only(children)))
end

function maketerm(T::Type{<:SymbolicVariable}, ::Nothing, children, metadata)
    SymbolicVariable(only(children))
end
function maketerm(T::Type{<:SymbolicParameter}, ::Nothing, children, metadata)
    SymbolicParameter(only(children))
end
function maketerm(T::Type{<:AbstractSymbolic}, ::Nothing, children, metadata)
    SymbolicNumber(only(children))
end

head(ex::SymbolicExpression) =  operation(ex)
children(ex::SymbolicExpression) = arguments(ex)

metadata(x::AbstractSymbolic) = nothing
metadata(x::AbstractSymbolic, md) = nothing

# convert string or expression to symbolic value
# using TermInterface
"""
    sympify(ex::String)

Turn a string into an expression, converting symbols to symbolic variables.

Name comes from SymPy.
"""
sympify(ex::String) = asSymbolic(Meta.parse(ex))
sympify(ex::Expr) = asSymbolic(ex)

# don't rely on TermInterface---it is an extension; these shadow it for Expressions
_isexpr(ex) = false
_isexpr(ex::Expr) = true
_iscall(ex) = false
_iscall(ex::Expr) = ex.head == :call
_operation(e::Expr) = _iscall(e) ? first(e.args) : error("operation called on a non-function call expression")
_arguments(e::Expr) = _iscall(e) ? @view(e.args[2:end]) : error("arguments called on a non-function call expression")

function asSymbolic(ex)
    if _isexpr(ex)
        maketerm(AbstractSymbolic,
                 getfield(Base, _operation(ex)),
                 asSymbolic.(_arguments(ex)),
                 nothing)
    else
        isa(ex, Symbol) ? SymbolicVariable(ex) : ex
    end
end
