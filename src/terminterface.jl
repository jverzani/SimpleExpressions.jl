## ---- TermInterface v2.0

## Expr
# from TermInterface

TermInterface.isexpr(::Symbol) = false
TermInterface.iscall(::Symbol) = false

## AbstractSymbolic
TermInterface.operation(x::AbstractSymbolic) = nothing
TermInterface.operation(x::SymbolicExpression) = (↓(x)).operation

TermInterface.arguments(x::AbstractSymbolic) = nothing
function TermInterface.arguments(x::SymbolicExpression)
    children = (↓(x)).children
    assymbolic.(children)
end
TermInterface.sorted_arguments(x::SymbolicExpression) = TupleTools.sort(arguments(x))



TermInterface.iscall(::SymbolicVariable) = false
TermInterface.iscall(::SymbolicParameter) = false
TermInterface.iscall(::SymbolicNumber) = false
TermInterface.iscall(::AbstractSymbolic) = true

TermInterface.isexpr(ex::SymbolicExpression) = true
TermInterface.isexpr(ex::AbstractSymbolic) = false


function TermInterface.maketerm(T::Type{<:AbstractSymbolic}, head, children, metadata)
    head(assymbolic.(children)...)
end

function TermInterface.maketerm(T::Type{<:SymbolicExpression}, head::SymbolicVariable, children, metadata)
    SymbolicExpression(head, children)
end

function TermInterface.maketerm(T::Type{<:SymbolicNumber}, ::Nothing, children, metadata)
    SymbolicNumber(DynamicConstant(only(children)))
end

function TermInterface.maketerm(T::Type{<:SymbolicVariable}, ::Nothing, children, metadata)
    SymbolicVariable(only(children))
end
function TermInterface.maketerm(T::Type{<:SymbolicParameter}, ::Nothing, children, metadata)
    SymbolicParameter(only(children))
end
function TermInterface.maketerm(T::Type{<:AbstractSymbolic}, ::Nothing, children, metadata)
    SymbolicNumber(only(children))
end

TermInterface.head(ex::SymbolicExpression) =  operation(ex)
TermInterface.children(ex::SymbolicExpression) = arguments(ex)

TermInterface.metadata(x::AbstractSymbolic) = nothing
TermInterface.metadata(x::AbstractSymbolic, md) = nothing

# convert string or expression to symbolic value
# using TermInterface
"""
    sympify(ex::String)

Turn a string into an expression, converting symbols to symbolic variables.

Name comes from SymPy.
"""
sympify(ex::String) = asSymbolic(Meta.parse(ex))
sympify(ex::Expr) = asSymbolic(ex)

function asSymbolic(ex)
    if isexpr(ex)
        maketerm(AbstractSymbolic,
                 getfield(Base, operation(ex)),
                 asSymbolic.(arguments(ex)),
                 nothing)
    else
        isa(ex, Symbol) ? SymbolicVariable(ex) : ex
    end
end
