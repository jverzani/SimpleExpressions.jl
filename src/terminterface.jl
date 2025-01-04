

## ---- TermInterface v2.0
TermInterface.operation(x::AbstractSymbolic) = nothing
TermInterface.operation(x::SymbolicExpression) = (↓(x)).operation

TermInterface.arguments(x::AbstractSymbolic) = nothing
function TermInterface.arguments(x::SymbolicExpression)
    children = (↓(x)).children
    assymbolic.(children)
end

TermInterface.head(ex::SymbolicExpression) =  TermInterface.operation(ex)
TermInterface.children(ex::SymbolicExpression) = [TermInterface.arguments(ex)...] # return AbstractVector not a tuple

TermInterface.iscall(ex::SymbolicExpression) = true
TermInterface.iscall(ex::AbstractSymbolic) = false

TermInterface.isexpr(::SymbolicVariable) = false
TermInterface.isexpr(::SymbolicParameter) = false
TermInterface.isexpr(::SymbolicNumber) = false
TermInterface.isexpr(::AbstractSymbolic) = true

function TermInterface.maketerm(T::Type{<:AbstractSymbolic}, head, children, metadata)
    head(assymbolic.(children)...)
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
