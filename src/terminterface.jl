

## ---- TermInterface v2.0
TermInterface.operation(x::AbstractSymbolic) = nothing
TermInterface.operation(x::SymbolicExpression) = (↓(x)).operation

TermInterface.arguments(x::AbstractSymbolic) = nothing
function TermInterface.arguments(x::SymbolicExpression)
    children = (↓(x)).children
    assymbolic.(children)
    #[assymbolic(child) for child in children]
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

TermInterface.head(ex::SymbolicExpression) =  TermInterface.operation(ex)
TermInterface.children(ex::SymbolicExpression) = TermInterface.arguments(ex)
