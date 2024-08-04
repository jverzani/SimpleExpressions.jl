module SimpleExpressionsTermInterfaceExt

using SimpleExpressions

import SimpleExpressions: AbstractSymbolic,
    Symbolic, SymbolicParameter, SymbolicNumber,
    SymbolicExpression, SymbolicEquation

using TermInterface

#In other symbolic expression languages, such as SymbolicUtils.jl, the head of a node can correspond to operation and children can correspond to arguments.

TermInterface.head(ex::SymbolicExpression) =  operation(ex)
TermInterface.children(ex::SymbolicExpression) = arguments(ex)

TermInterface.operation(X::SymbolicExpression) = X.op
TermInterface.arguments(X::SymbolicExpression) = collect(X.arguments)


TermInterface.iscall(ex::SymbolicExpression) = true
TermInterface.iscall(ex::AbstractSymbolic) = false


TermInterface.isexpr(::Symbolic) = false
TermInterface.isexpr(::SymbolicParameter) = false
TermInterface.isexpr(::SymbolicNumber) = false
TermInterface.isexpr(::AbstractSymbolic) = true



function TermInterface.maketerm(T::Type{<:AbstractSymbolic}, head, children, metadata)
    if isa(head, Symbol)
        head == :. && return first(children)
        @show head, children, metadata
        return 42
    end
    head(SimpleExpressions.assymbolic.(children)...)
end


TermInterface.arity(::AbstractSymbolic) = 0
TermInterface.arity(ex::SymbolicExpression) = length(ex.arguments)

TermInterface.metadata(::AbstractSymbolic) = nothing


# convert from Expression to SimpleExpression
# all variables become `𝑥` except `p` becomes `𝑝`, a parameter
function SimpleExpressions.assymbolic(x::Expr)
    body = _assymbolic(x)
    eval(body)
end

function _assymbolic(x)
    if !TermInterface.istree(x)
        isa(x, Symbol) && return x == :p ? :(SymbolicParameter(:𝑝)) : :(Symbolic(:𝑥))
        return x
    end

    op = TermInterface.operation(x)
    arguments = TermInterface.arguments(x)
    Expr(:call, op, _assymbolic.(arguments)...)
end

end
