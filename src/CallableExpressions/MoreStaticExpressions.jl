module MoreStaticExpressions

using ..ExpressionOperation: ExpressionOperation
using ..ExpressionChildren: ExpressionChildren
using ..ExpressionConstructor: ExpressionConstructor
using ..ExpressionMetadata: ExpressionMetadata
using ..ExpressionNew: ExpressionNew

export MoreStaticExpression

struct MoreStaticExpression{Children, Operation}
    children::Children
end

function MoreStaticExpression{<:Any,Op}(children::C) where {Op,C}
    MoreStaticExpression{C,Op}(children)
end

function ExpressionOperation.expression_operation(::MoreStaticExpression{<:Any,Op}) where {Op}
    Op
end

function ExpressionChildren.expression_children_overload(e::MoreStaticExpression)
    e.children
end

function ExpressionConstructor.expression_constructor(::Type{<:MoreStaticExpression})
    MoreStaticExpression
end

function ExpressionMetadata.expression_metadata(::MoreStaticExpression)
    nothing
end

function ExpressionNew.expression_new_overload(::Type{MoreStaticExpression}, operation, children, ::Nothing)
    MoreStaticExpression{<:Any,operation}(children)
end

end
