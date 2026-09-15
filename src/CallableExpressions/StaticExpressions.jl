module StaticExpressions

using ..ExpressionOperation: ExpressionOperation
using ..ExpressionChildren: ExpressionChildren
using ..ExpressionConstructor: ExpressionConstructor
using ..ExpressionMetadata: ExpressionMetadata
using ..ExpressionNew: ExpressionNew

export StaticExpression

struct StaticExpression{Children, Operation}
    children::Children
    operation::Operation
end

function ExpressionOperation.expression_operation(e::StaticExpression)
    e.operation
end

function ExpressionChildren.expression_children_overload(e::StaticExpression)
    e.children
end

function ExpressionConstructor.expression_constructor(::Type{<:StaticExpression})
    StaticExpression
end

function ExpressionMetadata.expression_metadata(::StaticExpression)
    nothing
end

function ExpressionNew.expression_new_overload(::Type{StaticExpression}, operation, children, ::Nothing)
    StaticExpression(children, operation)
end

end
