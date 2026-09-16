module ExpressionWithChildren

using
    ..ExpressionTypeAliases, ..ExpressionMetadata,
    ..ExpressionNew, ..ExpressionOperation, ..DynamicExpressions

export expression_with_children

const Alia = ExpressionTypeAliases

function expression_with_children(expr::Alia.Expression, children)
    constructor = typeof(expr)
    operation = expression_operation(expr)
    metadata = expression_metadata(expr)
    expression_new(constructor, operation, children, metadata)
end

# specialize as a performance optimization
function expression_with_children(expr::DynamicExpression, children)
    DynamicExpression(children, expr.operation_key, expr.operations)
end

end
