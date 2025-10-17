module DynamicExpressions

using ..ExpressionOperation: ExpressionOperation
using ..ExpressionChildren: ExpressionChildren
using ..ExpressionConstructor: ExpressionConstructor
using ..ExpressionMetadata: ExpressionMetadata
using ..ExpressionNew: ExpressionNew

export DynamicExpression

struct DynamicExpression{Children, AllowedOperations}
    children::Children
    operation_key::Symbol
    operations::AllowedOperations
end

function ExpressionOperation.expression_operation(e::DynamicExpression)
    getproperty(e.operations, e.operation_key)
end

function ExpressionChildren.expression_children_overload(e::DynamicExpression)
    e.children
end

function ExpressionConstructor.expression_constructor(::Type{<:DynamicExpression})
    DynamicExpression
end

function ExpressionMetadata.expression_metadata(e::DynamicExpression)
    e.operations
end

function ExpressionNew.expression_new_overload(::Type{DynamicExpression}, operation, children, ::Nothing)
    DynamicExpression(children, :operation, (; operation = operation))
end

function ExpressionNew.expression_new_overload(::Type{DynamicExpression}, operation, children, operations)
    operation_key = nothing
    for key ∈ keys(operations)
        key::Symbol
        if getproperty(operations, key) == operation
            operation_key = key
            break
        end
    end
    if isnothing(operation_key)
        throw(ArgumentError("operation not found in metadata"))
    end
    operation_key::Symbol
    DynamicExpression(children, operation_key, operations)
end

end
