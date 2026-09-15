module CallableExpressions

include("ConstantValue.jl")
include("VariableName.jl")
include("ExpressionOperation.jl")
include("ExpressionChildren.jl")
include("ExpressionConstructor.jl")
include("ExpressionMetadata.jl")
include("ExpressionNew.jl")
include("DynamicConstants.jl")
include("DynamicVariables.jl")
include("DynamicExpressions.jl")
include("StaticConstants.jl")
include("StaticVariables.jl")
include("StaticExpressions.jl")
include("MoreStaticExpressions.jl")
include("ExpressionTypeAliases.jl")
include("ExpressionWithChildren.jl")
include("ExpressionEvaluated.jl")
include("Callable.jl")
include("EqualityAndHashing.jl")
include("ExpressionIsConstant.jl")
include("ExpressionMapMatched.jl")
include("Convert.jl")
include("ExpressionIntoTypeDomain.jl")
include("Promotion.jl")

# TODO: package extension: https://juliahub.com/ui/Packages/General/RecipesBase

# TODO: package extension: https://juliahub.com/ui/Packages/General/TermInterface

# TODO: package extension: conversion from/to https://juliahub.com/ui/Packages/General/SimpleExpressions

# TODO: package extension: conversion from/to https://juliahub.com/ui/Packages/General/DynamicExpressions

# TODO: package extension: conversion from/to https://juliahub.com/ui/Packages/General/Polynomials

# TODO: package extension: conversion from/to https://juliahub.com/ui/Packages/General/MultivariatePolynomials

using
    .ConstantValue, .VariableName, .ExpressionOperation, .ExpressionChildren,
    .ExpressionIsConstant, .ExpressionWithChildren, .ExpressionMapMatched,
    .ExpressionIntoTypeDomain, .ExpressionMetadata, .ExpressionNew,
    .DynamicExpressions, .StaticExpressions, .MoreStaticExpressions,
    .DynamicVariables,   .StaticVariables,
    .DynamicConstants,   .StaticConstants

export
    DynamicExpression, StaticExpression, MoreStaticExpression,
    DynamicVariable, StaticVariable,
    DynamicConstant, StaticConstant,
    expression_operation, expression_children, expression_with_children,
    expression_metadata, expression_new,
    expression_is_constant, expression_map_matched, expression_into_type_domain,
    ExpressionTypeAliases

"""
    DynamicExpression(children, operation_key, operations)

Constructor for an expression.
"""
DynamicExpression

"""
    StaticExpression(children, operation)

Constructor for an expression.
"""
StaticExpression

"""
    MoreStaticExpression{<:Any,Operation}(children)

Constructor for an expression, keeping the operation in the type domain.
"""
MoreStaticExpression

"""
    DynamicVariable(name)

Constructor for a variable.
"""
DynamicVariable

"""
    StaticVariable{Name}()

Constructor for a variable, keeping the name in the type domain.
"""
StaticVariable

"""
    DynamicConstant(value)

Constructor for a constant.
"""
DynamicConstant

"""
    StaticConstant{value}()

Constructor for a constant, keeping the value in the type domain.
"""
StaticConstant

"""
    expression_operation(expression)

Returns the top-level operation of the given expression.
"""
expression_operation

"""
    expression_children(expression)

Returns the immediate children of the given expression.
"""
expression_children

"""
    expression_with_children(expression, children)

Returns an expression based on `expression`, but with `children` as its immediate children.
"""
expression_with_children

"""
    expression_metadata(expression)

Returns the *metadata* associated with an expression. See [expression_new](@ref).
"""
expression_metadata

"""
    expression_new(expression_type::Type, operation, children, metadata)

Returns an expression whose type is based on `expression_type`, but with `children` as its
immediate children and with `operation` as its top-level operation.

The `metadata` argument may carry additional necessary data. Use `nothing` as the default.
"""
expression_new

"""
    expression_is_constant(expression)::Bool

Return `true` if and only if expression is constant, in the sense of not depending on any
variable.
"""
expression_is_constant

"""
    expression_map_matched(predicate, mapping, expression)

Returns an expression based on `expression`, but with `mapping` applied to each matching
subexpression. The argument `predicate` determines whether an expression is matching.
"""
expression_map_matched

"""
    expression_into_type_domain(expression)

Returns an expression equal to `expression` whose data is completely contained in the type
domain.

May throw in case Julia isn't able to use a relevant value as a type parameter.

Some properties, should hold for any valid `expr`:

* `Base.issingletontype(typeof(expression_into_type_domain(expr)))`

* `expr == expression_into_type_domain(expr)`
"""
expression_into_type_domain

"""
    ExpressionTypeAliases::Module

A module exporting several type aliases.
"""
ExpressionTypeAliases

end
