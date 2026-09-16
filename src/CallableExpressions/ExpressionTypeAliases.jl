module ExpressionTypeAliases

using
    ..DynamicConstants, ..StaticConstants,
    ..DynamicVariables, ..StaticVariables,
    ..DynamicExpressions, ..StaticExpressions, ..MoreStaticExpressions

export Constant, Variable, Expression, ExpressionLoosely

"""
    Constant::Type

Constant.
"""
const Constant = Union{DynamicConstant,StaticConstant}

"""
    Variable::Type

Variable.
"""
const Variable = Union{DynamicVariable,StaticVariable}

"""
    Expression::Type

*Proper* expression, i.e., non-leaf expressions.
"""
const Expression = Union{DynamicExpression,StaticExpression,MoreStaticExpression}

"""
    ExpressionLoosely::Type

`Union{Constant,Variable,Expression}`.
"""
const ExpressionLoosely = Union{Constant,Variable,Expression}

end
