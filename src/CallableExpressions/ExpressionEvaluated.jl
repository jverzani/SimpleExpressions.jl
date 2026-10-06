module ExpressionEvaluated

using
  ..ExpressionTypeAliases,
  ..ConstantValue, ..VariableName, ..ExpressionOperation, ..ExpressionChildren

export expression_evaluated

function expression_evaluated(c::Constant, ::Any)
    constant_value(c)
end

function expression_evaluated(v::Variable, variable_values)
    name = variable_name(v)
    getproperty(variable_values, name)
end

function expression_evaluated(e::Expression, variable_values)
    f = let variable_values = variable_values
        x -> expression_evaluated(x, variable_values)
    end
    children = expression_children(e)
    operation = expression_operation(e)
    args = map(f, children)
    _apply(operation, args...) # XXX modification
end

_apply(op, args...) = op(args...)
# `2^-4` throws for integers; `1/x` is stored as `x^-1`
_apply(::typeof(^), a::Integer, b::Integer) = b < 0 ? float(a)^b : a^b

end
