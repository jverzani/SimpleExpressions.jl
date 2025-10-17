module Callable

using ..ExpressionTypeAliases, ..ExpressionEvaluated

function (e::ExpressionLoosely)(variable_values)
    expression_evaluated(e, variable_values)
end

end
