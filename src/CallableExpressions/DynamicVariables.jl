module DynamicVariables

using ..VariableName: VariableName

export DynamicVariable

struct DynamicVariable
    sym::Symbol
end

function VariableName.variable_name(v::DynamicVariable)
    v.sym
end

end
