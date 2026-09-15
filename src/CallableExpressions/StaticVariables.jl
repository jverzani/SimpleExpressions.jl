module StaticVariables

using ..VariableName: VariableName

export StaticVariable

struct StaticVariable{sym} end

function VariableName.variable_name(::StaticVariable{sym}) where {sym}
    sym
end

end
