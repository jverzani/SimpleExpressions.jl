module StaticConstants

using ..ConstantValue: ConstantValue

export StaticConstant

struct StaticConstant{Value} end

function ConstantValue.constant_value(::StaticConstant{V}) where {V}
    V
end

end
