module DynamicConstants

using ..ConstantValue: ConstantValue

export DynamicConstant

struct DynamicConstant{T}
    value::T
end

function ConstantValue.constant_value(c::DynamicConstant)
    c.value
end

end
