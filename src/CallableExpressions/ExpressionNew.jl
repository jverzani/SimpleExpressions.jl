module ExpressionNew

using ..ExpressionConstructor

export expression_new

function expression_new_overload end

function expression_new(::Type{T}, op, ch, md) where {T}
    constructor = expression_constructor(T)::Type{>:T}
    expression_new_overload(constructor, op, ch, md)::constructor
end

end
