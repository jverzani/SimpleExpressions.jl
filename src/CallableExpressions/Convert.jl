module Convert

using
    ..StaticConstants, ..DynamicConstants,
    ..StaticVariables, ..DynamicVariables,
    ..MoreStaticExpressions, ..StaticExpressions, ..DynamicExpressions,
    ..ExpressionTypeAliases, ..ConstantValue, ..VariableName,
    ..ExpressionOperation, ..ExpressionChildren

const Alia = ExpressionTypeAliases

# constants and variables

function Base.convert(::Type{StaticConstant}, x::Alia.Constant)
    v = constant_value(x)
    StaticConstant{v}()
end

function Base.convert(::Type{DynamicConstant}, x::Alia.Constant)
    v = constant_value(x)
    DynamicConstant(v)
end

function Base.convert(::Type{DynamicConstant{T}}, x::Alia.Constant) where {T}
    u = constant_value(x)
    v = convert(T, u)::T
    DynamicConstant(v)
end

function Base.convert(::Type{StaticVariable}, x::Alia.Variable)
    v = variable_name(x)
    StaticVariable{v}()
end

function Base.convert(::Type{DynamicVariable}, x::Alia.Variable)
    v = variable_name(x)
    DynamicVariable(v)
end

function Base.convert(::Type{StaticConstant{V}}, x::Alia.Constant) where {V}
    convert(StaticConstant, x)::StaticConstant{V}
end

function Base.convert(::Type{StaticVariable{V}}, x::Alia.Variable) where {V}
    convert(StaticVariable, x)::StaticVariable{V}
end

# expressions

function Base.convert(::Type{T}, x::S) where {T<:Alia.Expression,S<:T}
    x::T
end

function Base.convert(::Type{MoreStaticExpression}, x::Union{StaticExpression,DynamicExpression})
    operation = expression_operation(x)
    children = expression_children(x)
    MoreStaticExpression{<:Any,operation}(children)
end

function Base.convert(::Type{StaticExpression}, x::Union{MoreStaticExpression,DynamicExpression})
    operation = expression_operation(x)
    children = expression_children(x)
    StaticExpression(children, operation)
end

end
