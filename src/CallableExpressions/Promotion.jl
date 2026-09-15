module Promotion

using ..StaticConstants, ..DynamicConstants, ..StaticVariables, ..DynamicVariables

function Base.promote_rule(::Type{DynamicConstant{U}}, ::Type{DynamicConstant{V}}) where {U,V}
    W = promote_type(U, V)
    DynamicConstant{W}
end

function Base.promote_rule(::Type{StaticConstant{U}}, ::Type{DynamicConstant{V}}) where {U,V}
    W = promote_type(typeof(U), V)
    DynamicConstant{W}
end

function Base.promote_rule(::Type{StaticConstant{U}}, ::Type{StaticConstant{V}}) where {U,V}
    W = promote_type(typeof(U), typeof(V))
    DynamicConstant{W}
end

function Base.promote_rule(::Type{StaticVariable{V}}, ::Type{DynamicVariable}) where {V}
    DynamicVariable
end

function Base.promote_rule(::Type{StaticVariable{U}}, ::Type{StaticVariable{V}}) where {U,V}
    DynamicVariable
end

end
