module EqualityAndHashing

using ..ExpressionTypeAliases, ..ConstantValue, ..VariableName, ..ExpressionOperation, ..ExpressionChildren

function Base.:(==)(l::Constant, r::Constant)
    constant_value(l) == constant_value(r)
end

function Base.:(==)(l::Variable, r::Variable)
    variable_name(l) == variable_name(r)
end

function elementwise_equality(l, r)
    ret = true
    any_missing = false
    for e ∈ zip(l, r)
        q = ==(e...)::Union{Bool,Missing}
        if ismissing(q)
            any_missing = true
        elseif !q
            ret = false
            break
        end
    end
    if !ret
        false
    elseif any_missing
        missing
    else
        true
    end
end

function Base.:(==)(l::Expression, r::Expression)
    ops = expression_operation(l) == expression_operation(r)
    is_missing_ops = ismissing(ops)
    if !is_missing_ops && !ops
        false
    else
        let cl = expression_children(l), cr = expression_children(r)
            if length(cl) != length(cr)
                false
            else
                let x = elementwise_equality(cl, cr), is_missing_x = ismissing(x)
                    if !is_missing_x && !x
                        false
                    elseif is_missing_ops | is_missing_x
                        missing
                    else
                        true
                    end
                end
            end
        end
    end
end

function Base.isequal(l::Constant, r::Constant)
    isequal(constant_value(l), constant_value(r))
end

function Base.isequal(l::Variable, r::Variable)
    isequal(variable_name(l), variable_name(r))
end

function elementwise_isequal(l, r)
    ret = true
    for e ∈ zip(l, r)
        if !isequal(e...)
            ret = false
            break
        end
    end
    ret::Bool
end

function Base.isequal(l::Expression, r::Expression)
    isequal(expression_operation(l), expression_operation(r)) &&
    let cl = expression_children(l), cr = expression_children(r)
        (length(cl) == length(cr)) && elementwise_isequal(cl, cr)
    end::Bool
end

function Base.hash(x::Constant, h::UInt)
    g = 0x01ed892e74c73afa15dfb9ccc620e69d % UInt
    hh = xor(g, h)::UInt
    y = constant_value(x)
    hash(y, hh)::UInt
end

function Base.hash(x::Variable, h::UInt)
    g = 0xf226876c2d8e3044b41e510cc9a2ac21 % UInt
    hh = xor(g, h)::UInt
    y = variable_name(x)
    hash(y, hh)::UInt
end

function Base.hash(x::Expression, h::UInt)
    g = 0x4a4f5b6de0f858a456cb8ef91d8c1161 % UInt
    hh = xor(g, h)::UInt
    y = expression_operation(x)
    r = hash(y, hh)::UInt

    for c ∈ expression_children(x)
        r = hash(c, r)::UInt
    end

    r::UInt
end

end
