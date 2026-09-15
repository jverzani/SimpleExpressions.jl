module ExpressionIsConstant

using ..ExpressionTypeAliases, ..ExpressionChildren

export expression_is_constant

const Alia = ExpressionTypeAliases

function expression_is_constant(x::Alia.ExpressionLoosely)
    if x isa Alia.Constant
        true
    elseif x isa Alia.Variable
        false
    else
        x::Alia.Expression
        let ret = true
            for c ∈ expression_children(x)
                if !expression_is_constant(c)
                    ret = false
                    break
                end
            end
            ret
        end
    end::Bool
end

end
