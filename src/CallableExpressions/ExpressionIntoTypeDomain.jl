module ExpressionIntoTypeDomain

using
    ..ExpressionTypeAliases, ..ExpressionChildren, ..ExpressionMapMatched,
    ..StaticConstants, ..StaticVariables, ..MoreStaticExpressions

export expression_into_type_domain

const Alia = ExpressionTypeAliases

function expression_into_type_domain(e::Alia.ExpressionLoosely)
    function matcher(@nospecialize e::Alia.ExpressionLoosely)
        !Base.issingletontype(typeof(e)) &&
        if e isa Union{Alia.Constant,Alia.Variable}
            true
        else
            e::Alia.Expression
            let children = expression_children(e)
                Base.issingletontype(typeof(children))
            end
        end::Bool
    end
    function mapping(@nospecialize e::Alia.ExpressionLoosely)
        if e isa ExpressionTypeAliases.Constant
            convert(StaticConstant, e)
        elseif e isa ExpressionTypeAliases.Variable
            convert(StaticVariable, e)
        else
            e::Alia.Expression
            convert(MoreStaticExpression, e)
        end
    end
    while !Base.issingletontype(typeof(e))
        e = expression_map_matched(matcher, mapping, e)
    end
    e::Union{StaticConstant,StaticVariable,MoreStaticExpression}
end

end
