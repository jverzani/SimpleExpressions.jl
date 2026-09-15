module ExpressionMapMatched

using ..ExpressionTypeAliases, ..ExpressionChildren, ..ExpressionWithChildren

export expression_map_matched

const Alia = ExpressionTypeAliases

function expression_map_matched(is_match::P, f::F, x::Alia.ExpressionLoosely) where {P,F}
    if is_match(x)
        f(x)
    else
        if x isa Union{Alia.Constant,Alia.Variable}
            x
        else
            x::Alia.Expression
            old_children = expression_children(x)
            children = let
                g = let is_match = is_match, f = f
                    y -> expression_map_matched(is_match, f, y)
                end
                map(g, old_children)
            end
            expression_with_children(x, children)
        end
    end::Alia.ExpressionLoosely
end

end
