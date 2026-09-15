module ExpressionChildren

export expression_children

function expression_children_overload end

function expression_children(e)
    ret = expression_children_overload(e)
    Base.IteratorSize(ret)::Union{Base.HasLength,Base.HasShape}
    ret
end

end
