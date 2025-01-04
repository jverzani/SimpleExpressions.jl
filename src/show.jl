
## ----- show
# show (Fix parens)
Base.show(io::IO, ::MIME"text/plain", x::AbstractSymbolic) = show(io, x)
Base.show(io::IO, x::AbstractSymbolic) = _show(io, ↓(x))

_show(io::IO, u::DynamicVariable) = print(io, Symbol(u))
_show(io::IO, u::StaticVariable{S}) where {S} = print(io, S)
_show(io::IO, u::DynamicConstant) = print(io, u.value)

function Base.show(io::IO, x::SymbolicExpression)
    broadcast = ""
    op, arguments = operation(x), children(x)
    if op == Base.broadcasted
        broadcast= "."
        op′, arguments... = arguments
        op = ↓(op′).value
    end

    infix_ops = (+, - , *, /, //, ^, >=, >, ==, !=, <, <=) # infix
    if op ∈ infix_ops
        if length(arguments) == 1
            print(io, broadcast, string(op), "(")
            show(io, only(arguments))
            print(io, ")")
        else
            n = length(arguments)
            for (i, a) ∈ enumerate(arguments)
                isa(a, SymbolicExpression) && operation(a) ∈ infix_ops && print(io, "(")
                show(io, a)
                isa(a, SymbolicExpression) && operation(a) ∈ infix_ops && print(io, ")")
                i != n && print(io, " ", broadcast, string(op), " ")
            end
        end
    elseif op == ifelse
        p,a,b = arguments
        print(io, "𝕀(")
        show(io, p)
        print(io, ")")
    elseif op == getindex
        a, idx = arguments
        show(io, a)
        print(io, "[")
        show(io, idx)
        print(io, "]")
    else
        print(io, op, broadcast, "(")
        join(io, arguments, ", ", ", ")
        print(io, ")")

    end
end

function Base.show(io::IO, eq::SymbolicEquation)
    show(io, eq.lhs)
    print(io, " ~ ")
    show(io, eq.rhs)
end


# catch others
_show(io::IO, x) = show(io, x)

