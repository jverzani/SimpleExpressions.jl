
## ----- show
# show (Fix parens)
Base.show(io::IO, ::MIME"text/plain", x::AbstractSymbolic) = show(io, x)
Base.show(io::IO, x::AbstractSymbolic) = _show(io, ↓(x))

_show(io::IO, u::DynamicVariable) = print(io, u.sym)
_show(io::IO, u::StaticVariable{S}) where {S} = print(io, S)
_show(io::IO, u::DynamicConstant) = print(io, u.value)

Base.show(io::IO, x::SymbolicCall) = _showcall(io, x)

function _showcall(io::IO, x)
    broadcast = ""
    op, args = operation(x), arguments(x)
    if op == Base.broadcasted
        broadcast= "."
        op′, args... = args
        op = ↓(op′).value
    end

    if x isa SymbolicProd && op === (^) && (e = x.coefs[1]) isa Real && e < 0
        d = _display_division((SymbolicNumber(1), materialize(x)))
        d === nothing || return _showcall(io, d)
    end
    if op === (*) && length(args) > 1
        d = _display_division(args)
        d === nothing || return _showcall(io, d)
    end

    infix_ops = (+, - , *, /, //, ^, >=, >, ==, !=, <, <=) # infix
    if op ∈ infix_ops
        if length(args) == 1
            print(io, broadcast, string(op), "(")
            show(io, only(args))
            print(io, ")")
        else
            n = length(args)
            for (i, a) ∈ enumerate(args)
                parens = _infix_parens(a, op, i, infix_ops)
                parens && print(io, "(")
                show(io, a)
                parens && print(io, ")")
                i != n && print(io, " ", broadcast, string(op), " ")
            end
        end
    elseif op == ifelse
        p,a,b = args
        print(io, "𝕀(")
        show(io, p)
        print(io, ")")
    elseif op == getindex
        a, idx = args
        show(io, a)
        print(io, "[")
        show(io, idx)
        print(io, "]")
    else
        print(io, op, broadcast, "(")
        join(io, args, ", ", ", ")
        print(io, ")")

    end
end

# does argument `a` (in position `i`) of the infix operator `op` need parentheses?
# Compound expressions do; so do numbers whose printed form isn't atomic:
# `x ^ 1//2` parses as `(x ^ 1) // 2` and `-2 ^ x` as `-(2 ^ x)`
function _infix_parens(a, op, i, infix_ops)
    isa(a, SymbolicExpression) && operation(a) ∈ infix_ops && return true
    v = _num(a)
    v isa Complex && return true
    v isa Rational && op ∈ (*, /, //, ^) && return true
    op === (^) && i == 1 && v isa Real && v < 0 && return true
    false
end

function Base.show(io::IO, eq::SymbolicEquation)
    show(io, eq.lhs)
    print(io, " ~ ")
    show(io, eq.rhs)
end


# catch others
_show(io::IO, x) = show(io, x)
