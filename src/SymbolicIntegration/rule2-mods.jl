# has trouble with
# (sub = :(f(a, a, b)), pat = :(f(~(~(~x)))))
# (sub = a + b + c, pat = :(~x + ~(~y)))
#
# modifications made
const SymsType = AbstractSymbolic
# replaced SymbolicUtils.unwrap_const with just unwrap_const
symbolic_isone(x) = isone(x)


# Does wildcard have a predicate?
has_predicate(x::Symbol)::Bool = false
function has_predicate(x::Expr)::Bool
    if x.args[1] ∈ (:~, :!)
        has_predicate(x.args[2])
    else
        length(x.args) == 2 && x.head==:(::)
    end
end


# get_predicate. Assumes user has called `has_predicate` and got TRUE
get_predicate(x::Symbol) = :nothing
function get_predicate(x::Expr)
    if x.args[1] ∈ (:~, :!)
        get_predicate(x.args[2])
    else
        x.args[2]
    end
end

eq_expr(a::Any, b::Any) = isequal(unwrap_const(a), unwrap_const(b))

varname(x::Symbol) = x
function varname(x::Expr)
    iscall(x) && !(x.args[1] ∈ (:~, :!)) && throw(ArgumentError("$x is not a wild card variable"))
    if x.args[1] ∈ (:~, :!)
        varname(x.args[2])
    else
        varname(x.args[1])
    end
end

# Expr
is_𝑋(x::Expr) = (iscall(x) && operation(x) === :(~))  ||
    ((!iscall(x) && isexpr(x)) && head(x) != :... && is_𝑋(first(x.args)))

function has_𝑋(x::Expr)
    is_𝑋(x) && return true
    !iscall(x) && return false
    is_𝑋(operation(x)) && return true
    any(has_𝑋, arguments(x))
end

has_𝑋(x::Union{SymbolicVariable,SymbolicParameter}) = is_wildcard(x)
has_𝑋(x::AbstractSymbolic) = any(has_𝑋, arguments(x))
