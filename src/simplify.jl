# simplify and expand
simplify(ex) = __resolve(ex, simplify_rules)
expand(ex)   = __resolve(ex, expand_rules)

# useful functions for pattern/replacement-rule writing
Σ(x) = isempty(x) ? 0 : sum(x)
Π(x) = isempty(x) ? 1 : prod(x)
scalar_mult(c, x) = c .* x
integer_gt(n) = i -> isinteger(i) && i >= n
function contains_var(xs...)::Bool
    n = length(xs)
    x = xs[end]
    for i in 1:(n-1)
        contains(xs[i],x) && return true
    end
    return false
end
function unwrapped_compare(op, a, b)::Bool
    a′ = unwrap_const(a)
    b′ = unwrap_const(b)
    (isconstant(a′) && isconstant(b′)) || return false
    op(a, b)
end
lt(a, b) = unwrapped_compare(<,  a, b)
le(a, b) = unwrapped_compare(<=, a, b)
eq(a, b) = unwrapped_compare(==,  a, b)
ge(a, b) = unwrapped_compare(>=, a, b)
gt(a, b) = unwrapped_compare(>,  a, b)

## ------- rules to apply
canonicalize = [
    @rule(~a + (~b + ~c)              => +(~a,~b,~c)),
    @rule(~a * (~b * ~c)              => *(~a,~b,~c)),
    @rule(~a - ~a                     => zero(~a)),
    @rule(*(~~~a) + *(-1, ~~~a) + ~~b => Σ(~~b)),

    @rule(*(~!a, ~~~x) + *(~!b, ~~~x) + (~~c) => *(~a + ~b, Π(~~~x)) + Σ(~~c)),
    @rule(*(~~~a, ~x)  + *(~~b, ~x)   + (~~c) => *(Π(~~~a) + Π(~~b), ~x) + Σ(~~c)),

    @rule((~x)^(~z::iszero)       => one(~x)),
    @rule((~x)^(~z::isone)        => ~x),
    @rule((~x::isone)^(~z)        => one(~x)),
    @rule(sqrt(~x)                => (~x)^(1//2)),
    @rule(cbrt(~x)                => (~x)^(1//3)),
    @rule(ℯ^(~z)                  => exp(~z)),
    @rule(exp(~z::iszero)         => 1),
    @rule(exp(~z::isone)          => ℯ),

    @rule(sin(~x)/cos(~x) => tan(~x)),
    @rule(sin(~x)*cot(~x) => cos(~x)),
    @rule(cos(~x)/sin(~x) => cot(~x)),
    @rule(cos(~x)*cot(~x) => sin(~x)),

]

# https://docs.sympy.org/latest/tutorials/intro-tutorial/simplification.html
powsimp = [
    @rule((~x)^(~!m) * (~x)^(~n) * (~~a) => Π(~~a) * (~x)^(~m + ~n)),
    @rule((~x)^(~!m) * (~y)^(~m) * (~~a) => Π(~~a) * (~x*~y)^(~m)), # needs x,y > 0
    @rule(((~x)^(~m))^(~n)               => (~x)^(~m*~n)),
]

expsimp = [
    @rule((~~a) * exp(~x) * exp(~y) => Π(~~a) * exp(~x + ~y)),
    @rule(exp(~x)^(~y)              => exp(~x * ~y)),
    @rule(exp(~!a*log(~x))          => (~x)^(~!a)),
    @rule(exp(~x::iszero)           => one(~x)),
]

logsimp = [
    @rule((~!a)*log(~x) + (~!a)*log(~y) + (~~b)    => (~!a) * log(~x*~y) + Σ(~~b)),
    @rule((~n)* log(~x)             => log((~x)^(~n))),
    @rule(log(~!a*exp(~x))          => log(~!a) + ~x),
    @rule(log(~x::isone)            => zero(~x)),
]


iseveninteger(x) = isinteger(x) && iseven(x)
trigsimp = [
    @rule((~!a) * sin(~x)^2 + (~!a) * cos(~x)^2 + ~~b          => ~a + Σ(~~b)),
    @rule((~!a) * sinh(~x)^2 + (~!a) * cosh(~x)^2 + ~~b        => ~a*cosh(2*~x) + Σ(~~b)),
    @rule((~!a) * cos(~x)^2 + (-1) *  (~!a) * sin(~x)^2 + ~~b  => ~a * cos(2*~x) + Σ(~~b)),
    @rule((~!a) * cosh(~x)^2 + (~!a) * sinh(~x)^2 + ~~b        => ~a * cosh(2*~x) + Σ(~~b)),
    @rule((~!a) * sin(~x)*cos(~y) + (~!a) * sin(~y)*cos(~x) + ~~b => (~!a) * sin(~x + ~y) + Σ(~~b)),
    @rule((~!a) * sinh(~x)*cosh(~y) + (~!a) * sinh(~y)*cosh(~x) + ~~b => (~!a) * sinh(~x + ~y) + Σ(~~b)),

    @rule((~!a) * cos(~x)*cos(~y) + (-1) * (~!a) * sin(~x)*sin(~y) + ~~b => (~!a) * cos(~x + ~y) + Σ(~~b)),
    @rule((~!a) * cosh(~x)*cosh(~y) + (~!a) * sinh(~y)*sinh(~x) + ~~b    => (~!a) * cosh(~x + ~y) + Σ(~~b)),

    @rule((~!a) * (~m::iseven)*sin(~x)*cos(~x)   => (~!a) * (~m/2) * sin(2*~x)),
    @rule((~!a) * (~m::iseven)*sinh(~x)*cosh(~x) => (~!a) * (~m/2) * sinh(2*~x)),

    @rule(sin(0) => 0),
    @rule(sin((~!n::isinteger)*pi) => 0),
    @rule(sin(pi/2) => 1),
    @rule(sin((~!n::isinteger)*pi/2) => 0),
    @rule(cos(0) => 1),
    @rule(cos((~!n::isinteger)*pi) => 1),
    @rule(cos(pi/2) => 1),
    @rule(cos((~!n::iseveninteger)*pi/2) => 1*isodd(~n÷2)),

]

## --- expand
expand_canonicalize = [
    @rule(*(~!a, +(~~~b))         => sum(scalar_mult(~!a, ~~~b))),
    @rule(*(~a + ~b,~x)           => *(~a, ~x) + *(~b, ~x)),
    @rule(+(~a, ~~b) * +(~c, ~~d) * ~~~e => (~a*~c + ~a*Σ(~~d) + ~c*Σ(~~b) + Σ(~~b) * Σ(~~d))*Π(~~e)),

    @rule((~x)^(~n::integer_gt(2))=> ~x * (~x)^(~n-1)),
    @rule((~x)^(~z::iszero)       => one(~x)),
    @rule((~x)^(~z::isone)        => ~x),
    @rule((~x::isone)^~z          => one(~x)),
    @rule((~x)^(1//2)             => sqrt(~x)),
    @rule((~x)^(1//3)             => cbrt(~x)),

    @rule(ℯ^(~z)                  => exp(~z)),
    @rule(exp(~z::iszero)         => 1),
    @rule(exp(~z::isone)          => ℯ),


]

expand_pow = [
    @rule((~x)^(~m + ~n) => (~x)^(~!m) * (~x)^(~n)),
    @rule((~x*~y)^(~m) => (~x)^(~!m) * (~y)^(~m)),
    @rule((~x)^(~m*~n) => ((~x)^(~m))^(~n))
]

expand_exp = [
    @rule(exp(~x + ~y) => exp(~x) * exp(~y)),
    @rule(ℯ^(~x + ~y)  => exp(~x) * exp(~y)),
    @rule(exp(~x * ~y) => exp(~x)^(~y)),
    @rule(ℯ^(~x * ~y)  => exp(~x)^(~y))
]

expand_log = [
    @rule(log(~x * ~y)   => log(~x) + log(~y)),
    @rule(log((~x) ^ ~n) => ~n * log(~x))
]

expand_trig = [
    @rule(tan(~x)       => sin(~x) / cos(~x)),
    @rule(cot(~x)       => cos(~x) / sin(~x)),
    @rule(sec(~x)       => 1/cos(~x)),
    @rule(csc(~x)       => 1/sin(~x)),

    @rule(sin(2 * ~x)   => 2*sin(~x)*cos(~x)),
    @rule(cos(2 * ~x)   => cos(~x) ^ 2 - sin(~x) ^ 2),

    @rule(sin(~n::integer_gt(3) * ~x) => 2*cos(~x)*sin((~n-1)*~x) - sin((~n-2)*~x)),
    @rule(cos(~n::integer_gt(3) * ~x) => 2*cos(~x)*cos((~n-1)*~x) - cos((~n-2)*~x)),

    @rule(sinh(2 * ~x)  => 2*sinh(~x)*cosh(~x)),
    @rule(cosh(2 * ~x)  => sinh(~x) ^ 2 + cosh(~x) ^ 2),

    @rule(sin(~x + ~y)  => sin(~x) * cos(~y) + sin(~y) * cos(~x)),
    @rule(cos(~x + ~y)  => cos(~x) * cos(~y) - sin(~x) * sin(~y)),

    @rule(sinh(~x + ~y) => sinh(~x) * cosh(~y) + sinh(~y) * cosh(~x)),
    @rule(cosh(~x + ~y) => cosh(~x) * cosh(~y) + sinh(~y) * sinh(~x))
]

const simplify_rules = vcat(canonicalize, powsimp, expsimp, logsimp,
                            trigsimp)
const expand_rules = vcat(expand_canonicalize, expand_pow, expand_exp,
                          expand_log, expand_trig)

## -----------------------------------------------------##
function walk(ex, inner, outer)
    (!isvariable(ex) && (iscall(ex) || isexpr(ex))) || return outer(ex)
    if isexpr(ex) && !iscall(ex)
        ex′ = Expr(head(ex), map(inner, children(ex))...)
    elseif isexpr(ex)
        ex′ = maketerm(AbstractSymbolic, operation(ex), map(inner, arguments(ex)), nothing)
    end
    outer(ex′)
end

postwalk(f, ex) = walk(ex,    x -> postwalk(f,x), f)
prewalk(f, ex)  = walk(f(ex), x -> prewalk(f, x), identity)

# apply rules to expression
__apply_rules(x::Number, rs) = x
__apply_rules(x::Bool, rs) = x
function __apply_rules(x, rs)
    for r ∈ rs
        ex = r(x)
        isnothing(ex) || ex == x || return ex
    end
    return x
end

# Apply `rs` bottom-up repeatedly until the expression stops changing.
# Rules can cycle (e.g. a rule and its reverse), so revisiting an earlier
# expression also ends the iteration, as does the `maxiter` safeguard.
function __resolve(ex, rs; maxiter=1000)
    seen = Any[]
    for _ in 1:maxiter
        iscall(ex) || return ex
        ex′ = postwalk(x -> __apply_rules(x, rs), ex)
        (isnothing(ex′) || isequal(ex′, ex)) && return ex
        any(y -> isequal(y, ex′), seen) && return ex′
        push!(seen, ex)
        ex = ex′
    end
    return ex
end
