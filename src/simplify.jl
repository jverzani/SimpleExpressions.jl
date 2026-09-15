# simplify and expand
simplify(ex) = __resolve(ex, simplify_rules)
expand(ex)   = __resolve(ex, expand_rules)

## ------- rules to apply
canonicalize = [
    :(*(~a, ~x) + *(~b, ~x) + (~!c)) => :(*(~a + ~b, ~x) + ~c),
    :(~a + (~b + ~c))          => :(+(~a,~b,~c)),
    :(~a * (~b * ~c))          => :(*(~a,~b,~c)),
    :(~a - ~a)                 => :(zero(~a)),
    :((~x)^(~z::iszero))       => :(one(~x)),
    :((~x)^(~z::isone))        => :(~x),
    :((~x::isone)^~z)          => :(one(~x)),
    :(sqrt(~x))                => :((~x)^(1//2)),
    :(cbrt(~x))                => :((~x)^(1//3)),
    #        :(ℯ^(~z)) => :(exp(~x)),
    :(exp(~z::iszero))         => 1,
    :(exp(~z::isone))          => ℯ,

    :(sin(~x)/cos(~x)) => :(tan(~x)),
    :(sin(~x)*cot(~x)) => :(cos(~x)),
    :(cos(~x)/sin(~x)) => :(cot(~x)),
    :(cos(~x)*cot(~x)) => :(sin(~x)),

]

canonicalize_expand = [
    :(*(~a + ~b,~x))           => :(*(~a, ~x) + *(~b, ~x)),
    :((~x)^(~z::iszero))       => :(one(~x)),
    :((~x)^(~z::isone))        => :(~x),
    :((~x::isone)^~z)          => :(one(~x)),

    :((~x)^(1//2))             => :(sqrt(~x)),
    :((~x)^(1//3))             => :(cbrt(~x)),

    :(ℯ^(~z)) => :(exp(~z)),
    :(exp(~z::iszero))         => 1,
    :(exp(~z::isone))          => ℯ,

    :(sin(~x)/cos(~x)) => :(tan(~x)),
    :(sin(~x)*cot(~x)) => :(cos(~x)),
    :(cos(~x)/sin(~x)) => :(cot(~x)),
    :(cos(~x)*cot(~x)) => :(sin(~x)),

]


# https://docs.sympy.org/latest/tutorials/intro-tutorial/simplification.html
powsimp = [
    :((~x)^(~!m) * (~x)^(~n) * (~!a)) => :((~a) * (~x)^(~m + ~n)),
    :((~x)^(~!m) * (~y)^(~m) * (~!a)) => :((~a) * (~x*~y)^(~m)), # needs x,y > 0
    :(((~x)^(~m))^(~n))       => :((~x)^(~m*~n)),
]
expand_pow = reverse.(powsimp)

expsimp = [
    :((~!a) * exp(~x) * exp(~y)) => :((~!a) * exp(~x + ~y)),
    :(exp(~x)^(~y))      => :(exp(~x * ~y))
]
expand_exp = reverse.(expsimp)

logsimp = [
    :((~!a)*log(~x) + (~!a)*log(~y) + (~!b))    => :((~!a) * log(~x*~y) + (~!b)),
    :((~n)* log(~x))                    => :(log((~x)^(~n))),
]
expand_log = reverse.(logsimp)

trigsimp = [
    :((~!a) * sin(~x)^2 + (~!a) * cos(~x)^2 + ~!b) => :(~a + ~!b),
    :((~!a) * sinh(~x)^2 + (~!a) * cosh(~x)^2) => :(~a*cosh(2*~x)),


    :((~!a) * cos(~x)^2 - (~!a) * sin(~x)^2)   => :(~a * cos(2*~x)),
    :((~!a) * cosh(~x)^2 + (~!a) * sinh(~x)^2) => :(~a * cosh(2*~x)),


    :((~!a) * sin(~x)*cos(~y) + (~!a) * sin(~y)*cos(~x))     => :((~!a) * sin(~x + ~y)),
    :((~!a) * sinh(~x)*cosh(~y) + (~!a) * sinh(~y)*cosh(~x)) => :((~!a) * sinh(~x + ~y)),

    :((~!a) * cos(~x)*cos(~y) - (~!a) * sin(~y)*sin(~x))     => :((~!a) * cos(~x + ~y)),
    :((~!a) * cosh(~x)*cosh(~y) + (~!a) * sinh(~y)*sinh(~x)) => :((~!a) * cosh(~x + ~y)),
]
expand_trig = reverse.(trigsimp)

trigsimpa = [
    :((~!a) * (~m::iseven)*sin(~x)*cos(~x))   => :((~!a) * div(unwrap_const(~m),2)*sin(2*~x)),
    :((~!a) * (~m::iseven)*sinh(~x)*cosh(~x)) => :((~!a) * div(unwrap_const(~m),2)*sinh(2*~x)),

    :((~!a) * cos(~x)^2  + (~!a) * sin(~x)^2)   => :(~a),
    :((~!a) * cosh(~x)^2 - (~!a) * sinh(~x)^2)  => :(~a),
]

const simplify_rules = vcat(canonicalize, powsimp, expsimp, logsimp,
                      trigsimp, trigsimpa)
const expand_rules = vcat(canonicalize, expand_pow, expand_exp, expand_trig)

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
function __apply_rules(x, rs)
    for r ∈ rs
        pat, rhs = r
        σ = match(pat, x)
        if σ != FAIL_DICT
            ex =  rewrite(σ, rhs)
            return ex
        end
    end
    return x
end

function __resolve(ex, rs)
    n = 1
    while n < 10
        !iscall(ex) && break
        ex′ = postwalk(x -> __apply_rules(x, rs), ex)
        isnothing(ex′) && return ex
        isequal(ex′, ex) && return ex
        ex = ex′
        n += 1
    end
    return ex
end


#=

## ----- Interface
"""
    simplify(ex)

Simplify expression using `Metatheory.jl` and rules on loan from `SymbolicUtils.jl`.
"""
function simplify()
end

"""
    expand(ex)

Expand terms in an expression using `Metatheory.jl`
"""
function expand()
end

# some default definitions
# we extend to SymbolicExpression in the Metatheroy extension
for fn ∈ (:simplify, :expand,
          :canonicalize, :powsimp, :trigsimp, :logcombine,
          :expand_trig, :expand_power_exp, :expand_log)
    @eval begin
        $fn(ex::AbstractSymbolic) = ex
        $fn(eq::SymbolicEquation) = SymbolicEquation($fn.(eq)...)
    end
end
=#
