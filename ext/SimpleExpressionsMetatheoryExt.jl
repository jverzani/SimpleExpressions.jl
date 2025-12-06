module SimpleExtensionsMetatheoryExt

## ---------------

import Combinatorics: combinations, permutations
using Metatheory
using SimpleExpressions
import SimpleExpressions: SymbolicNumber, SymbolicParameter,
    SymbolicVariable, SymbolicExpression, AbstractSymbolic
import SimpleExpressions:
    simplify, powsimp,trigsimp,logcombine,
    expand,expand_trig,expand_power_exp, expand_log,
    canonoicalize

## ----- predicates -----
is_literal_number(::SymbolicNumber) = true
is_literal_number(::Number) = true
is_literal_number(::Any) = false

isminusone(x::Number) = x == -1
isminusone(x::SymbolicNumber) = isminusone(x())
isminusone(::AbstractSymbolic) = false

istwo(x::Number) = x == 2
istwo(x::SymbolicNumber) = istwo(x())
istwo(::AbstractSymbolic) = false

## ---- SymbolicUtils ----

#=
Lifted and modified from MIT licensed [SymbolicUtils.jl](https://github.com/JuliaSymbolics/SymbolicUtils.jl/blob/master/LICENSE.md)

[Rules](https://github.com/JuliaSymbolics/SymbolicUtils.jl/blob/master/src/rule.jl) are mostly based on rules in that package.
=#

# recreate @acrule and @ordered_acrule
struct ACRule{F,R}
    sets::F
    rule::R
    arity::Int
end

macro acrule(expr)
    arity = length(expr.args[2].args[2:end])
    quote
        ACRule(permutations, $(esc(:(@rule($(expr))))), $arity)
    end
end

macro ordered_acrule(expr)
    arity = length(expr.args[2].args[2:end])
    quote
        ACRule(combinations, $(esc(:(@rule($(expr))))), $arity)
    end
end

function (acr::ACRule)(term)
    r = acr.rule
    if !iscall(term)
        r(term)
    else
        f =  operation(term)
        # # Assume that the matcher was formed by closing over a term
        # if f != operation(r.lhs) # Maybe offer a fallback if m.term errors.
        #     return nothing
        # end

        args = arguments(term)

        itr = acr.sets(eachindex(args), acr.arity)

        for inds in itr
            result = r(f(args[inds]...)) #Term{T}(f, @views args[inds]))
            if result !== nothing
                # Assumption: inds are unique
                length(args) == length(inds) && return result
                return maketerm(typeof(term), f, [result, (args[i] for i in eachindex(args) if i ∉ inds)...], nothing) # metadata(term))
            end
        end
    end
end

## ---- some predicates used in SymbolicUtils rules
function isnotflat(⋆)
    function (x)
        args = arguments(x)
        for t in args
            if is_operation(⋆)(t)
                return true
            end
        end
        return false
    end
end

function flatten_term(⋆, x)
    args = arguments(x)
    # flatten nested ⋆
    flattened_args = []
    for t in args
        if is_operation(⋆)(t)
            append!(flattened_args, arguments(t))
        else
            push!(flattened_args, t)
        end
    end
    maketerm(SymbolicExpression, ⋆, flattened_args, metadata(x))
end

hasrepeats(::SimpleExpressions.AbstractSymbolic)= false
function hasrepeats(x′::SimpleExpressions.SymbolicExpression)
    x = SimpleExpressions.arguments(x′)
    length(x) <= 1 && return false
    for i=1:length(x)-1
        if isequal(x[i], x[i+1])
            return true
        end
    end
    return false
end

_merge_op(::typeof(*), a, b) = a^b
_merge_op(::typeof(+), a, b) = b*a
function merge_repeats(op, xs)

    length(xs) <= 1 && return xs
    merged = ()
    d = Dict{Any, Int}()
    for k in xs
        cnt = get(d, k, 0)
        d[k] = cnt + 1
    end

    return tuple((v==1 ? k : _merge_op(op, k,v) for (k,v) in pairs(d))...)

end

function has_trig_exp(term)
    !iscall(term) && return false
    fns = (sin, cos, tan, cot, sec, csc, exp, cosh, sinh)
    op = operation(term)

    if Base.@nany 9 i->fns[i] === op
        return true
    else
        return any(has_trig_exp, arguments(term))
    end
end


needs_sorting(f) = x -> is_operation(f)(x) && !issorted(arguments(x))
needs_sorting₊ = needs_sorting(+) # issue with using rhs as predicate?
needs_sortingₓ = needs_sorting(*)

function sort_args(f, t)
    args = arguments(t)
    args = merge_repeats(f, args) # had issue with hasrepeats, slipped in here
    if length(args) < 2
        return maketerm(typeof(t), f, args, metadata(t))
    elseif length(args) == 2
        x, y = args
        return maketerm(typeof(t), f, x < y ? [x,y] : [y,x], metadata(t))
    end
    args = args isa Tuple ? [args...] : args
    maketerm(typeof(t), f, TupleTools.sort(args), metadata(t))
end

# issue is ~~x
Base.view(t::NTuple{N, AbstractSymbolic}, ind::UnitRange) where {N} = t[ind]

## rules for simplification
CANONICALIZE_PLUS = [

#    @rule(~x::isnotflat(+) => flatten_term(+, ~x))

    @rule(~x::needs_sorting₊ => sort_args(+, ~x)) # also merge
    @ordered_acrule(~a::is_literal_number + ~b::is_literal_number => ~a + ~b)
    #XXX @acrule(*(~~x) + *(~β, ~~x) => *(1 + ~β, (~~x)...)) ## JUST WRONG!
    @acrule(~x + *(~β, ~x) => *(1 + ~β, ~x))
    @acrule(*(~α::is_literal_number, ~x) + ~x => *(~α + 1, ~x))
    # @rule(+(~~x::hasrepeats) => +(merge_repeats(*, ~~x)...)) # XXX p_var issue

    @ordered_acrule((~z::iszero + ~x) => ~x)
    @rule(+(~x) => ~x)

    #@rule(-(~x) => (-1) * ~x)
    @rule(-(~x, ~y) => +(~x, (-1) * ~y))

    @acrule(~x + ~c::isminusone * ~x => zero(~x))
]

CANONICALIZE_TIMES = [
#    @rule(~x::isnotflat(*) => flatten_term(*, ~x))
    @rule(~x::needs_sortingₓ => sort_args(*, ~x))

    @ordered_acrule(~a::is_literal_number * ~b::is_literal_number => ~a * ~b)
    # @rule(*(~~x::hasrepeats) => *(merge_repeats(^, ~~x)...))

    @acrule((~y)^(~n) * ~y => (~y)^(~n+1))

    @ordered_acrule((~z::isone  * ~x) => ~x)
    @ordered_acrule((~z::iszero * ~x) => ~z)

    @rule(~x / ~x => one(~x))
    @rule(*(~x,~xs...) / (~x,~ys...) => *(~xs...) / *(~ys...))
    @rule(~x / (~x, ~ys...) => one(~x) / *(~ys...))
    @rule(*(~x,~xs...) / ~x => *(~xs...))

    @acrule(~x * (~x)^(~c::isone) => one(~x))
    @rule(*(~x) => ~x)
]

CANONICALIZE_POW = [
    @rule(^(*(~~x), ~y::isinteger) => *(map(a->a^~y, ~~x)...))
    @rule((((~x)^(~p::isinteger))^(~q::isinteger)) => (~x)^((~p)*(~q)))
    @rule(^(~x, ~z::iszero) => 1)
    @rule(^(~x, ~z::isone) => ~x)
    # @rule(inv(~x) => 1/(~x))
]

PLUS_DISTRIBUTE = [
    @acrule(*(~α, ~~x) + *(~β, ~~x) => *(~α + ~β, (~~x)...))
    @acrule(*(~~x, ~α) + *(~~x, ~β) => *(~α + ~β, (~~x)...))
]


POW_RULES = [
    @rule(^(~x::isone, ~z) => 1)
    @ordered_acrule((~x)^(~a) * (~x)^(~b) => (~x)^(~a + ~b)) # always
    @ordered_acrule((~x)^(~a) * (~y)^(~a) => (~x*~y)^(~a))   # x,y ≥ 0; a real
    # (x^a)^b => x^(a*b) is in canonicalize; b \in Z
]

ASSORTED_RULES = [
    @rule(identity(~x) => ~x)
    @rule((~x::isone) \ ~y => ~y)
    @rule(~x \ ~y => ~y / (~x))
    @rule(one(~x) => 1)
    @rule(zero(~x) => 0)
    @rule(conj(~x::isreal) => ~x)
    @rule(real(~x::isreal) => ~x)
    @rule(imag(~x::isreal) => 0)
    #@rule(ifelse(~x::is_literal_number, ~y, ~z) => ~x ? ~y : ~z)
    @rule(ifelse(~x, ~y, ~y) => ~y)
]

TRIG_RULES = [
    # @acrule(~r*~x::has_trig_exp + ~r*~y => ~r*(~x + ~y))
    # @acrule(~r*~x::has_trig_exp + -1*~r*~y => ~r*(~x - ~y))
    @acrule(sin(~x)^(~c::istwo) + cos(~x)^(~c::istwo)   => one(~x))
    @acrule(sin(~x)^(~c::istwo) - (~c::isone) => -1*cos(~x)^2)
    @acrule(cos(~x)^(~c::istwo) - (~c::isone) => -1*sin(~x)^2)

    @acrule(cos(~x)^(~c::istwo) + (~d::isminusone)*sin(~x)^(~c::istwo) => cos(2 * ~x))
    @acrule(sin(~x)^(~c::istwo) + (~d::isminusone)*cos(~x)^(~c::istwo) => -cos(2 * ~x))
    @acrule(cos(~x) * sin(~x) => sin(2 * ~x)/2)

    @acrule(tan(~x)^(~c::istwo) + (~d::isminusone)*sec(~x)^(~c::istwo) => one(~x))
    @acrule((~d::isminusone)*tan(~x)^(~c::istwo) + sec(~x)^(~c::istwo) => one(~x))
    @acrule(tan(~x)^(~c::istwo) + (~d::isone)      => sec(~x)^2)
    @acrule(sec(~x)^(~c::istwo) + (~d::isminusone) => tan(~x)^2)

    @acrule(cot(~x)^(~c::istwo) + (~d::isminusone)*csc(~x)^(~c::istwo) => one(~x))
    @acrule(cot(~x)^(~c::istwo) + (~d::isone)      => csc(~x)^2)
    @acrule(csc(~x)^(~c::istwo) + (~d::isminusone) => cot(~x)^2)

    @acrule(cosh(~x)^(~c::istwo) + (~d::isminusone)*sinh(~x)^(~c::istwo) => one(~x))
    @acrule(cosh(~x)^(~c::istwo) + (~d::isminusone) => sinh(~x)^2)
    @acrule(sinh(~x)^(~c::istwo) + (~d::isone)      => cosh(~x)^2)

    @acrule(cosh(~x)^(~c::istwo) + sinh(~x)^(~c::istwo) => cosh(2 * ~x))
    @acrule(cosh(~x) * sinh(~x)                         => sinh(2 * ~x)/2)
]

EXP_RULES = [
    @acrule(exp(~x) * exp(~y) => iszero(~x + ~y) ? 1 : exp(~x + ~y))
    @rule(exp(~x)^(~y)        => exp(~x * ~y))
]

LOG_RULES = [
        @acrule(log(~x) + log(~y)   => log(~x * ~y)),
        @acrule(~n * log(~x)        => log((~x)^(~n)))
]

CANONICALIZE = CANONICALIZE_PLUS ∪ CANONICALIZE_TIMES ∪ CANONICALIZE_POW

## rules for expansion

_expand_minus = [
    @rule(-(~a + ~b) => -~a + -~b)
    @rule((~c::isone) * (+(~~xs...)) => +(~~xs...))
    @rule((~c::isminusone) * (+(~~xs...)) => +((-).(~~xs)...))
    @rule(~a - ~a => zero(~a))
    @rule(-~a + ~a => zero(~a))
]



_expand_distributive = [
    @rule(~z*(~x + ~y) => ~z*~x + ~z*~y)
    @rule((~x + ~y) * ~z => ~z*~x + ~z*~y)
    @rule(~z * (+(~~xs...))  => sum(~z*x for x in ~~xs))
    @rule(+(~~xs...) * ~z => sum(~z*x for x in ~~xs))

    @rule(~z*(~x - ~y) => ~z*~x - ~z*~y)
    @rule((~x - ~y) * ~z => ~z*~x - ~z*~y)
]

_expand_binom = [
    @rule((~x + ~y)^(~c::isone) => ~x + ~y)
    @rule((~x + ~y)^(~c::istwo) => (~x)^2 + 2*~x*~y + (~y)^2)
    @rule((~x + ~y)^(~n::isinteger) => sum(binomial(Int(~n), k) * (~x)^k * (~y)^((~n)-k) for k in 0:Int(~n)))
]


_expand_trig = [
    @rule(sin(~c::istwo * ~a) => 2sin(~a)*cos(~a))
    @rule sin(~a + ~b) => sin(~a)*cos(~b) + cos(~a)*sin(~b)
    @rule cos(~c::istwo * ~a) => cos(~a)^2 - sin(~a)^2
    @rule cos(~a + ~b) => cos(~a)*cos(~b) - sin(~a)*sin(~b)
    @rule sec(~a) => 1 / cos(~a)
    @rule csc(~a) => 1 / sin(~a)
    @rule tan(~a) => sin(~a)/cos(~a)
    @rule cot(~a) => cos(~a)/sin(~a)
]


_expand_power = [
    @rule (~x)^(~a+~b) => (~x)^(~a) * (~x)^(~b)
    @rule((~x*~y)^(~a) => (~x)^(~a) * (~y)^(~a))
]

_expand_log = [
    @rule(log(~x*~y) => log(~x) + log(~y))
    @rule log((~x)^(~n)) => ~n * log(~x)
]

_expand_misc = [
    @rule( -(~a) => (-1)*~a)
    @rule(((~c)::isone/~a) * ~a => one(~a))
    @rule(~a * ((~c)::isone/~a) => one(~a))
    #@rule(/(~a,~b) => *(~a, (~b)^(-1)))
]


# make methods for expressions
function simplify(ex::SymbolicExpression)
    ex = rewrite(ex, CANONICALIZE)
    theories = (PLUS_DISTRIBUTE,
                POW_RULES,
                ASSORTED_RULES,
                TRIG_RULES,
                EXP_RULES,
                LOG_RULES
                )
    ex = rewrite(ex, reduce(∪, theories))
    ex = rewrite(ex, CANONICALIZE)
end

function expand(ex::SymbolicExpression)
    theories =  (
        _expand_minus,
        _expand_distributive, _expand_binom, _expand_trig,
        _expand_power, _expand_log,
        _expand_misc
    )
    ex = rewrite(ex, reduce(∪,theories))
end


#
canonicalize(ex::SymbolicExpression) = rewrite(ex, CANONICALIZE)
powsimp(ex::SymbolicExpression) = rewrite(ex, CANONICALIZE ∪ POW_RULES ∪ EXP_RULES)
trigsimp(ex::SymbolicExpression) = rewrite(ex, CANONICALIZE ∪ TRIG_RULES)
logcombine(ex::SymbolicExpression) = rewrite(ex, CANONICALIZE ∪ LOG_RULES)

expand_trig(ex::SymbolicExpression) = rewrite(ex, _expand_trig)
expand_power_exp(ex::SymbolicExpression) = rewrite(ex, _expand_power)
expand_log(ex::SymbolicExpression) = rewrite(ex, _expand_log)


# function to run quickly and make terms more nice
# used by show
function _canon(x)
    rules = [
        # sort
        @rule(~x::needs_sorting₊ => sort_args(+, ~x)) # also merge
        @rule(~x::needs_sortingₓ => sort_args(*, ~x))

        # combine terms
        @rule(~x + ~x => 2x)
        @acrule(~x + *(~β, ~x) => *(1 + ~β, ~x))
        @acrule(*(~~x) + *(~β, ~~x) => *(1 + ~β, (~~x)...))
        @acrule(*(~α, ~~x) + *(~β, ~~x) => *(~α + ~β, (~~x)...))

        # additive identity
        @acrule(~z::iszero + ~x => ~x)
        @acrule(+(~z::iszero, ~~xs...) => +(~~xs...))

        # multiplicative zero
        @ordered_acrule(~z::iszero * ~x => zero(~x))
        @ordered_acrule(*(~z::iszero, ~~xs...) => zero(~z))

        # multiplicative identity
        @acrule(~z::isone * ~x => ~x)
        @acrule(*(~z::isone, ~~xs...) => *(~~xs...))


    ]

    rewrite(x, rules)
end

end
