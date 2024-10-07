module SimpleExpressionsMetatheoryExt

import SimpleExpressions
import SimpleExpressions: SymbolicNumber
import SimpleExpressions: permutations, combinations

using Metatheory
using Metatheory.Library

# Modified from MIT licensed SymbolicUtils.jl
# https://github.com/JuliaSymbolics/SymbolicUtils.jl/blob/master/src/rule.jl
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

function SimpleExpressions.expand(ex::SimpleExpressions.SymbolicExpression)

    _expand_minus = @theory a b xs begin
        -(a + b) => -a + -b
        -1*(+(xs...)) => +(-xs...)
        a - a => 0
        -a + a => 0
    end



    _expand_distributive = @theory x y z xs ys begin
        z*(x + y) => z*x + z*y
        (x + y) * z => z*x + z*y
        # z * (+(xs...))  => sum(z*x for x in xs)
        # +(xs...) * z -->

        z*(x - y) => z*x - z*y
        (x - y) * z => z*x - z*y

    end

    _expand_binom = @theory x y n begin
        (x + y)^1 => x + y
        (x + y)^2 => x^2 + 2*x*y + y^2
        (x + y)^n::isinteger => sum(binomial(Int(n), k) * x^k * y^(n-k) for k in 0:Int(n))
    end

    _expand_trig = @theory a b begin
        sin(2a) => 2sin(a)*cos(a)
        sin(a + b) => sin(a)*cos(b) + cos(a)*sin(b)
        cos(2a) => cos(a)^2 - sin(a)^2
        cos(a + b) => cos(a)*cos(b) - sin(a)*sin(b)
        sec(a) => 1 / cos(a)
        csc(a) => 1 / sin(a)
        tan(a) => sin(a)/cos(a)
        cot(a) => cos(a)/sin(a)
    end


    _expand_power = @theory x y a b begin
        x^(a+b) => x^a*x^b
        (x*y)^a => x^a * y^a
    end
    _expand_log = @theory x y n begin
        log(x*y) => log(x) + log(y)
        log(x^n) => n * log(x)
    end

    _expand_misc = @theory a b begin
        -a => (-1)*a
        (1/a) * a => 1
        a * (1/a) => 1
        /(a,b) => *(a, b^(-1))
    end

    t =  reduce(∪, (
        _expand_minus,
        _expand_distributive, _expand_binom, _expand_trig,
        _expand_power, _expand_log,
        _expand_misc))

    Metatheory.rewrite(ex, t)
end


function SimpleExpressions.simplify(ex::SimpleExpressions.SymbolicExpression)

    PLUS_DISTRIBUTE = [
        @acrule(*(~α, ~~x) + *(~β, ~~x) => *(~α + ~β, (~~x)...))
        @acrule(*(~~x, ~α) + *(~~x, ~β) => *(~α + ~β, (~~x)...))
    ]

    CANONICALIZE_TIMES = [
        #@rule(~x::isnotflat(*) => flatten_term(*, ~x))
        #@rule(~x::needs_sorting(*) => sort_args(*, ~x))

        #        @ordered_acrule(~a::is_literal_number * ~b::is_literal_number => ~a * ~b)
        #        @rule(*(~~x::hasrepeats) => *(merge_repeats(^, ~~x)...))

        @acrule((~y)^(~n) * ~y => (~y)^(~n+1))

        @ordered_acrule((~z::isone  * ~x) => ~x)
        @ordered_acrule((~z::iszero *  ~x) => ~z)
        @rule(*(~x) => ~x)
    ]

    MUL_DISTRIBUTE = @ordered_acrule((~x)^(~n) * (~x)^(~m) => (~x)^(~n + ~m))

    CANONICALIZE_POW = [
        @rule(^(*(~~x), ~y::isinteger) => *(map(a->pow(a, ~y), ~~x)...))
        @rule((((~x)^(~p::isinteger))^(~q::isinteger)) => (~x)^((~p)*(~q)))
        @rule(^(~x, ~z::iszero) => 1)
        @rule(^(~x, ~z::isone) => ~x)
        @rule(inv(~x) => 1/(~x))
    ]

    POW_RULES = [
        @rule(^(~x::isone, ~z) => 1)
    ]

    ASSORTED_RULES = [
        @rule(identity(~x) => ~x)
        @rule(-(~x) => -1*~x)
        @rule(-(~x, ~y) => ~x + -1(~y))
        @rule(~x::isone \ ~y => ~y)
        @rule(~x \ ~y => ~y / (~x))
        @rule(one(~x) => 1) #one(symtype(~x)))
        @rule(zero(~x) => 0) #zero(symtype(~x)))
        @rule(conj(~x::isreal) => ~x)
        @rule(real(~x::isreal) => ~x)
        @rule(imag(~x::isreal) => 0)#zero(symtype(~x)))
                                #        @rule(ifelse(~x::is_literal_number, ~y, ~z) => ~x ? ~y : ~z)
        @rule(ifelse(~x, ~y, ~y) => ~y)
    ]

    TRIG_EXP_RULES = [
 #       @acrule(~r*~x::has_trig_exp + ~r*~y => ~r*(~x + ~y))
 #       @acrule(~r*~x::has_trig_exp + -1*~r*~y => ~r*(~x - ~y))
        @acrule(sin(~x)^2 + cos(~x)^2 => one(~x))
        @acrule(sin(~x)^2 + -1        => -1*cos(~x)^2)
        @acrule(cos(~x)^2 + -1        => -1*sin(~x)^2)

        @acrule(cos(~x)^2 + -1*sin(~x)^2 => cos(2 * ~x))
        @acrule(sin(~x)^2 + -1*cos(~x)^2 => -cos(2 * ~x))
        @acrule(cos(~x) * sin(~x) => sin(2 * ~x)/2)

        @acrule(tan(~x)^2 + -1*sec(~x)^2 => one(~x))
        @acrule(-1*tan(~x)^2 + sec(~x)^2 => one(~x))
        @acrule(tan(~x)^2 +  1 => sec(~x)^2)
        @acrule(sec(~x)^2 + -1 => tan(~x)^2)

        @acrule(cot(~x)^2 + -1*csc(~x)^2 => one(~x))
        @acrule(cot(~x)^2 +  1 => csc(~x)^2)
        @acrule(csc(~x)^2 + -1 => cot(~x)^2)

        @acrule(cosh(~x)^2 + -1*sinh(~x)^2 => one(~x))
        @acrule(cosh(~x)^2 + -1            => sinh(~x)^2)
        @acrule(sinh(~x)^2 +  1            => cosh(~x)^2)

        @acrule(cosh(~x)^2 + sinh(~x)^2 => cosh(2 * ~x))
        @acrule(cosh(~x) * sinh(~x) => sinh(2 * ~x)/2)

        @acrule(exp(~x) * exp(~y) => _iszero(~x + ~y) ? 1 : exp(~x + ~y))
        @rule(exp(~x)^(~y) => exp(~x * ~y))
    ]

              t = vcat(PLUS_DISTRIBUTE,
                       MUL_DISTRIBUTE,
                       CANONICALIZE_POW,
                       POW_RULES,
                       ASSORTED_RULES,
                       TRIG_EXP_RULES)

              rewrite(ex, t)

end

end
