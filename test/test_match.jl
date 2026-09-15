using Test
using SimpleExpressions
S = SimpleExpressions

# the `matchpy.jl` code was moved into AssociativeCommutativePatternMatching
# this file has been significantly modified to only test the match, replace
# interface

import SimpleExpressions: SymbolicVariable, SymbolicExpression
import SimpleExpressions: @symbolic_variables

@symbolic x p
@symbolic ⋯
@symbolic_variables y z a b c
@symbolic_variables x_ x__ x___ y_ y__ y___ z_ z__ z___

@symbolic_variables g() f() fₐ() fₘ() fₐₘ()
f ⨝ as = f(as...)

## ----
# Main user interface are methods for `replace`, `match`
@testset "replace head" begin
    # replace operation
    ex = log(1 + x^2) + log(1 + x^3)
    @test replace(ex, log=>sin) == sin(1 + (x ^ 2)) + sin(1 + (x ^ 3))

    @symbolic_variables f() g()
    @test_broken replace(f(a,a,b), f(x__) => g(x__)) ==  g((a,a,b)) # not g(a,a,b); XXX issue with this match
end

@testset "replace" begin
    # with wildcards
    ≈ₑ(u,v) = (x₀ = rand(); u(x₀) ≈ v(x₀))
    ≈ₚ(u,v) = (x₀ = rand(); p₀ = rand(); u(x₀, p₀) ≈ v(x₀, p₀))


    # replace parts
    ex = log(1 + x^2) + log(1 + x^3)
    @test replace(ex, log(1+x__) => log1p(x__)) == log1p(x ^ 2) + log1p(x ^ 3)

    ex = log(sin(x)) + tan(sin(x^2))
    @test replace(ex, sin => cos) == log(cos(x)) + tan(cos(x^2))
    @test replace(ex, sin(⋯) => tan(⋯)) == log(tan(x)) + tan(tan(x^2))
    @test replace(ex, sin(⋯) => tan((⋯)/2)) == log(tan(x/2)) + tan(tan(x^2/2))
    @test replace(ex, sin(⋯) => ⋯) == log(x) + tan(x^2)

    ex = (1 + x^2)^2 # outer one is peeled off first by replace
    pr = (⋯)^2 => (⋯)^4
    @test replace(ex, pr) == (1 + (x ^ 2)) ^ 4
    @test replace(ex, pr, pr) == (1 + (x ^ 4)) ^ 4


    ex = sin(x + x*log(x) + cos(p + x + p + x^2))
    @test_broken replace(ex, cos(x + x__) => x__) ≈ₚ sin(x + (x * log(x)) + p + p + (x ^ 2)) # XXX match x + x__

    @test replace(x, p=>2) == x
    @test replace(1 + x^2, x^2 => 2)() == 3  # 1 + 2 evaluates to 3


    # x_ matches different parts of expression tree in replace
    ex = sin(cos(a))*cos(b)
    @test replace(ex, cos(x_) => tan(x_)) == sin(tan(a)) * tan(b)

    # no variable in substitution
    @test replace(sin(a), sin(x_) => x) == x
    @test replace(sin(a), sin(x_) => x_) == a
    @test replace(sin(a), sin(x_) => 2) == 2

    # match with expressions
    @test replace(sin(a), :(sin(~x)) => :(y)) == y
    @test replace(sin(a), :(sin(~x)) => :(~x)) == a

    ex = log(sin(x)) + tan(sin(x^2))
    @test replace(ex, :(sin(~x)) => :(tan(~x))) == log(tan(x)) + tan(tan(x^2))
    @test replace(ex, :(sin((~x)^2)) => :(tan(x))) == log(sin(x)) + tan(tan(x))
    @test replace(ex, :(sin(~x)) => :(~x)) == log(x) + tan(x^2)

    ex = (1 + x^2)^2 # outer one is peeled off first by replace
    pr = :((~x)^2) => :((~x)^4)
    @test replace(ex, pr) == (1 + (x ^ 2)) ^ 4
    @test replace(replace(ex, pr), pr) == (1 + (x ^ 4)) ^ 4 # XXX replace(ex,pr,pr) not defined

    ## expressions with defslots
    @test replace(x^2, :(~!a * (~x)^(~n)) => :(~a * (~x)^(~n+1)/(~n + 1))) == x^3/3

    ## expression with defslots and predicates
    replace(x^(2), :(~!a * (~x)^(~n::(!=(-1)))) => :(~a * (~x)^(~n+1)/(~n + 1))) == x^3/3
    replace(x^(-1), :(~!a * (~x)^(~n::(!=(-1)))) => :(~a * (~x)^(~n+1)/(~n + 1))) == x^(-1)

end

@testset "replace exact" begin
    # no wild card
    ex = x^2 + x^4
    @test replace(ex, x^2 => x) == x + x^4

    ex = x * sin(x)
    @test replace(ex, x*sin(x) => x) == x
    @test replace(ex*cos(x), x*sin(x) => x) == ex * cos(x)

end

@testset "match" begin

    # match 1
    @test match((x_)^(x_), (x+p)^(x+p)) == SimpleExpressions.MatchDict(:x, x + p)

    # match 2 wildcards
    σ = match(x_*sin(y_), x*sin(x))
    @test σ[:y] == x
    @test σ[:x] == x
    @test length(σ) == 2

    # match can have more than 1 substitution
    σ = match(f(x__,y__), f(a,b,c))
    @test_broken f(x__, y__)(σ...) ∈ (f((a,b), (c,)), f((a,), (b,c))) # XXX this fails

    # empty match returns FAIL_DICT, was `nothing`
    @test match(sin(⋯), sin(x)^2) == SimpleExpressions.FAIL_DICT

end
