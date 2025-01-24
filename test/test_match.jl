using Test
using SimpleExpressions
S = SimpleExpressions

import SimpleExpressions: SyntacticMatch, MatchOneToOne,
    MatchSequence,MatchCommutativeSequence
import SimpleExpressions: SymbolicVariable, SymbolicExpression
import SimpleExpressions: @symbolic_variables

@symbolic x p
@symbolic ⋯
@symbolic_variables y z a b c
@symbolic_variables x_ x__ x___ y_ y__ y___ z_ z__ z___

@symbolic_variables g() f() fₐ() fₘ() fₐₘ()
f ⨝ as = f(as...)

function S.isassociative(x::S.SymbolicFunction)
    nm = string(Symbol(x))
    endswith(nm, "ₐ") && return true
    endswith(nm, "ₐₘ") && return true
    false
end

function S.iscommutative(x::S.SymbolicFunction)
    nm = string(Symbol(x))
    endswith(nm, "ₘ") && return true
    false
end

## ----
# Main user interface are methods for `replace`, `match`, `eachmatch`
@testset "replace head" begin
    # replace operation
    ex = log(1 + x^2) + log(1 + x^3)
    @test replace(ex, log=>sin) == sin(1 + (x ^ 2)) + sin(1 + (x ^ 3))

    @symbolic_variables f() g()
    @test replace(f(a,a,b), f(x__) => g(x__)) ==  g((a,a,b)) # not g(a,a,b)
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
    @test replace(ex, cos(x + x__) => x__) ≈ₚ sin(x + (x * log(x)) + p + p + (x ^ 2))

    @test replace(x, p=>2) == x
    @test replace(1 + x^2, x^2 => 2)() == 3  # 1 + 2 evaluates to 3


    # x_ matches different parts of expression tree in replace
    ex = sin(cos(a))*cos(b)
    @test replace(ex, cos(x_) => tan(x_)) == sin(tan(a)) * tan(b)

    # no variable in substitution
    @test replace(sin(a), sin(x_) => x) == x
    @test replace(sin(a), sin(x_) => x_) == a
    @test replace(sin(a), sin(x_) => 2) == 2
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
    @test match((⋯)^(⋯), (x+p)^(x+p)) == ((⋯) => x + p,)

    # match 2 wildcards
    σ = match(x_*sin(y_), x*sin(x))
    @test (y_ => x) ∈ σ && (x_ =>x) ∈ σ && length(σ) == 2

    # match can have more than 1 substitution
    σ = match(f(x__,y__), f(a,b,c))
    @test f(x__, y__)(σ...) ∈ (f((a,b), (c,)), f((a,), (b,c)))

    # empty match returns `nothing`
    @test isnothing(match(sin(⋯), sin(x)^2))

    # eachmatch returns iterator
    sub = a + b + c
    @test isempty(eachmatch(1 + x_, sub))
    @test length(collect(eachmatch(x_ + y_, sub))) == 6 # associative
end

## -- test internal functions
@testset "exact" begin
    𝑝, 𝑠 = cos(sin(a)), cos(sin(a))
    m = SyntacticMatch(𝑠, 𝑝)
    @test m == ()

    𝑝, 𝑠 = cos(sin(a)), cos(sin(b))
    m = SyntacticMatch(𝑠, 𝑝)
    @test isnothing(m)

    m = SyntacticMatch(sin(cos(a)), cos(a))
    @test isnothing(m)
end

@testset "associative" begin
    𝑠 = 1 + a + b
    𝑝 = 1 + x_
    Θ = MatchOneToOne((𝑠,), 1 + x_)
    @test length(collect(Θ)) == 1
    σ = only(Θ)
    @test S.sorted_arguments(last(σ[1])) == (a,b)

    Θ = MatchOneToOne((a + b + c,), x__ + y__)
    @test length(collect(Θ)) == 6 # (c, a+b),(a,c+b),(b,c+a),(c+a,b),(c+b,a), (a+b,c)

    # match
    # should not match
    𝑠 = log(1 + x^2/2 - x^4/24)
    @test !isnothing(match(log(1 + ⋯), 𝑠))
    @test !isnothing(match(log(1 + x__), 𝑠)) # again x_ like x__

end

@testset "constant patterns" begin
    @test isempty(MatchSequence((a,b,c), (a,b,b)))    # no substitutions
    @test only(MatchSequence((a,b,c), (a,b,c))) == () # one trivial substitution
end

@testset "matched variables" begin

    ss, ps = (a,b,c), (x_,y_,z_)
    σ = (x_ => a,)

    ss′, ps′ = S._match_matched_variables(ss, ps, σ)
    @test ss′ == (b,c) && ps′ == (y_,z_)

    Θ = MatchCommutativeSequence(ss, ps, nothing, ((),))
    @test length(collect(Θ)) == 6
    Θ = MatchCommutativeSequence(ss, ps, nothing, (σ,))
    @test length(collect(Θ)) == 2

end


@testset "non-variable" begin
    𝑝 = fₘ(g(a,x_), g(x_,y_), g(z__))
    𝑠 = fₘ(g(a,b), g(b,a), g(a,c))
    Θ = MatchOneToOne((𝑠,), 𝑝)
    σ = only(Θ)
    @test length(σ) == 3
    @test (x_ => b) ∈ σ && (y_ => a) ∈ σ && (z__ => (a, c)) ∈ σ

end

@testset "regular variables" begin
    𝑠 = fₘ(a,a,a,b,b,c)
    𝑝 = fₘ(x_,x_,y___)
    Θ = MatchOneToOne((𝑠,), 𝑝)
    @test length(collect(Θ)) == 1 # σ =  (x_ => a, y___ => (a, b, b, c))
    @test (x_ => a, y___ => (a, b, b, c)) ∈ Θ # ordering is ok

    𝑠 = fₐₘ(a,a,a,b,b,c)
    𝑝 = fₐₘ(x_,x_,y___) # associative has x_ like x__
    Θ = MatchOneToOne((𝑠,), 𝑝)
    @test length(collect(Θ)) == 3 # (x_ => fₐₘ(a, b), y___ => fₐₘ(a, c))


end

@testset "sequence variables" begin
    @symbolic_variables u() uₐ() uₘ() uₐₘ()

    Θ = MatchSequence((a,b,c), (x__, y__), u)
    @test length(collect(Θ)) == 2 # u(a,b), u(c); u(a), u(b,c)

    Θ = MatchSequence((a,b,c), (x__, y___), u)
    @test length(collect(Θ)) == 3 # add u(a,b,c),u()

    Θ = MatchSequence((a,b,c), (x___, y___), u)
    @test length(collect(Θ)) == 4


    Θ = MatchSequence((a,b,c), (x__, y__), uₘ) # are these right
    @test length(collect(Θ)) == 2 #

    Θ = MatchSequence((a,b,c), (x__, y___), uₘ)
    @test length(collect(Θ)) == 3


    Θ = MatchSequence((a,b,c), (x___, y___), uₐₘ)
    @test length(collect(Θ)) == 4


end

