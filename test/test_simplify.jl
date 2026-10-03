using SimpleExpressions
using Test

import SimpleExpressions: simplify, expand, @symbolic_variables
import SimpleExpressions: canonicalize,
    powsimp, expsimp, logsimp, trigsimp, trigsimpa,
    expand_canonicalize,
    expand_pow, expand_exp, expand_log, expand_trig

@symbolic_variables x y z a b c

# apply a single rule set (not the full pipeline) to an expression
apply(ex, rules) = SimpleExpressions.__resolve(ex, rules)

@testset "simplify" begin

    @testset "canonicalize: combine like terms" begin
        @test simplify(2x + 3x + 2) == 5x + 2
        @test simplify(2x + 3x) == 5x
        @test simplify(2x + 3x + y) == 5x + y
        @test simplify(x + x) == 2x
        @test apply(2x + 3x + 2, canonicalize) == 5x + 2
    end

    @testset "canonicalize: flatten" begin
        @test simplify(a + b + (c + x)) == a + b + c + x
        @test simplify(a * (b * c)) == a * b * c
        @test apply(a + (b + c), canonicalize) == a + b + c
        @test apply(a * (b * c), canonicalize) == a * b * c
    end

    @testset "canonicalize: cancellation" begin
        @test iszero(simplify(x - x))
        @test iszero(simplify(sin(x) - sin(x)))
    end

    @testset "canonicalize: powers" begin
        @test simplify(x^0) == 1
        @test simplify(sin(x)^0) == 1
        @test simplify(x^1) == x
        @test simplify(sin(x)^1) == sin(x)
        @test simplify(1^y) == 1
        @test simplify(sqrt(x)) == x^(1//2)
        @test simplify(cbrt(x)) == x^(1//3)
    end

    @testset "canonicalize: exp" begin
        @test simplify(ℯ^x) == exp(x)
        @test simplify(exp(0 * x)) == 1
    end

    @testset "canonicalize: trig quotients" begin
        @test simplify(sin(x) / cos(x)) == tan(x)
        @test simplify(sin(x) * cot(x)) == cos(x)
        @test simplify(cos(x) / sin(x)) == cot(x)
        @test simplify(cos(x) * cot(x)) == sin(x)
        @test simplify(sin(x + y) / cos(x + y)) == tan(x + y)
    end

    @testset "powsimp" begin
        @test simplify(x^2 * x^3) == x^5
        @test simplify(x^a * x^b) == x^(a + b)
        @test simplify(z * x^2 * x^3) == z * x^5
        @test simplify(x^2 * y^2) == (x * y)^2       # same exponent
        @test simplify(z * x^2 * y^2) == z * (x * y)^2
        @test simplify((x^2)^3) == x^6
        @test simplify((x^a)^b) == x^(a * b)
    end

    @testset "expsimp" begin
        @test simplify(exp(x) * exp(y)) == exp(x + y)
        @test simplify(z * exp(x) * exp(y)) == z * exp(x + y)
        @test simplify(exp(x)^y) == exp(x * y)
    end

    @testset "logsimp" begin
        @test simplify(2log(x) + 2log(y)) == log((x * y)^2)
        @test simplify(y * log(x)) == log(x^y)
        @test simplify(2 * log(x)) == log(x^2)
    end

    @testset "trigsimp: Pythagorean" begin
        @test simplify(sin(x)^2 + cos(x)^2) == 1
        @test simplify(2sin(x)^2 + 2cos(x)^2) == 2
        @test simplify(sinh(x)^2 + cosh(x)^2) == cosh(2x)
        @test simplify(cosh(x)^2 + sinh(x)^2) == cosh(2x)
    end

    @testset "trigsimp: double angle" begin
        @test simplify(cos(x)^2 - sin(x)^2) == cos(2x)
    end

    @testset "trigsimp: angle sums" begin
        @test simplify(sin(x) * cos(y) + sin(y) * cos(x)) == sin(x + y)
        @test simplify(sinh(x) * cosh(y) + sinh(y) * cosh(x)) == sinh(x + y)
        @test simplify(cos(x) * cos(y) - sin(x) * sin(y)) == cos(x + y)
        @test simplify(cosh(x) * cosh(y) + sinh(x) * sinh(y)) ∈ (cosh(y + x), cosh(x + y))
    end

    @testset "trigsimpa" begin
        @test simplify(2 * sin(x) * cos(x)) == sin(2x)
        @test simplify(2 * sinh(x) * cosh(x)) == sinh(2x)
    end

    @testset "simplify leaves non-matching expressions alone" begin
        @test simplify(x) == x
        @test simplify(sin(x) + cos(y)) == sin(x) + cos(y)
    end
end

@testset "expand" begin
    ==ₛ(x,y) = iszero(simplify(x - y))
    @testset "canonicalize_expand" begin
        @test expand((x + y) * z) ==ₛ x * z + y * z
        @test expand(x^0) == 1
        @test expand(x^1) == x
        @test expand(x^(1//2)) == sqrt(x)
        @test expand(x^(1//3)) == cbrt(x)
        @test expand(ℯ^x) == exp(x)
        @test expand(sin(x) / cos(x)) == tan(x)
        @test expand(cos(x) / sin(x)) == cot(x)
    end

    @testset "expand_pow" begin
        @test expand((x * y)^2) ==ₛ x^2 * y^2
        @test expand(x^(a + b)) ==ₛ x^a * x^b
        @test expand(x^(a * b)) == (x^a)^b
    end

    @testset "expand_exp" begin
        @test expand(exp(x + y)) == exp(x) * exp(y)
        @test expand(exp(x * y)) == exp(x)^y
    end

    @testset "expand_log" begin
        @test expand(log(x^y)) == y * log(x)
        @test expand(log(x * y)) == log(x) + log(y)
    end

    @testset "expand_trig" begin
        @test expand(sin(x + y)) == sin(x) * cos(y) + sin(y) * cos(x)
        @test expand(sinh(x + y)) == sinh(x) * cosh(y) + sinh(y) * cosh(x)
        @test expand(cosh(2x)) == sinh(x)^2 + cosh(x)^2
    end
end
