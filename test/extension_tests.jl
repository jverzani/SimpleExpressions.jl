# test extensions

import SimpleExpressions: simplify, expand
using Metatheory
using Metatheory.Library

if pkgversion(Metatheory) >= v"3.0.0"

    @testset "Metatheory" begin
        @symbolic x p
        r = @rule sin(~x + ~y) --> sin(~x)cos(~y) + sin(~y)*cos(~x)
        @test r(sin(x + p)) == sin(x)*cos(p) + sin(p)*cos(x)

        t = @theory begin
            ~a + ~a --> 2 * (~a)
            ~x / ~x --> 1
            ~c::isone * ~x  --> ~x
        end
        @test rewrite(x + x, t) == 2x
        @test rewrite(x/x, t) == 1
        @test rewrite(1 * x, t) == x

        # similar to example from docs
        t = @theory a b c e begin
            ~a * ~b --> ~b * ~a
            (~a * ~b) * ~c --> a * (b * c)
            ~a * (~b * ~c) --> (a * b) * c
            1 * ~a --> ~a

            a + 0 --> a
            a + b --> b + a
            a + inv(a) --> 0 # inverse
            a + (b + c) --> (a + b) + c
	    a * (b + c) --> (a * b) + (a * c)
	    (a * b) + (a * c) --> a * (b + c)
	    a * a --> a^2
	    a --> a^1
	    a^b * a^c --> a^(b+c)
	    log(a^b) --> b * log(a)
	    log(a * b) --> log(a) + log(b)
	    log(1) --> 0
	    log(:e) --> 1
	    (:e)^(log(a)) --> a
	    a::Number + b::Number => a + b
	    a::Number * b::Number => a * b
        end
        @symbolic a
        exa = (log(a^3 * a^2))
        ex = exa * exa
        g = EGraph(ex)
        r = saturate!(g, t);
        @test extract!(g, astsize) == log(a^5)^2
    end

    # these tests can be a ...ve....ry... slow
    @testset "simplify" begin
        @symbolic x p
        # canonical
        @test simplify(x + 2p + 3x*sin(x)) |> children |> length == 3

        # distribute
        @test simplify(x + 2x) == 3x
        @test simplify(x + 2x + 3x) == 6x
        @test simplify(2x*sin(x) + 3x*sin(x)) == 5*x*sin(x)


        # power rules
        @test simplify(1^x) == 1
        @test simplify((2*x*p)^3) == 2^3 * p^3 * x^3
        @test simplify((x^2)^3) == x^(2*3)
        a,b = SimpleExpressions.SymbolicNumber.((0,1))
        @test simplify(x^a) == 1
        @test simplify(x^b) == x
        @test simplify(p*x^3*x^p) == p*x^(3+p)

        # trig rules
        @test simplify(cos(x)^2 + sin(x)^2) == 1
        @test simplify(cos(x)^2 + sin(x)^2) + 1 == 2
        @test simplify(cos(x)^2 - sin(x)^2) == cos(2x)

        # exp
        @test simplify(2*exp(x)*exp(p)) == 2*exp(p + x)

        # log
        @test simplify(log(x) + log(p) + log(x*p)) == log(p^2*x^2)
    end

    @testset "expand" begin
        @symbolic x p
        u = x*exp(x)
        @test SimpleExpressions.canonicalize(x - p) == x + (-1)*p
        @test expand(-(x + sin(x) + p)) == (-1)*x + (-1*sin(x)) + (-1)*p
        @test expand(x - x) == zero(x)
        @test expand(u - u) == zero(x)
        @test_broken expand(-u + u) == zero(x)

        @test expand(sin(2x)) == 2 * sin(x) * cos(x)
        @test expand(sin(x + p)) == sin(x)*cos(p) + cos(x) * sin(p)
        @test expand(cos(2x)) == cos(x)^2 - sin(x)^2
        @test expand(cos(x + p)) == cos(x)*cos(p) - sin(x)*sin(p)

        @test expand(x^(2 + p)) == x^2 * x^p
        @test expand((x*sin(x))^p) == x^p * sin(x)^p

        @test expand(log(x*p)) == log(x) + log(p)
        @test expand(log(x^p)) == p*log(x)

        @test expand(-x) == (-1)*x

    end

end
