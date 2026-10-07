using Test
using SimpleExpressions
using SimpleExpressions.Gruntz: gruntz_limit, GruntzError
using SimpleExpressions: limit

SimpleExpressions.@symbolic_variables x a b

@testset "limits via Gruntz's algorithm" begin
    @test gruntz_limit(x, x) == Inf
    @test gruntz_limit(1/x, x) == 0
    @test gruntz_limit(x^2 * exp(-x), x) == 0
    @test gruntz_limit(log(x)/x, x) == 0
    @test gruntz_limit((x^2 + 1) / (2x^2 - 3), x) == 1//2
    @test gruntz_limit(sqrt(x^2 + x) - x, x) == 1//2
    @test gruntz_limit(exp(x + exp(-x)) - exp(x), x) == 1
    @test gruntz_limit(exp(x) / x^100, x) == Inf
    @test gruntz_limit(-exp(x), x) == -Inf
    @test gruntz_limit(x * sin(1/x), x) == 1
    @test gruntz_limit(log(x + 1) - log(x), x) == 0
    @test gruntz_limit(x^x / exp(x), x) == Inf
    @test gruntz_limit(x - x, x) == 0
    @test gruntz_limit((1 + 1/x)^x, x) == exp(one(x))
    @test gruntz_limit((1 + a/x)^x, x) == exp(a)

    # finite points, one-sided limits, -Inf
    @test gruntz_limit(sin(x)/x, x, 0) == 1
    @test gruntz_limit((exp(x) - 1)/x, x, 0) == 1
    @test gruntz_limit(1/x, x, 0; dir = :+) == Inf
    @test gruntz_limit(1/x, x, 0; dir = :-) == -Inf
    @test gruntz_limit(x^2, x, -Inf) == Inf

    @test gruntz_limit(x + 1/x, x, 3) == 10//3

    # arctangent
    @test gruntz_limit(atan(x), x) == π*one(x)/2
    @test gruntz_limit(atan(x), x, -Inf) ==  -(π*one(x))/2
    @test gruntz_limit(atan(exp(x)), x) == (π*one(x))/2
    @test gruntz_limit(atan(x)/x, x) == 0
    @test gruntz_limit(x * atan(1/x), x) == 1
    @test gruntz_limit(x^2 * (atan(x + 1) - atan(x)), x) == 1
    @test gruntz_limit(atan(1/x), x, 0; dir = :+) == (π*one(x))/2
    @test gruntz_limit(atan(1/x), x, 0; dir = :-) == -(π*one(x))/2
    @test gruntz_limit(atan(x)/x, x, 0) == 1
    @test gruntz_limit((atan(x) - x)/x^3, x, 0) == -1//3
    @test gruntz_limit(atan(x^2 - x), x) == (π*one(x))/2

    # absolute value (resolved from the sign near the limit point)
    @test gruntz_limit(x/abs(x), x, 0; dir = :+) == 1
    @test gruntz_limit(x/abs(x), x, 0; dir = :-) == -1
    @test_throws GruntzError gruntz_limit(x/abs(x), x, 0; dir = :both)
    @test gruntz_limit(abs(x), x, 0) == 0
    @test gruntz_limit(abs(x), x, -Inf) == Inf
    @test gruntz_limit(abs(x)/x, x, -Inf) == -1
    @test gruntz_limit(abs(x - 1)/(x - 1), x, 1; dir = :-) == -1
    @test gruntz_limit(abs(sin(x))/x, x, 0; dir = :+) == 1
    @test gruntz_limit(abs(x^2 - 4)/(x - 2), x, 2; dir = :+) == 4
    @test gruntz_limit(abs(x^2 - 4)/(x - 2), x, 2; dir = :-) == -4

    # harder ones?
    @test gruntz_limit((cos(sin(x))-cos(x))/x^4, x, 0) == 1//6
    @test gruntz_limit((x^x - x^a)/(a^x - a^a), x, a) == 1
    @test gruntz_limit((sin(x)/x)^(1/x^2),x, 0) == 1/exp(one(x))^(1//6)
    @test gruntz_limit(1/x^2 - 1/sin(x)^2, x, 0) == - 1//3
    @test gruntz_limit(sqrt((1 - cos(2x)))/x, x, 0; dir=:+) == sqrt(2*one(x))
    @test gruntz_limit(sqrt((1 - cos(2x)))/x, x, 0; dir=:-) == -sqrt(2*one(x))

    # bounded oscillation has no limit
    @test_throws GruntzError gruntz_limit(sin(x), x)
end

@testset "limit interface" begin
    # test direction specification
    @test_throws GruntzError limit(x/abs(x), x=>0) # :both is default
    @test limit(x/abs(x), x=>0; dir=-) == -1
    @test limit(x/abs(x), x=>0; dir=+) == 1

    # specify direction different ways
    @test limit(x/abs(x), x=>0; dir=-) == limit(x/abs(x), x=>0; dir=:-) == limit(x/abs(x), x=>0; dir="-")

    @test limit(x/abs(x), x=>0; dir=+) == limit(x/abs(x), x=>0; dir=:+) == limit(x/abs(x), x=>0; dir="+")

end
