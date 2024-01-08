using SimpleExpressions
using Test

@testset "SimpleSymbolics.jl" begin

    @symbolic x p

    # basic arithemtic
    x₀ = 3
    @test (x + 2)(x₀)  == x₀ + 2
    @test (-x)(x₀)  == -x₀
    @test (x * 2)(x₀)  == x₀ * 2
    @test (x / 2)(x₀) == x₀ / 2
    @test (x // 2)(x₀) == x₀ // 2
    @test (x ^ 2)(x₀)  == x₀ ^ 2

    # in a pipeline
    @test 1 |> x^2 |> x - 3 == 1^2 - 3

    # simple map
    @test map(x^2, (1,2)) == (1^2, 2^2)

    # broadcasting
    x₀ = [1,2,3]
    @test x₀ |> (x .- sum(x)/length(x)) |> x .* x  == (x₀ .- sum(x₀)/length(x₀)).^2
    @test x₀ |> x.^2 == x₀.^2
    @test x₀ |> x.^2.0 == x₀.^2.0
    @test_skip @test_throws MethodError x₀ |> x^2



    # using a parameter
    u = cos(x) - p*x
    @test u(1,2) == cos(1) - 2*1

    # basic generators
    x₀, p₀ = 2, (1,2,3)

    # eachindex
    u = sum(x[i] for i ∈ eachindex(x))
    @test u(x₀) == sum(x₀[i] for i ∈ eachindex(x₀))

    # enumerate
    u = sum(pᵢ*x^i for (i,pᵢ) ∈ enumerate(p))
    @test u(x₀, p₀) == sum(pᵢ*x₀^i for (i, pᵢ) ∈ enumerate(p₀))

    # zip
    x₀ = (2,3)
    u = prod(xᵢ*pᵢ for (xᵢ, pᵢ) ∈ zip(x, p))
    @test u(x₀, p₀) == prod(xᵢ*pᵢ for (xᵢ, pᵢ) ∈ zip(x₀, p₀))

    # test show
    @symbolic x p

    @test repr(2x) == "2 * x"
    @test repr(x*2) == "x * 2"

    @test repr(x*(1+x)) == "x * (1 + x)"
    @test repr(x / 2) == "x / 2"
    @test repr((x+2) / 2) == "(x + 2) / 2"
    @test repr(x / (x+2)) == "x / (x + 2)"
    @test repr(x .- sum(x)/length(x)) == "x .- (sum(x) / length(x))" # parens around expressions, like `sum(x)`.

    @test repr((1+x)^2) == "(1 + x) ^ 2"

end

@testset "derivatives" begin
    D = SimpleExpressions.D
    ∂(u,x) = (h = 1e-6; (u(x+h)-u(x))/h)

    @symbolic x
    x₀ = 2

    ex = cos(x)*sin(x^2+x)
    @test D(ex)(x₀) ≈ ∂(ex, x₀) atol=1e-4

    ex = exp(x^2 - 2) * log(x + sin(x))
    @test D(ex)(x₀) ≈ ∂(ex, x₀) atol=1e-4

    ex = log1p(x^2) * sqrt(1 + sin(x)^2)
    @test D(ex)(x₀) ≈ ∂(ex, x₀) atol=1e-4

end
