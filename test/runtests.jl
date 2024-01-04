using SimpleExpressions
using Test

@testset "SimpleSymbolics.jl" begin

    @symbolic x p

    # simple map
    @test map(x^2, (1,2)) == (1^2, 2^2)

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
    u = cos(x) - p*x
    x₀, p₀ = 2,3
    @test eval(Meta.parse(repr(u)))(x₀,p₀) == u(x₀, p₀)


end
