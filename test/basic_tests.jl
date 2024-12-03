# basics


@testset "SimpleExpressions.jl" begin

    @symbolic x p

    # basic arithmetic
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

    # using a parameter
    u = cos(x) - p*x
    @test u(1,2) == cos(1) - 2*1
    x₀ = rand()
    @test (u(nothing,2)(x₀) == u(:,2)(x₀) == cos(x₀) - 2*x₀) # replace parameter

    # function algebra
    f = sin(x)
    g = cos(x)
    @test (f∘g)(x₀) == f(g(x₀)) == sin(cos(x₀))
    for op ∈ (+, -, *, /, ^)
        @test op(f,g)(x₀) == op(f(x₀), g(x₀))
    end

end

@testset "evaluation/substitution" begin
    @symbolic x p
    f = (x,p)  -> cos(x) - x*p
    u = f(x, p)
    x₀,p₀ = 1, 2

    @test u(x₀,p₀)       == f(x₀, p₀)
    @test u(x₀)(:,p₀)    == f(x₀, p₀)
    @test u(x₀, :)(:,p₀) == f(x₀, p₀)
    @test u(:,p₀)(x₀)    == f(x₀, p₀)
    @test u(:,p₀)(x₀,:)  == f(x₀, p₀)

    @test u(;x=x₀, p=p₀) == f(x₀, p₀)

    f = (x,p) -> x^2
    u = f(x,p)
    @test u(x₀)          == f(x₀, p₀)
    @test u(x₀,p₀)       == f(x₀, p₀)
    @test u(:,p₀)(x₀)    == f(x₀, p₀)
    @test u(:,p₀)(x₀,:)  == f(x₀, p₀)

    f = (x,p) -> p^2
    u = f(x,p)
    @test u(:, p₀)       == f(x₀, p₀)
    @test u(x₀,p₀)       == f(x₀, p₀)
    @test u(x₀)(:,p₀)    == f(x₀, p₀)
    @test u(x₀, :)(:,p₀) == f(x₀, p₀)

end




@testset "show" begin
        # test show
    # note *,+ do light simplification and sort arguments
    @symbolic x p

    @test_broken repr(2x) == "2 * x"
    @test_broken repr(x*2) == "2 * x"

    @test_broken repr(x / 2) == "x / 2"
    @test_broken repr((x+2) / 2) == "(2 + x) / 2"
    @test_broken repr(x / (x+2)) == "x / (2 + x)"
    @test_broken repr(x .- sum(x)/length(x)) == "x .- (sum(x) / length(x))" # parens around expressions, like `sum(x)`.

    @test_broken repr((1+x)^2) == "(1 + x) ^ 2"

end

@testset "broken" begin
    @symbolic x p
    # broadcasting
    x₀ = [1,2,3]
    @test_broken x₀ |> (x .- sum(x)/length(x)) |> x .* x  == (x₀ .- sum(x₀)/length(x₀)).^2
    @test_broken x₀ |> x.^2 == x₀.^2
    @test_broken x₀ |> x.^2.0 == x₀.^2.0
## XXX    @test_broken_throws MethodError x₀ |> x^2

    # basic generators
    x₀, p₀ = 2, (1,2,3)

    # eachindex
    u = sum(x[i] for i ∈ eachindex(x))
    @test_throws MethodError u(x₀) == sum(x₀[i] for i ∈ eachindex(x₀))

    # enumerate
    u = sum(pᵢ*x^i for (i,pᵢ) ∈ enumerate(p))
    @test_broken u(x₀, p₀) == sum(pᵢ*x₀^i for (i, pᵢ) ∈ enumerate(p₀))

    # zip
    x₀ = (2,3)
    u = prod(xᵢ*pᵢ for (xᵢ, pᵢ) ∈ zip(x, p))
    @test_broken u(x₀, p₀) == prod(xᵢ*pᵢ for (xᵢ, pᵢ) ∈ zip(x₀, p₀))

    # make new symbolic expressions
    u = @symbolic_expression foldl(=>, @symbolic_expression(1:x))
    @test_broken u(4) == (((1 => 2) => 3) => 4)

    # make new symbolic expressions
    u = @symbolic_expression foldl(=>, @symbolic_expression(1:x))
    @test_broken u(4) == (((1 => 2) => 3) => 4)


    # convert Expr type
    # (convert Expr to symbolic can be done with `assymbolic` if `TermInterface`
    # is loaded.
    u = sin(x) * (cos(x) - x^2)
    ex = convert(Expr, u)


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

    ex = (x^2 + 1) / (x^2 - x)
    @test D(ex)(x₀) ≈ ∂(ex, x₀) atol=1e-4

    ex = abs(inv(x))
    @test D(ex)(x₀) ≈ ∂(ex, x₀) atol=1e-4

    ex = log(x)
    u = D(D(ex)) - D(1/x)
    @test u(x₀) == 0
    @test_broken isnan(u(-x₀)) # Fix indicator ones

end
