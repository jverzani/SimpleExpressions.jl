# basics
import SimpleExpressions.TermInterface: children

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

    # +,* nary
    @test children(x + 2x + 6sin(x)) == 3
    @test children(x * 2x * 6sin(x)) == 5

    # sort
    @test sort(children(6*sin(x)*x*p*2)) == [2,6,p,x,sin(x)]

    # isless + isequal: exactly one of those three yields true.
    xs = (2, 3, x, 2x, p, 2p, x^2,sin(x),x+x^2+x^3)
    for a in xs
        for b in xs
            @test sum(isless(a,b) + isequal(a,b) + isless(b,a)) == 1
        end
    end

    # inv
    @test inv(inv(x)) == x
    @test inv(x^p) == x^(-p)
    x₀ = rand() # integers special cased, so harder to test
    @test inv(x^2)(x₀) ≈ (x^(-2))(x₀)
end

@testset "evaluation/substitution" begin
    @symbolic x p
    f = (x,p)  -> cos(x) - x*p
    uxp = f(x, p)
    x₀,p₀ = 1, 2

    @test uxp(x₀,p₀)         == f(x₀, p₀)
    @test_throws "type NamedTuple has no field" uxp(x₀)          # no p
    @test uxp(x₀, :)(:,p₀)() == f(x₀, p₀)
    @test uxp(:,p₀)(x₀)      == f(x₀, p₀)
    @test uxp(:,p₀)(x₀,:)()  == f(x₀, p₀)

    @test uxp(;x=x₀, p=p₀) == f(x₀, p₀)   # direct to CallableExpressions

    f = (x,p) -> x^2
    ux_ = f(x,p)
    @test ux_(x₀)            == f(x₀, p₀)
    @test ux_(x₀,p₀)         == f(x₀, p₀)
    @test ux_(:,p₀)(x₀)      == f(x₀, p₀)
    @test ux_(:,p₀)(x₀,:)()  == f(x₀, p₀)

    @test ux_(;x=x₀, p=p₀)   == f(x₀, p₀) # direct to CallableExpressions

    f = (x,p) -> p^2
    u_p = f(x,p)
    @test u_p(:, p₀)()       == f(x₀, p₀)
    @test u_p(x₀,p₀)         == f(x₀, p₀)
    @test_throws "type NamedTuple has no field" u_p(x₀)          # no p
    @test u_p(x₀, :)(:,p₀)() == f(x₀, p₀)

    @test u_p(;x=x₀, p=p₀)   == f(x₀, p₀) # direct to CallableExpressions


    u__ = replace(u_p, p=>p₀)
    @test u__()       == f(x₀,p₀)
    @test u__(x₀)     == f(x₀,p₀)
    @test_throws "duplicate field name in NamedTuple" u__(x₀, p₀) == f(x₀,p₀)

    u = cos(x)*sin(p*x)
    # : = nothing = missing
    import SimpleExpressions: ↓
    @test ↓(u(:,p₀)) == ↓(u(missing, p₀)) == ↓(u(nothing, p₀))
    @test ↓(u(x₀,:)) == ↓(u(x₀, missing)) == ↓(u(x₀, nothing))

    # Number or symbolic output
    @test u(x₀, p₀)                isa Number
    @test u(:, p₀)(x₀)             isa Number
    @test u(x₀, :)(*, p₀)          isa Number
    @test u(:, p₀)                 isa SimpleExpressions.AbstractSymbolic
    @test u(x₀, :)                 isa SimpleExpressions.AbstractSymbolic
    @test replace(u, x=>x₀, p=>p₀) isa SimpleExpressions.AbstractSymbolic
    @test u(x=>x₀, p=>p₀)          isa SimpleExpressions.AbstractSymbolic
    @test u(x=>x₀)                 isa SimpleExpressions.AbstractSymbolic
    @test u(p=>p₀)                 isa SimpleExpressions.AbstractSymbolic
end




@testset "show" begin
    # test show
    # note *,+ do **not** do light simplification and sort arguments
    # though they had
    @symbolic x p

    @test repr(2x) == "2 * x"
    @test_broken repr(x*2) == "2 * x" # sort?

    @test repr(x / 2) == "x / 2"
    @test_broken repr((x+2) / 2) == "(2 + x) / 2"
    @test repr(x / (x+2)) == "x / (x + 2)"
    @test_broken repr(x .- sum(x)/length(x)) == "x .- (sum(x) / length(x))" # parens around expressions, like `sum(x)`.

    @test repr((1+x)^2) == "(1 + x) ^ 2"

end

@testset "broadcast/generators" begin
    @symbolic x p

    x₀ = [1,2,3]
    @test x₀ |> (x .- sum(x)/length(x)) |> x .* x  == (x₀ .- sum(x₀)/length(x₀)).^2

    @test x₀ |> x.^2 == x₀.^2
    @test x₀ |> x.^2.0 == x₀.^2.0

    @test_throws MethodError x₀ |> x^2

    # basic generators
    x₀, p₀ = 2, (1,2,3)

    # eachindex
    u = sum(x[i] for i ∈ eachindex(x))
    @test u(x₀) == sum(x₀[i] for i ∈ eachindex(x₀))

    x₀′ = (1,2)
    u = sum(p[i] for i ∈ eachindex(x))
    @test u(x₀′, p₀) == sum(p₀[i] for i in eachindex(x₀′))



    # enumerate
    u = sum(pᵢ*x^i for (i,pᵢ) ∈ enumerate(p))
    @test u(x₀, p₀) == sum(pᵢ*x₀^i for (i, pᵢ) ∈ enumerate(p₀))

    # zip
    x₀ = (3,2,3)
    u = prod(xᵢ*pᵢ for (xᵢ, pᵢ) ∈ zip(x, p))
    @test u(x₀, p₀) == prod(xᵢ*pᵢ for (xᵢ, pᵢ) ∈ zip(x₀, p₀))

    # make new symbolic expressions
    u = @symbolic_expression foldl(=>, @symbolic_expression(1:x))
    @test u(4) == (((1 => 2) => 3) => 4)


    # convert Expr type
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
    @test isnan(u(-x₀)) # Fix indicator ones

end
