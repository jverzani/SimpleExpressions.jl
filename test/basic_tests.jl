# basics
import SimpleExpressions.TermInterface: children
import SimpleExpressions: D, solve, coefficients

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
    @test length(children(x + 2x + 6sin(x))) == 3
    @test length(children(x * 2x * 6sin(x))) == 5

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

@testset "predicates" begin
    a,b,c = SimpleExpressions.SymbolicNumber.((1,2,3))
    @test isinteger(a)
    @test isinteger(a + b + c)
    @test ispow2(b)
    @test !ispow2(c)
    @test !iszero(a)
    @test iszero(a-a)
    @test isone(a)
    @test isone(b-a)
    @test !isone(b)
    @test iseven(b)
    @test iseven(a + c)
    @test !iseven(a)
    @test isodd(a)
    @test isodd(a + b)
    @test !isodd(b)
    @test isfinite(a)
    @test !isfinite(a*Inf)
    @test !isinf(a)
    @test isinf(a*Inf)
    @test isnan(a*Inf - a*Inf)
    @test !isnan(a)
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

@testset "replace" begin
    @symbolic x p
    @symbolic ⋯

    ≈ₑ(u,v) = (x₀ = rand(); u(x₀) ≈ v(x₀))

    ex = log(1 + x^2) + log(1 + x^3)
    @test replace(ex, log=>sin) == sin(1 + (x ^ 2)) + sin(1 + (x ^ 3))
    @test replace(ex, log(1+⋯) => log1p(⋯)) == log1p(x ^ 2) + log1p(x ^ 3)

    ex = log(sin(x)) + tan(sin(x^2))
    @test replace(ex, sin => cos) == log(cos(x)) + tan(cos(x^2))
    @test replace(ex, sin(⋯) => tan(⋯)) == log(tan(x)) + tan(tan(x^2))
    @test replace(ex, sin(⋯) => tan((⋯)/2)) == log(tan(x/2)) + tan(tan(x^2/2))
    @test replace(ex, sin(⋯) => ⋯) == log(x) + tan(x^2)

    ex = (1 + x^2)^2 # outer one
    pr = (⋯)^2 => (⋯)^4
    @test replace(ex, pr) == (1 + (x ^ 2)) ^ 4
    @test replace(ex, pr, pr) == (1 + (x ^ 4)) ^ 4

    ex = sin(x + x*log(x) + cos(p + x + p + x^2))
    @test replace(ex, cos(x + ⋯) => ⋯) == sin(x + (x * log(x)) + p + p + (x ^ 2))

    @test replace(x, p=>2) == x
    @test replace(1 + x^2, x^2 => 2)() == 3  # 1 + 2 evaluates to 3

    # exact replacement; a bit speedier than `replace(ex, expr=>replacement)`
    ex = x^2 + x^4
    @test replace(ex, x^2 => x) == x + x^4

    ex = x * sin(x)
    @test replace(ex, x*sin(x) => x) == x
    @test replace(ex*cos(x), x*sin(x) => x) == ex * cos(x)

    u = x + 2
    ex = u + exp(u) # (x + 2 + exp(x+2)) but + is vararg so no match
    @test replace(ex, u=>x) == (x + 2 + exp(x))

    @symbolic y; @symbolic z
    @test replace(x*y + z, x*y => pi) == pi + z
    @test replace(x*y*z, x*y => pi) == x*y*z
    @test replace(2x, 2x => y, x => z) == y
    @test replace(2 * (2x), 2x => y, x => z) == 4 * z # y isn't replaced, just x

    # match
    @test match(log(1 + ⋯), log(1 + x^2/2 - x^4/24)) ≈ₑ x^2/2 - x^4/24
    @test match((⋯)^(⋯), (x+p)^(x+p)) == x + p
    @test isnothing(match(sin(⋯), sin(x)^2))


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
    ## Need to address Base.broadcasted method
    @test x₀ |> (x .- sum(x)/length(x)) |> x .* x  == (x₀ .- sum(x₀)/length(x₀)).^2

    @test x₀ .|> x^2 == x₀.^2
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

    ## goal with broadcasting
    ## we want to be able to broadcast functdion calls
    @symbolic x p
    u = x + p
    @test u.([1,2],3) == [4,5]
    @test u.([1,2], [3,4]) == [1+3, 2+4]
    @test u.([1,2],[3 4]) == [1+3 2+3; 1+4 2+4]

    ## we want to be able to create symbolic expressions that will broadcast arguments
    u = x.^2  # literal_pow
    v = x^2
    @test u((1,2)) == v.((1,2)) == (1,2) .^ 2

    u = sin.(x)
    v = sin(x)
    @test u((1,2)) == v.((1,2)) == sin.((1,2))

    u = x .^ 2.0
    v = x ^ 2.0
    @test u((1,2)) == v.((1,2)) == (1,2) .^ 2

    u = x .+ p
    v = x + p
    @test u((1,2), (3,4)) == v.((1,2), (3,4)) == (1+3, 2+4)

    # u  maps like f
    @symbolic x
    u = x^2
    f(x) = x^2
    xs = [1,2]
    @test map(u, xs)    == map(f, xs)
    @test map.(u, xs)   == map(f, xs)
    @test map.([u], xs) == map.([f], xs)
end

@testset "derivatives" begin

    ∂(u,x) = (h = 1e-6; (u(x+h)-u(x))/h)

    @symbolic x
    x₀ = 2

    ex = cos(x)*sin(x^2+x)
    @test D(ex, x)(x₀) ≈ ∂(ex, x₀) atol=1e-4

    ex = exp(x^2 - 2) * log(x + sin(x))
    @test D(ex,x)(x₀) ≈ ∂(ex, x₀) atol=1e-4

    ex = log1p(x^2) * sqrt(1 + sin(x)^2)
    @test D(ex,x)(x₀) ≈ ∂(ex, x₀) atol=1e-4

    ex = (x^2 + 1) / (x^2 - x)
    @test D(ex,x)(x₀) ≈ ∂(ex, x₀) atol=1e-4

    ex = abs(inv(x))
    @test D(ex,x)(x₀) ≈ ∂(ex, x₀) atol=1e-4

    ex = log(x)
    u = D(D(ex,x),x) - D(1/x,x)
    @test u(x₀) == 0
    @test isnan(u(-x₀)) # Fix indicator ones

    # sum rule (vararg)
    ex = sin(x) + cos(x) + tan(x)
    @test D(ex, x)(x₀) ≈ ∂(ex, x₀) atol=1e-4

    # different variables
    @symbolic w p
    ex = cos(w) - p*w
    @test D(ex, w) == D(ex) # finds w from expression
    @test D(ex, p) == -w
    
end

@testset "solve" begin
    # solve is really rudimentary
    # and experimental to see if it has any value
    @symbolic a A
    @symbolic b B

    eq = solve(sin(A)/a ~ sin(B)/b, b)
    @test eq.lhs == b
    @test !contains(eq.rhs, b)

    eq = solve(a*A + b ~ B, A)
    @test eq.lhs == A
    @test !contains(eq.rhs, A)

    constraint = B ~ 2a + 2b
    A = a * b
    u = solve(constraint, b)
    A = A(u) # use equation in replacement
    v = solve(D(A, a) ~ 0, a) # lack of simplification masks answer
    l,r = v
    @test l == a # solved
    @test !contains(r, a)
    @test contains(r, B) # parameterized still
    
end

@testset "coefficients" begin
    @symbolic x p

    @test length(coefficients(x^2 - x ~ 1, x)) == 2 + 1
    @test length(coefficients(x^2*(x+1)*(x+p) ~ 0, x)) == 4 + 1
    @test isnothing(coefficients(x + 1/x ~ 1, x))
    @test isnothing(coefficients(x + sin(x)*x^2 ~ 1, x))
end
