# test extensions
using SimpleExpressions
@symbolic x p

using Metatheory


@testset "Metatheory" begin
r = @rule sin(2(~x)) --> 2sin(~x)*cos(~x)
@test r(sin(2x)) === 2*sin(x) * cos(x)

t = @theory begin
    ~a + ~a --> 2 * (~a)
    ~x / ~x --> 1
    ~x * 1 --> ~x
end
@test isequal(rewrite(x + x, t), 2x)
@test isequal(rewrite(x/x, t), 1)
@test isequal(rewrite(1*x, t), x)

end
