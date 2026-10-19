# Practice problems from Paul's Online Math Notes (https://tutorial.math.lamar.edu),
# with the variable renamed to x. Each solution must satisfy the equation and
# the solutions must match the published real ones (trig: principal value).
using SimpleExpressions
using Test
import SimpleExpressions: solve

@testset "solve: Paul's Online Notes examples" begin
    @symbolic x
    P = Tuple{String,String,Any,Vector{Float64}}[]   # (id, text, equation thunk, expected real roots)
    add(id, txt, f, roots) = push!(P, (id, txt, f, Float64.(roots)))
    # expected real roots, from the published solutions; trig: principal value only
    # --- linear / rational
    add("L1", "4x - 7(2-x) = 3x + 2",           () -> 4x - 7(2-x) ~ 3x + 2, [2])
    add("L2", "2(x+3) - 10 = 6(32-3x)",         () -> 2(x+3) - 10 ~ 6(32-3x), [49/5])
    add("L3", "(4-2x)/3 = 3/4 - 5x/6",          () -> (4-2x)/3 ~ 3//4 - 5x/6, [-7/2])
    add("L4", "4x/(x^2-25) = 1/(5-x)",          () -> 4x/(x^2-25) ~ 1/(5-x), [-1])
    add("L5", "(3x+4)/(x-1) = 2 + 7/(x-1)",     () -> (3x+4)/(x-1) ~ 2 + 7/(x-1), [])
    add("L6", "5x/(3x-3) + 6/(x+2) = 5/3",      () -> 5x/(3x-3) + 6/(x+2) ~ 5//3, [8/23])
    # --- quadratics
    add("Q1", "x^2 - 5x - 14 = 0",              () -> x^2 - 5x - 14 ~ 0, [-2, 7])
    add("Q2", "x^2 + 15x = -50",                () -> x^2 + 15x ~ -50, [-10, -5])
    add("Q3", "x^2 = 11x - 28",                 () -> x^2 ~ 11x - 28, [4, 7])
    add("Q4", "19x = 7 - 6x^2",                 () -> 19x ~ 7 - 6x^2, [-7/2, 1/3])
    add("Q5", "6x^2 - x = 5",                   () -> 6x^2 - x ~ 5, [-5/6, 1])
    add("Q6", "x^2 - 16x + 61 = 2x - 20",       () -> x^2 - 16x + 61 ~ 2x - 20, [9])
    add("Q7", "12x^2 = 25x",                    () -> 12x^2 ~ 25x, [0, 25/12])
    add("Q8", "x^2 - 10x + 34 = 0",             () -> x^2 - 10x + 34 ~ 0, [])
    add("Q9", "x^2 + 8x - 9 = 0",               () -> x^2 + 8x - 9 ~ 0, [-9, 1])
    add("Q10", "x^2 + 9x + 16 = 0",             () -> x^2 + 9x + 16 ~ 0, [(-9-sqrt(17))/2, (-9+sqrt(17))/2])
    add("Q11", "4x^2 - 8x + 5 = 0",             () -> 4x^2 - 8x + 5 ~ 0, [])
    add("Q12", "2x^2 + 5x + 3 = 0",             () -> 2x^2 + 5x + 3 ~ 0, [-3/2, -1])
    add("Q13", "x^2 - 6x + 4 = 0",              () -> x^2 - 6x + 4 ~ 0, [3-sqrt(5), 3+sqrt(5)])
    add("Q14", "9x^2 - 6x = 101",               () -> 9x^2 - 6x ~ 101, [(1-sqrt(102))/3, (1+sqrt(102))/3])
    add("Q15", "8x^2 + 5x + 70 = 5 - 7x",       () -> 8x^2 + 5x + 70 ~ 5 - 7x, [])
    add("Q16", "169 - 20x + 4x^2 = 0",          () -> 169 - 20x + 4x^2 ~ 0, [])
    add("Q17", "2x^2 + x - 72 = x^2 - 2x + 58", () -> 2x^2 + x - 72 ~ x^2 - 2x + 58, [-13, 10])
    # --- polynomial zeros
    add("P1", "2x^3 - 13x^2 + 3x + 18 = 0",     () -> 2x^3 - 13x^2 + 3x + 18 ~ 0, [-1, 3/2, 6])
    add("P2", "x^4 - 3x^3 - 5x^2 + 3x + 4 = 0", () -> x^4 - 3x^3 - 5x^2 + 3x + 4 ~ 0, [-1, 1, 4])
    add("P3", "2x^4 - 7x^3 - 2x^2 + 28x - 24 = 0", () -> 2x^4 - 7x^3 - 2x^2 + 28x - 24 ~ 0, [-2, 3/2, 2])
    add("P4", "8x^5 + 36x^4 + 46x^3 + 7x^2 - 12x - 4 = 0", () -> 8x^5 + 36x^4 + 46x^3 + 7x^2 - 12x - 4 ~ 0, [-2, -1/2, 1/2])
    # --- reducible to quadratic
    add("R1", "x^6 - 9x^3 + 8 = 0",             () -> x^6 - 9x^3 + 8 ~ 0, [1, 2])
    add("R2", "x^-4 - 7x^-2 - 18 = 0",          () -> x^-4 - 7x^-2 - 18 ~ 0, [-1/3, 1/3])
    # real roots are -27, -729/64, but x^(1//3) is a DomainError for x < 0 in Julia (see R3c)
    add("R3", "4x^(2/3) + 21x^(1/3) + 27 = 0",  () -> 4x^(2//3) + 21x^(1//3) + 27 ~ 0, [])
    add("R3c", "4cbrt(x)^2 + 21cbrt(x) + 27 = 0", () -> 4cbrt(x)^2 + 21cbrt(x) + 27 ~ 0, [-27, -729/64])
    add("R4", "x^10 - 6x^5 + 7 = 0",            () -> x^10 - 6x^5 + 7 ~ 0, [(3-sqrt(2))^(1/5), (3+sqrt(2))^(1/5)])
    add("R5", "2/x^2 + 17/x + 21 = 0",          () -> 2/x^2 + 17/x + 21 ~ 0, [-2/3, -1/7])
    add("R6", "1/x - 11/sqrt(x) + 18 = 0",      () -> 1/x - 11/sqrt(x) + 18 ~ 0, [1/81, 1/4])
    # --- absolute value
    add("A1", "|4x - 7| = 3",                   () -> abs(4x - 7) ~ 3, [1, 5/2])
    add("A2", "|2 - 4x| = 1",                   () -> abs(2 - 4x) ~ 1, [1/4, 3/4])
    add("A3", "6x = |1 + 3x|",                  () -> 6x ~ abs(1 + 3x), [1/3])
    add("A4", "|2x - 3| = 4 - x",               () -> abs(2x - 3) ~ 4 - x, [-1, 7/3])
    add("A5", "|x/2 + 4| = |4x - 6|",           () -> abs(x/2 + 4) ~ abs(4x - 6), [4/9, 20/7])
    # --- radicals
    add("RD1", "2x = sqrt(x+3)",                () -> 2x ~ sqrt(x + 3), [1])
    add("RD2", "sqrt(33-2x) = x+1",             () -> sqrt(33 - 2x) ~ x + 1, [4])
    add("RD3", "7 = sqrt(39+3x) - x",           () -> 7 ~ sqrt(39 + 3x) - x, [-1])
    add("RD4", "x = 1 + sqrt(2x-2)",            () -> x ~ 1 + sqrt(2x - 2), [1, 3])
    add("RD5", "1 + sqrt(1-x) = sqrt(2x+4)",    () -> 1 + sqrt(1 - x) ~ sqrt(2x + 4), [0])
    # --- exponential
    add("E1", "6^(2x) = 6^(1-3x)",              () -> 6^(2x) ~ 6^(1 - 3x), [1/5])
    add("E2", "5^(1-x) = 25",                   () -> 5^(1 - x) ~ 25, [-1])
    add("E3", "8^(x^2) = 8^(3x+10)",            () -> 8^(x^2) ~ 8^(3x + 10), [-2, 5])
    add("E4", "7^(4-x) = 7^(4x)",               () -> 7^(4 - x) ~ 7^(4x), [4/5])
    add("E5", "2^(3x) = 10",                    () -> 2^(3x) ~ 10, [log(10)/(3log(2))])
    add("E6", "7^(1-x) = 4^(3x+1)",             () -> 7^(1 - x) ~ 4^(3x + 1), [(log(7)-log(4))/(log(7)+3log(4))])
    add("E7", "9 = 10^(4+6x)",                  () -> 9 ~ 10^(4 + 6x), [(log10(9)-4)/6])
    add("E8", "e^(7+2x) - 3 = 0",               () -> exp(7 + 2x) - 3 ~ 0, [(log(3)-7)/2])
    add("E9", "e^(4-7x) + 11 = 20",             () -> exp(4 - 7x) + 11 ~ 20, [(4-log(9))/7])
    add("C1", "12 - 4e^(7+3x) = 7",             () -> 12 - 4exp(7 + 3x) ~ 7, [(log(5/4)-7)/3])
    add("C2", "1 = 10 - 3e^(x^2-2x)",           () -> 1 ~ 10 - 3exp(x^2 - 2x), [1-sqrt(1+log(3)), 1+sqrt(1+log(3))])
    add("C3", "2x - x e^(6x-1) = 0",            () -> 2x - x*exp(6x - 1) ~ 0, [0, (1+log(2))/6])
    add("C4", "4x+1 = (12x+3) e^(x^2-2)",       () -> 4x + 1 ~ (12x + 3)*exp(x^2 - 2), [-sqrt(2-log(3)), -1/4, sqrt(2-log(3))])
    add("C5", "2e^(3x+8) - 11e^(5-10x) = 0",    () -> 2exp(3x + 8) - 11exp(5 - 10x) ~ 0, [(log(11/2)-3)/13])
    add("C6", "14e^(6-x) + e^(12x-7) = 0",      () -> 14exp(6 - x) + exp(12x - 7) ~ 0, [])
    add("C11", "16 = 17^(x-2) + 11",            () -> 16 ~ 17^(x - 2) + 11, [2 + log(5)/log(17)])
    add("C12", "2^(3-8x) - 7 = 11",             () -> 2^(3 - 8x) - 7 ~ 11, [(3 - log2(18))/8])
    # --- logarithms
    add("LG1", "log4(x^2-2x) = log4(5x-12)",    () -> log(4, x^2 - 2x) ~ log(4, 5x - 12), [3, 4])
    add("LG2", "log(6x) - log(4-x) = log(3)",   () -> log10(6x) - log10(4 - x) ~ log10(3), [4/3])
    add("LG3", "ln x + ln(x+3) = ln(20-5x)",    () -> log(x) + log(x + 3) ~ log(20 - 5x), [2])
    add("LG4", "log3(25-x^2) = 2",              () -> log(3, 25 - x^2) ~ 2, [-4, 4])
    add("LG5", "log2(x+1) - log2(2-x) = 3",     () -> log(2, x + 1) - log(2, 2 - x) ~ 3, [5/3])
    add("LG6", "log4(-x) + log4(6-x) = 2",      () -> log(4, -x) + log(4, 6 - x) ~ 2, [-2])
    add("LG7", "log x = 2 - log(x-21)",         () -> log10(x) ~ 2 - log10(x - 21), [25])
    add("LG8", "ln(x-1) = 1 + ln(3x+2)",        () -> log(x - 1) ~ 1 + log(3x + 2), [])
    add("LG9", "2log x - log(7x-1) = 0",        () -> 2log10(x) - log10(7x - 1) ~ 0, [(7-sqrt(45))/2, (7+sqrt(45))/2])
    add("C7", "1 - 8 ln((2x-1)/7) = 14",        () -> 1 - 8log((2x - 1)/7) ~ 14, [(7exp(-13/8)+1)/2])
    add("C9", "log x + log(x-21) = 2",          () -> log10(x) + log10(x - 21) ~ 2, [25])
    # --- trig (no intervals; principal value expected)
    add("T1", "4sin(3x) = 2",                   () -> 4sin(3x) ~ 2, [pi/18])
    add("T3", "2cos(x/3) + sqrt2 = 0",          () -> 2cos(x/3) + sqrt(2) ~ 0, [9pi/4])
    add("T5", "4cos(6x) = sqrt12",              () -> 4cos(6x) ~ sqrt(12), [pi/36])
    add("T6", "2sin(3x/2) + sqrt3 = 0",         () -> 2sin(3x/2) + sqrt(3) ~ 0, [-2pi/9])
    add("T7", "8tan(2x) - 5 = 3",               () -> 8tan(2x) - 5 ~ 3, [pi/8])
    add("T8", "16 = -9sin(7x) - 4",             () -> 16 ~ -9sin(7x) - 4, [])
    add("T9", "sqrt3 tan(x/4) + 5 = 4",         () -> sqrt(3)*tan(x/4) + 5 ~ 4, [-2pi/3])
    add("T10", "sqrt3 csc(9x) - 7 = -5",        () -> sqrt(3)*csc(9x) - 7 ~ -5, [pi/27])
    add("T11", "1 - 14cos(2x/5) = -6",          () -> 1 - 14cos(2x/5) ~ -6, [5pi/6])
    add("T12", "15 = 17 + 4cos(x/7)",           () -> 15 ~ 17 + 4cos(x/7), [14pi/3])
    add("T13", "7cos(4x) + 11 = 10",            () -> 7cos(4x) + 11 ~ 10, [acos(-1/7)/4])
    add("T14", "6 + 5cos(x/3) = 10",            () -> 6 + 5cos(x/3) ~ 10, [3acos(4/5)])
    add("T15", "3 = 6 - 11sin(x/8)",            () -> 3 ~ 6 - 11sin(x/8), [8asin(3/11)])
    add("T16", "4sin(6x) + 13/10 = -3/10",      () -> 4sin(6x) + 13//10 ~ -3//10, [asin(-2/5)/6])
    add("T17", "9cos(4x/9) + 21sin(4x/9) = 0",  () -> 9cos(4x/9) + 21sin(4x/9) ~ 0, [9atan(-3/7)/4])
    add("T18", "3tan(x/4) - 1 = 11 - 2tan(x/4)",() -> 3tan(x/4) - 1 ~ 11 - 2tan(x/4), [4atan(12/5)])
    add("T19", "17 - 3sec(x/2) = 2",            () -> 17 - 3sec(x/2) ~ 2, [2acos(1/5)])
    add("T20", "12sin(7x) + 11 = 3 + 4sin(7x)", () -> 12sin(7x) + 11 ~ 3 + 4sin(7x), [-pi/14])
    add("T21", "5 - 14tan(8x) = 30",            () -> 5 - 14tan(8x) ~ 30, [atan(-25/14)/8])
    add("T22", "0 = 18 + 2csc(x/3)",            () -> 0 ~ 18 + 2csc(x/3), [3asin(-1/9)])
    add("T23", "cos(x/8)/2 + 1/4 = 2/3",        () -> cos(x/8)/2 + 1//4 ~ 2//3, [8acos(5/6)])
    add("T24", "4/3 = 1 + 3sec(2x)",            () -> 4//3 ~ 1 + 3sec(2x), [])

    solved(s) = isequal(s.lhs, x) && !contains(s.rhs, x)
    numval(s) = try Float64(real(s.rhs())) catch; NaN end
    function classify(eq, sols, expected)
        isnothing(sols) && return ("NOTHING", "")
        sols isa Tuple || (sols = (sols,))
        if isempty(sols)
            return isempty(expected) ? ("OK", "()") : ("WRONG", "() but expected $(expected)")
        end
        any(!solved, sols) && return ("UNSOLVED", string(sols))
        vs = sort(map(numval, collect(sols)))
        # check residuals
        bad = [v for v in vs if !(abs(eq(v)) < 1e-8 * (1 + abs(v)))]
        ok = all(e -> any(v -> isapprox(v, e; atol=1e-9, rtol=1e-9), vs), expected)
        extra = [v for v in vs if !any(e -> isapprox(v, e; atol=1e-9, rtol=1e-9), expected)]
        msg = string(length(sols) == 1 ? only(sols) : sols)
        !isempty(bad) && return ("WRONG", msg * "  [residual fails at $bad]")
        ok && isempty(extra) && return ("OK", msg)
        ok && return ("EXTRA?", msg * "  [valid but not expected: $extra]")   # e.g. other branch of trig
        return ("PARTIAL", msg * "  [missing: $(setdiff(expected, vs))]")
    end

    for (id, txt, f, expected) in P
        eq = f()
        status, msg = classify(eq, solve(eq, x), expected)
        @test (id, status) == (id, "OK")
    end
end
