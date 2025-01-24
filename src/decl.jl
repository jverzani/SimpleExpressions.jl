## constructors
## copied from SymPyCore: /src/decl.jl.
## Contributed by @matthieubulte to SymPy pr #419.
"""
    @symbolic_variables w x[1:3] y() z=>"𝑧" Ω::isinteger

Define multiple symbolic variables or symbolic functions. Guards are currently ignored.

Not exported.

```@repl
julia> using SimpleExpressions: @symbolic_variables

julia> @symbolic_variables  w x[1:3] y() z=>"𝑧" Ω::isinteger
(w, SimpleExpressions.SymbolicVariable[x₁, x₂, x₃], y, 𝑧, Ω)

julia> x
3-element Vector{SimpleExpressions.SymbolicVariable}:
 x₁
 x₂
 x₃
```

"""
macro symbolic_variables(xs...)
    # If the user separates declaration with commas, the top-level expression is a tuple
    if length(xs) == 1 && isa(xs[1], Expr) && xs[1].head == :tuple
        _gensyms(xs[1].args...)
    elseif length(xs) > 0
        _gensyms(xs...)
    end
end

function _gensyms(xs...)
    asstokw(a) = Expr(:kw, esc(a), true)

    # Each declaration is parsed and generates a declaration using `symbols`
    symdefs = map(xs) do expr
        decl = parsedecl(expr)
        symname = sym(decl)
        symname, gendecl(decl)
    end
    syms, defs = collect(zip(symdefs...))

    # The macro returns a tuple of Symbols that were declared
    Expr(:block, defs..., :(tuple($(map(esc,syms)...))))
end


# The map_subscripts function is stolen from Symbolics.jl
const IndexMap = Dict{Char,Char}(
    '-' => '₋',
    '0' => '₀',
    '1' => '₁',
    '2' => '₂',
    '3' => '₃',
    '4' => '₄',
    '5' => '₅',
    '6' => '₆',
    '7' => '₇',
    '8' => '₈',
    '9' => '₉')

function map_subscripts(indices)
    str = string(indices)
    join(IndexMap[c] for c in str)
end

# Define a type hierarchy to describe a variable declaration. This is mainly for convenient pattern matching later.
abstract type VarDecl end

struct SymDecl <: VarDecl
    sym :: Symbol
end

struct NamedDecl <: VarDecl
    name :: String
    rest :: VarDecl
end

struct FunctionDecl <: VarDecl
    rest :: VarDecl
end

struct TensorDecl <: VarDecl
    ranges :: Vector{AbstractRange}
    rest :: VarDecl
end

struct AssumptionsDecl <: VarDecl
    assumptions :: Vector{Symbol}
    rest :: VarDecl
end

# Transform a Decl struct in an Expression that calls SymPy to declare the corresponding symbol
function gendecl(x::VarDecl)
    asstokw(a) = Expr(:kw, esc(a), true)
    val = :($(ctor(x))($(name(x, missing)), $(map(asstokw, assumptions(x))...)))
    :($(esc(sym(x))) = $(genreshape(val, x)))
end

# Transform an expression in a Decl struct
function parsedecl(expr)
    # @syms x
    if isa(expr, Symbol)
        return SymDecl(expr)

    elseif isa(expr, NTuple{N, T} where {N,T})
        return SymDecl.(expr)

    # @syms x::assumptions, where assumption = assumptionkw | (assumptionkw...)
    elseif isa(expr, Expr) && expr.head == :(::)
        symexpr, assumptions = expr.args
        assumptions = isa(assumptions, Symbol) ? [assumptions] : assumptions.args
        return AssumptionsDecl(assumptions, parsedecl(symexpr))

    # @syms x=>"name"
    elseif isa(expr, Expr) && expr.head == :call && expr.args[1] == :(=>)
        length(expr.args) == 3 || parseerror()
        isa(expr.args[3], String) || parseerror()

        expr, strname = expr.args[2:end]
        return NamedDecl(strname, parsedecl(expr))

    # @syms x()
    elseif isa(expr, Expr) && expr.head == :call && expr.args[1] != :(=>)
        length(expr.args) == 1 || parseerror()
        return FunctionDecl(parsedecl(expr.args[1]))

    # @syms x[1:5, 3:9]
    elseif isa(expr, Expr) && expr.head == :ref
        length(expr.args) > 1 || parseerror()
        ranges = map(parserange, expr.args[2:end])
        return TensorDecl(ranges, parsedecl(expr.args[1]))
    else
        parseerror()
    end
end

function parserange(expr)
    range = eval(expr)
    isa(range, AbstractRange) || parseerror()
    range
end

sym(x::SymDecl) = x.sym
sym(x::NamedDecl) = sym(x.rest)
sym(x::FunctionDecl) = sym(x.rest)
sym(x::TensorDecl) = sym(x.rest)
sym(x::AssumptionsDecl) = sym(x.rest)

ctor(::SymDecl) = :SymbolicVariable
ctor(x::NamedDecl) = ctor(x.rest)
ctor(::FunctionDecl) = :SymbolicFunction
ctor(x::TensorDecl) = ctor(x.rest)
ctor(x::AssumptionsDecl) = :GuardedSymbolicVariable

assumptions(::SymDecl) = []
assumptions(x::NamedDecl) = assumptions(x.rest)
assumptions(x::FunctionDecl) = assumptions(x.rest)
assumptions(x::TensorDecl) = assumptions(x.rest)
assumptions(x::AssumptionsDecl) = x.assumptions

# Reshape is not used by most nodes, but TensorNodes require the output to be given
# the shape matching the specification. For instance if @syms x[1:3, 2:6], we should
# have size(x) = (3, 5)
genreshape(expr, ::SymDecl) = expr
genreshape(expr, x::NamedDecl) = genreshape(expr, x.rest)
genreshape(expr, x::FunctionDecl) = genreshape(expr, x.rest)
genreshape(expr, x::TensorDecl) = let
    shape = tuple(length.(x.ranges)...)
    :(reshape(collect($(expr)), $(shape)))
end
genreshape(expr, x::AssumptionsDecl) = genreshape(expr, x.rest)

# To find out the name, we need to traverse in both directions to make sure that each node can get
# information from parents and children about possible name.
# This is done because the expr tree will always look like NamedDecl -> ... -> TensorDecl -> ... -> SymDecl
# and the TensorDecl node will need to know if it should create names base on a NamedDecl parent or
# based on the SymDecl leaf.
name(x::SymDecl, parentname) = coalesce(parentname, String(x.sym))
name(x::NamedDecl, parentname) = coalesce(name(x.rest, x.name), x.name)
name(x::FunctionDecl, parentname) = name(x.rest, parentname)
name(x::AssumptionsDecl, parentname) = name(x.rest, parentname)
name(x::TensorDecl, parentname) = let
    basename = name(x.rest, parentname)
    # we need to double reverse the indices to make sure that we traverse them in the natural order
    namestensor = map(Iterators.product(x.ranges...)) do ind
        sub = join(map(map_subscripts, ind), "_")
        string(basename, sub)
    end
    return tuple(namestensor[:]...)
    join(namestensor[:], ", ")
end

function parseerror()
    error("Incorrect @syms syntax. Try `@syms x::(real,positive)=>\"x₀\" y() z::complex n::integer` for instance.")
end
