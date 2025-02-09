## ----- Generators
## This is a bit hacky
struct SymbolicGenerator{T <: StaticExpression} <: AbstractSymbolic
    u::T
end

Base.show(io::IO, ex::SymbolicGenerator) = print(io, "symbolic generator")

for fn ∈ (:sum, #:prod by mapreduce
          :map, :filter,
          :Generator
          )
    @eval begin
        Base.$fn(f::T, iter::I) where {T,I<:AbstractSymbolic} =
            SymbolicGenerator(StaticExpression((↓(f), ↓(iter)), Base.$fn))
        Base.$fn(f::Type{T}, iter::I) where {T,I<:AbstractSymbolic} =
            SymbolicGenerator(StaticExpression((↓(f), ↓(iter)), Base.$fn))
        Base.$fn(iter::SymbolicGenerator) =
            SymbolicGenerator(StaticExpression((↓(identity), ↓(iter)), Base.$fn))

    end
end

Base.mapreduce(f, op, iter::AbstractSymbolic, iters...) =
    SymbolicGenerator(StaticExpression((↓(f), ↓(op), ↓(iter), map(↓,iters)...), mapreduce))

# no replace, specify both
# These are very fussy
# must substitute for iterator first then function, if need be
# but that still may not work
function (ex::SymbolicGenerator)(x, p=nothing)
    # two layers
    # iter substitute, then f,
    𝑥,𝑝 = xp(ex)
    u = ↓(ex)
    if 𝑥 != Δ && p != Δ
        xs = NamedTuple{(𝑥, 𝑝)}((x,p))
        u = ↓(u)(xs)
    elseif 𝑥 != Δ # iter is non Δ
        xs = NamedTuple{(𝑥,)}((x,))
        u = u(xs)
        if !isa(p, MISSING)
            𝑥,𝑝 = xp(u)
            if 𝑝 != Δ
                ps = NamedTuple{(𝑝,)}((p,))
                u = ↓(u)(ps)
            end
        end
    elseif 𝑝 != Δ

        ps = NamedTuple{(𝑝,)}((p,))
        u = u(ps)
        𝑥,𝑝 = xp(u)
        if 𝑥 != Δ
            xs = NamedTuple{(𝑥,)}((x,))
            u = ↓(u)(xs)
        end
    end
    if isa(u, AbstractSymbolic)
        expression_is_constant(↓(u)) && (u = u())
        u = u(x,p)
        !isa(u, Number) && expression_is_constant(↓(u)) && (u = u())
        return u

    else
        return u
    end
end
