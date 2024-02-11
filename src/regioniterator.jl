# This file is a part of Julia. License is MIT: https://julialang.org/license

struct RegionIterator{S <: AbstractString}
    str::S
    regions::Vector{UnitRange{Int}}
    annotations::Vector{Vector{Pair{Symbol, Any}}}
end

Base.length(si::RegionIterator) = length(si.regions)

Base.@propagate_inbounds function Base.iterate(si::RegionIterator, i::Integer=1)
    if i <= length(si.regions)
        @inbounds ((SubString(si.str, si.regions[i]), si.annotations[i]), i+1)
    end
end

Base.eltype(::RegionIterator{S}) where { S <: AbstractString} =
    Tuple{SubString{S}, Vector{Pair{Symbol, Any}}}

"""
    eachregion(s::AnnotatedString{S})
    eachregion(s::SubString{AnnotatedString{S}})

Identify the contiguous substrings of `s` with a constant annotations, and return
an iterator which provides each substring and the applicable annotations as a
`Tuple{SubString{S}, Vector{Pair{Symbol, Any}}}`.

# Examples

```jldoctest
julia> collect(StyledStrings.eachregion(Base.AnnotatedString(
           "hey there", [(1:3, :face => :bold), (5:9, :face => :italic)])))
3-element Vector{Tuple{SubString{String}, Vector{Pair{Symbol, Any}}}}:
 ("hey", [:face => :bold])
 (" ", [])
 ("there", [:face => :italic])
```
"""
function eachregion(s::AnnotatedString, region::UnitRange{Int}=firstindex(s):lastindex(s))
    isempty(s) || isempty(region) &&
        return RegionIterator(s, Vector{UnitRange{Int}}(), Vector{Vector{Pair{Symbol, Any}}}())
    regions = Vector{UnitRange{Int}}()
    annots = Vector{Vector{Pair{Symbol, Any}}}()
    changepoints = filter(c -> c in region,
                          Iterators.flatten((first(region), nextind(s, last(region)))
                                            for region in first.(s.annotations)) |>
                                                unique |> sort)
    isempty(changepoints) &&
        return RegionIterator(s.string, [region], [map(last, annotations(s, first(region)))])
    function registerchange!(start, stop)
        push!(regions, start:stop)
        push!(annots, map(last, annotations(s, start)))
    end
    if first(region) < first(changepoints)
        registerchange!(first(region), prevind(s, first(changepoints)))
    end
    for (start, stop) in zip(changepoints, changepoints[2:end])
        registerchange!(start, prevind(s, stop))
    end
    if last(changepoints) <= last(region)
        registerchange!(last(changepoints), last(region))
    end
    RegionIterator(s.string, regions, annots)
end

function eachregion(s::SubString{<:AnnotatedString}, region::UnitRange{Int}=firstindex(s):lastindex(s))
    if isempty(s)
        RegionIterator(s, Vector{UnitRange{Int}}(), Vector{Vector{Pair{Symbol, Any}}}())
    else
        eachregion(s.string, first(region)+s.offset:last(region)+s.offset)
    end
end
