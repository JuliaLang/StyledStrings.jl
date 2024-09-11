# This file is a part of Julia. License is MIT: https://julialang.org/license

struct RegionIterator{S <: AbstractString}
    str::S
    regions::Vector{UnitRange{Int}}
    annotations::Vector{Vector{@NamedTuple{label::Symbol, value::Any}}}
end

Base.length(si::RegionIterator) = length(si.regions)

Base.@propagate_inbounds function Base.iterate(si::RegionIterator, i::Integer=1)
    if i <= length(si.regions)
        @inbounds ((SubString(si.str, si.regions[i]), si.annotations[i]), i+1)
    end
end

Base.eltype(::RegionIterator{S}) where { S <: AbstractString} =
    Tuple{SubString{S}, Vector{@NamedTuple{label::Symbol, value::Any}}}

"""
    eachregion(s::AnnotatedString{S})
    eachregion(s::SubString{AnnotatedString{S}})

Identify the contiguous substrings of `s` with a constant annotations, and return
an iterator which provides each substring and the applicable annotations as a
`Tuple{SubString{S}, Vector{@NamedTuple{label::Symbol, value::Any}}}`.

# Examples

```jldoctest
julia> collect(StyledStrings.eachregion(AnnotatedString(
           "hey there", [(1:3, :face, :bold), (5:9, :face, :italic)])))
3-element Vector{Tuple{SubString{String}, Vector{@NamedTuple{label::Symbol, value}}}}:
 ("hey", [@NamedTuple{label::Symbol, value}((:face, :bold))])
 (" ", [])
 ("there", [@NamedTuple{label::Symbol, value}((:face, :italic))])
```
"""
function eachregion(s::AnnotatedString, subregion::UnitRange{Int}=firstindex(s):lastindex(s))
    isempty(s) || isempty(subregion) &&
        return RegionIterator(s.string, UnitRange{Int}[], Vector{@NamedTuple{label::Symbol, value::Any}}[])
    events = annotation_events(s, subregion)
    isempty(events) && return RegionIterator(s.string, [subregion], [@NamedTuple{label::Symbol, value::Any}[]])
    annotvals = @NamedTuple{label::Symbol, value::Any}[
        (; label, value) for (; label, value) in annotations(s)]
    regions = Vector{UnitRange{Int}}()
    annots = Vector{Vector{@NamedTuple{label::Symbol, value::Any}}}()
    pos = first(events).pos
    if pos > first(subregion)
        push!(regions, thisind(s, first(subregion)):prevind(s, pos))
        push!(annots, [])
    end
    activelist = Int[]
    for event in events
        if event.pos != pos
            push!(regions, pos:prevind(s, event.pos))
            push!(annots, annotvals[activelist])
            pos = event.pos
        end
        if event.active
            insert!(activelist, searchsortedfirst(activelist, event.index), event.index)
        else
            deleteat!(activelist, searchsortedfirst(activelist, event.index))
        end
    end
    if last(events).pos < nextind(s, last(subregion))
        push!(regions, last(events).pos:thisind(s, last(subregion)))
        push!(annots, [])
    end
    RegionIterator(s.string, regions, annots)
end

function eachregion(s::SubString{<:AnnotatedString}, pos::UnitRange{Int}=firstindex(s):lastindex(s))
    if isempty(s)
        RegionIterator(s.string, Vector{UnitRange{Int}}(), Vector{Vector{@NamedTuple{label::Symbol, value::Any}}}())
    else
        eachregion(s.string, first(pos)+s.offset:last(pos)+s.offset)
    end
end

"""
    annotation_events(string::AbstractString, annots::Vector{@NamedTuple{region::UnitRange{Int}, label::Symbol, value::Any}}, subregion::UnitRange{Int})
    annotation_events(string::AnnotatedString, subregion::UnitRange{Int})

Find all annotation "change events" that occur within a `subregion` of `annots`,
with respect to `string`. When `string` is styled, `annots` is inferred.

Each change event is given in the form of a `@NamedTuple{pos::Int, active::Bool,
index::Int}` where `pos` is the position of the event, `active` is a boolean
indicating whether the annotation is being activated or deactivated, and `index`
is the index of the annotation in question.
"""
function annotation_events(s::AbstractString, annots::Vector{@NamedTuple{region::UnitRange{Int}, label::Symbol, value::Any}}, subregion::UnitRange{Int})
    events = Vector{NamedTuple{(:pos, :active, :index), Tuple{Int, Bool, Int}}}() # Position, Active?, Annotation index
    for (i, (; region)) in enumerate(annots)
        if !isempty(intersect(subregion, region))
            start, stop = max(first(subregion), first(region)), min(last(subregion), last(region))
            start <= stop || continue # Currently can't handle empty regions
            push!(events, (pos=thisind(s, start), active=true, index=i))
            push!(events, (pos=nextind(s, stop), active=false, index=i))
        end
    end
    sort(events, by=e -> e.pos)
end

annotation_events(s::AnnotatedString, subregion::UnitRange{Int}) =
    annotation_events(s.string, annotations(s), subregion)
