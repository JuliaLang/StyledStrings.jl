# This file is a part of Julia. License is MIT: https://julialang.org/license

function _join_preserve_annotations(iterator, args...)
    if isconcretetype(eltype(iterator)) && !_isannotated(eltype(iterator)) && !any(_isannotated, args)
        sprint(join, iterator, args...)
    else
        io = AnnotatedIOBuffer()
        join(io, iterator, args...)
        # If we know (from compile time information, or dynamically in the case
        # of iterators with a non-concrete eltype), that the result is annotated
        # in nature, we extract an `AnnotatedString`, otherwise we just extract
        # a plain `String` from `io`.
        if isconcretetype(eltype(iterator)) || !isempty(io.annotations)
            read(seekstart(io), AnnotatedString{String})
        else
            String(take!(io.io))
        end
    end
end

function AnnotatedString(chars::AbstractVector{C}) where {C<:AbstractChar}
    str = if C <: AnnotatedChar
        String(getfield.(chars, :char))
    else
        sprint(sizehint=length(chars)) do io
            for c in chars
                print(io, c)
            end
        end
    end
    props = Tuple{UnitRange{Int}, Pair{Symbol, Any}}[]
    point = 1
    for c in chars
        if c isa AnnotatedChar
            for prop in c.properties
                push!(props, (point:point, prop))
            end
        end
        point += ncodeunits(c)
    end
    AnnotatedString(str, props)
end
