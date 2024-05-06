# This file is a part of Julia. License is MIT: https://julialang.org/license

function _join_preserve_annotations(iterator, args...)
    if _isannotated(eltype(iterator)) || any(_isannotated, args)
        io = AnnotatedIOBuffer()
        join(io, iterator, args...)
        read(seekstart(io), AnnotatedString{String})
    else
        sprint(join, iterator, args...)
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
