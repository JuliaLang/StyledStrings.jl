
# TODO: If/when we have `AnnotatedIO`, we can revisit this and
# implement it more nicely.
function join_annotated(iterator, delim="", last=delim)
    xs = zip(iterator, Iterators.repeated(delim)) |> Iterators.flatten |> collect
    xs = xs[1:end-1]
    if length(xs) > 1
        xs[end-1] = last
    end
    annotatedstring(xs...)::AnnotatedString{String}
end

function _join_maybe_annotated(args...)
    if any(function (arg)
               t = eltype(arg)
               !(t == Union{}) && (t <: AnnotatedString || t <: AnnotatedChar)
           end, args)
        join_annotated(args...)
    else
        sprint(join, args...)
    end
end

join(iterator) = _join_maybe_annotated(iterator)
join(iterator, delim) = _join_maybe_annotated(iterator, delim)
join(iterator, delim, last) = _join_maybe_annotated(iterator, delim, last)

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
