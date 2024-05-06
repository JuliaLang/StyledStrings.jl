## From basic.jl

function (*)(s1::Union{AbstractChar, AbstractString}, ss::Union{AbstractChar, AbstractString}...)
    if _isannotated(s1) || any(_isannotated, ss)
        annotatedstring(s1, ss...)
    else
        string(s1, ss...)
    end
end

# From io.jl

join(iterator) = _join_preserve_annotations(iterator)
join(iterator, delim) = _join_preserve_annotations(iterator, delim)
join(iterator, delim, last) = _join_preserve_annotations(iterator, delim, last)
