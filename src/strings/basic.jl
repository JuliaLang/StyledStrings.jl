# This file is a part of Julia. License is MIT: https://julialang.org/license

_isannotated(S::Type) = S != Union{} && (S <: AnnotatedString || S <: AnnotatedChar)
_isannotated(s) = _isannotated(typeof(s))

# actually from substring.jl
_isannotated(::SubString{T}) where {T} = _isannotated(T)

# NOTE Too invalidating:

# function (*)(s1::Union{AnnotatedChar, AnnotatedString}, s2::Union{AbstractChar, AbstractString})
#     annotatedstring(s1, s2)
# end

# function (*)(s1::Union{AbstractChar, AbstractString}, s2::Union{AnnotatedChar, AnnotatedString})
#     annotatedstring(s1, s2)
# end

# function (*)(s1::Union{AnnotatedChar, AnnotatedString}, s2::Union{AnnotatedChar, AnnotatedString})
#     annotatedstring(s1, s2)
# end

# function (*)(s1::Union{AbstractChar, AbstractString}, s2::Union{AbstractChar, AbstractString}, ss::Union{AbstractChar, AbstractString}...)
#     if _isannotated(s1) || _isannotated(s2) || any(_isannotated, ss)
#         annotatedstring(s1, s2, ss...)
#     else
#         string(s1, s2, ss...)
#     end
# end

# # From io.jl
# join(iterator) = _join_preserve_annotations(iterator)
# join(iterator, delim) = _join_preserve_annotations(iterator, delim)
# join(iterator, delim, last) = _join_preserve_annotations(iterator, delim, last)
