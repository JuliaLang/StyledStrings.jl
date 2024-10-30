# This file is a part of Julia. License is MIT: https://julialang.org/license

_isannotated(S::Type) = S != Union{} && (S <: AnnotatedString || S <: AnnotatedChar)
_isannotated(s) = _isannotated(typeof(s))

# actually from substring.jl
_isannotated(::SubString{T}) where {T} = _isannotated(T)

# The following meta-programming will generate code like:
#   Base.:*(s_annot::AnnotatedString, s::Union{AbstractChar,AbstractString}...) = annotatedstring(s_annot, s...)
#   Base.:*(s1::Union{AbstractChar,AbstractString}, s_annot::AnnotatedString, s::Union{AbstractChar,AbstractString}...) = annotatedstring(s1, s_annot, s...)
# and so on.
for i in 0:31
    front = [Symbol('s', j) for j in 1:i]
    front_typed = [:($s::Union{AbstractChar,AbstractString}) for s in front]
    @eval function Base.:*($(front_typed...), s_annot::AnnotatedString, s::Union{AbstractChar,AbstractString}...)
        annotatedstring($(front...), s_annot, s...)
    end
end

# # From io.jl
# join(iterator) = _join_preserve_annotations(iterator)
# join(iterator, delim) = _join_preserve_annotations(iterator, delim)
# join(iterator, delim, last) = _join_preserve_annotations(iterator, delim, last)
