# This file is a part of Julia. License is MIT: https://julialang.org/license

_isannotated(S::Type) = S != Union{} && (S <: AnnotatedString || S <: AnnotatedChar)
_isannotated(s) = _isannotated(typeof(s))

# actually from substring.jl
_isannotated(::SubString{T}) where {T} = _isannotated(T)
