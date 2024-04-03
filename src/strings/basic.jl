# This file is a part of Julia. License is MIT: https://julialang.org/license

function (*)(s1::Union{AbstractChar, AbstractString}, ss::Union{AbstractChar, AbstractString}...)
    if _isannotated(s1) || any(_isannotated, ss)
        annotatedstring(s1, ss...)
    else
        string(s1, ss...)
    end
end

_isannotated(S::Type) = S != Union{} && (S <: AnnotatedString || S <: AnnotatedChar)
_isannotated(s) = _isannotated(typeof(s))

# actually from substring.jl
_isannotated(::SubString{T}) where {T} = _isannotated(T)
