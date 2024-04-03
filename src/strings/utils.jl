# This file is a part of Julia. License is MIT: https://julialang.org/license

function lpad(
    s::Union{AbstractChar,AbstractString},
    n::Integer,
    p::Union{AbstractChar,AbstractString}=' ',
)
    stringfn = if any(isa.((s, p), Union{AnnotatedString, AnnotatedChar, SubString{<:AnnotatedString}}))
        annotatedstring else string end
    n = Int(n)::Int
    m = signed(n) - Int(textwidth(s))::Int
    m ≤ 0 && return stringfn(s)
    l = textwidth(p)
    q, r = divrem(m, l)
    r == 0 ? stringfn(p^q, s) : stringfn(p^q, first(p, r), s)
end

function rpad(
    s::Union{AbstractChar,AbstractString},
    n::Integer,
    p::Union{AbstractChar,AbstractString}=' ',
)
    stringfn = if any(isa.((s, p), Union{AnnotatedString, AnnotatedChar, SubString{<:AnnotatedString}}))
        annotatedstring else string end
    n = Int(n)::Int
    m = signed(n) - Int(textwidth(s))::Int
    m ≤ 0 && return stringfn(s)
    l = textwidth(p)
    q, r = divrem(m, l)
    r == 0 ? stringfn(s, p^q) : stringfn(s, p^q, first(p, r))
end
