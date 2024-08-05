# This file is a part of Julia. License is MIT: https://julialang.org/license

@static if VERSION < v"1.6"
    struct AnnotatedRegexMatch{S<:AbstractString}
        match::SubString{S}
        captures::Vector{Union{Nothing, SubString{S}}}
        offset::Int64
        offsets::Vector{Int64}
        regex::Regex
    end
else
    struct AnnotatedRegexMatch{S<:AbstractString} <: AbstractMatch
        match::SubString{S}
        captures::Vector{Union{Nothing, SubString{S}}}
        offset::Int64
        offsets::Vector{Int64}
        regex::Regex
    end
end

function _annotatedmatch(m::RegexMatch, str::AnnotatedString{S}) where {S<:AbstractString}
    AnnotatedRegexMatch{AnnotatedString{S}}(
        @raw_substring(AnnotatedString{S}, str, m.match.offset, m.match.ncodeunits),
        Union{Nothing,SubString{AnnotatedString{S}}}[
            if !isnothing(cap)
                @raw_substring(AnnotatedString{S},
                               str, cap.offset, cap.ncodeunits)
            end for cap in m.captures],
        m.offset, m.offsets, m.regex)
end

function match(re::Regex, str::AnnotatedString)
    m = match(re, str.string)
    if !isnothing(m)
        _annotatedmatch(m, str)
    end
end

function match(re::Regex, str::AnnotatedString, idx::Integer, add_opts::UInt32=UInt32(0))
    m = match(re, str.string, idx, add_opts)
    if !isnothing(m)
        _annotatedmatch(m, str)
    end
end
