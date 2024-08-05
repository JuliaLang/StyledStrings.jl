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

struct AnnotatedRegexMatchIterator{S <: AnnotatedString}
    regex::Regex
    string::S
    overlap::Bool
end

compile(itr::AnnotatedRegexMatchIterator) = (compile(itr.regex); itr)
eltype(::Type{AnnotatedRegexMatchIterator{S}}) where {S} = AnnotatedRegexMatch{S}
IteratorSize(::Type{<:AnnotatedRegexMatchIterator}) = SizeUnknown()

function iterate(itr::AnnotatedRegexMatchIterator, (offset,prevempty)=(1,false))
    opts_nonempty = UInt32(Base.PCRE.ANCHORED | Base.PCRE.NOTEMPTY_ATSTART)
    while true
        mat = match(itr.regex, itr.string, offset,
                    prevempty ? opts_nonempty : UInt32(0))

        if mat === nothing
            if prevempty && offset <= sizeof(itr.string)
                offset = nextind(itr.string, offset)
                prevempty = false
                continue
            else
                break
            end
        else
            if itr.overlap
                if !isempty(mat.match)
                    offset = nextind(itr.string, mat.offset)
                else
                    offset = mat.offset
                end
            else
                offset = mat.offset + ncodeunits(mat.match)
            end
            return (mat, (offset, isempty(mat.match)))
        end
    end
    nothing
end

eachmatch(re::Regex, str::AnnotatedString; overlap = false) =
    AnnotatedRegexMatchIterator(re, str, overlap)
