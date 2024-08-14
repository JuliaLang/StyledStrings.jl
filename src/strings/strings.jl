# This file is a part of Julia. License is MIT: https://julialang.org/license

module AnnotatedStrings

import Base: @propagate_inbounds, AbstractPipe, IteratorSize, TTY, String, cmp,
    codepoint, codeunit, codeunits, compile, convert, copy, eachmatch, eltype,
    empty, firstindex, get, getindex, haskey, in, isvalid, invoke, iterate,
    keys, keys, lastindex, length, ncodeunits, match, merge, parse, pipe_reader,
    pipe_writer, position, print, promote_rule, read, repeat, reverse, seek,
    seekend, setindex, show, skip, truncate, tryparse, write, ==

using ..Compat

# It's surprisingly annoying to clone a substring,
# thanks to that pesky inner constructor.
macro raw_substring(T, str, offset, ncu)
    esc(Expr(:new, Expr(:curly, :SubString, T), str, offset, ncu))
end

include(joinpath(dirname(@__DIR__), "compat.jl"))
include("annotated.jl")

include("unicode.jl")
include("basic.jl")
include("io.jl")
include("regex.jl")

end
