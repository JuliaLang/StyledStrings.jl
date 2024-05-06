# This file is a part of Julia. License is MIT: https://julialang.org/license

module AnnotatedStrings

import Base: @propagate_inbounds, AbstractPipe, TTY, String, cmp, codepoint,
    codeunit, codeunits, convert, copy, eltype, empty, firstindex, get,
    getindex, haskey, in, isvalid, invoke, iterate, join, keys, keys, lastindex,
    length, ncodeunits, merge, parse, pipe_reader, pipe_writer, position, print,
    promote_rule, read, repeat, reverse, seek, seekend, setindex, show, skip,
    truncate, tryparse, write, *, ==

using ..Compat

include(joinpath(dirname(@__DIR__), "compat.jl"))
include("annotated.jl")

include("unicode.jl")
include("basic.jl")
include("io.jl")
include("regex.jl")

function __init__()
    # Method overwriting
    ccall(:jl_generating_output, Cint, ()) == 0 &&
        eval(:(include(joinpath($@__DIR__, "overrides.jl"))))
end

end
