module AnnotatedStrings

import Base: @propagate_inbounds, TTY, String, cmp, codepoint, codeunit,
    codeunits, convert, eltype, empty, firstindex, get, getindex, haskey, in,
    isvalid, iterate, join, keys, keys, lastindex, length, ncodeunits, merge,
    parse, print, promote_rule, read, repeat, reverse, show, tryparse, write, *, ==

include(joinpath(dirname(@__DIR__), "compat.jl"))
include("annotated.jl")

function __init__()
    # Method overwriting
    eval(:(include(joinpath($@__DIR__, "basic.jl"))))
    eval(:(include(joinpath($@__DIR__, "io.jl"))))
    eval(:(include(joinpath($@__DIR__, "regex.jl"))))
end

end
