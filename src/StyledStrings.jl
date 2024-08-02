# This file is a part of Julia. License is MIT: https://julialang.org/license

module StyledStrings

using Base: AnnotatedString, AnnotatedChar, annotations, annotate!, annotatedstring,
      ScopedValue, with, @with

export @styled_str
public Face, addface!, withfaces, styled, SimpleColor

include("faces.jl")
include("regioniterator.jl")
include("io.jl")
include("styledmarkup.jl")
include("legacy.jl")

using .StyledMarkup

function __init__()
    if !isempty(DEPOT_PATH)
        userfaces = joinpath(first(DEPOT_PATH), "config", "faces.toml")
        isfile(userfaces) && loaduserfaces!(userfaces)
    end
    Legacy.load_env_colors!()
end

if Base.generating_output()
    include("precompile.jl")
end

end
