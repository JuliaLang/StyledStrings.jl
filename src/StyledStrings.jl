# This file is a part of Julia. License is MIT: https://julialang.org/license

module StyledStrings

import Base: AnnotatedString, AnnotatedChar, annotations, annotate!,
    annotatedstring, convert, merge, show, print, write,
    ScopedValue, with, @with

export @styled_str
public Face, addface!, SimpleColor

include("faces.jl")
include("regioniterator.jl")
include("io.jl")
include("stylemacro.jl")
include("legacy.jl")

function __init__()
    userfaces = joinpath(first(DEPOT_PATH), "config", "faces.toml")
    isfile(userfaces) && loaduserfaces!(userfaces)
    Legacy.load_env_colors!()
end

if Base.generating_output()
    include("precompile.jl")
end

end
