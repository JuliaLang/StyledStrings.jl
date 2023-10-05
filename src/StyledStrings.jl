# This file is a part of Julia. License is MIT: https://julialang.org/license

module StyledStrings

import Base: TaggedString, TaggedChar, textproperties, textproperty!,
    convert, merge, show, print, write

export @styled_str
public Face, addface!, SimpleColor

include("faces.jl")
include("regioniterator.jl")
include("io.jl")
include("stylemacro.jl")

function __init__()
    userfaces = joinpath(first(DEPOT_PATH), "config", "faces.toml")
    isfile(userfaces) && loadfaces!(Base.parsed_toml(userfaces))
end

end
