# This file is a part of Julia. License is MIT: https://julialang.org/license

module StyledStrings

import Base: convert, merge, show, print, write

export @styled_str

include("compat.jl")
using .Compat

const ncodeunits = Compat.ncodeunits

include("strings/strings.jl")
include("terminfo.jl")

import .AnnotatedStrings: AnnotatedString, AnnotatedChar, annotations,
    annotate!, annotatedstring, AnnotatedIOBuffer

include("faces.jl")
include("regioniterator.jl")
include("io.jl")
include("styledmarkup.jl")
include("legacy.jl")

using .StyledMarkup

function __init__()
    term_env = get(ENV, "TERM", @static Sys.iswindows() ? "" : "dumb")
    global current_terminfo = load_terminfo(term_env)
    userfaces = joinpath(first(DEPOT_PATH), "config", "faces.toml")
    isfile(userfaces) && loaduserfaces!(userfaces)
    Legacy.load_env_colors!()
end

if generating_output()
    include("precompile.jl")
end

end
