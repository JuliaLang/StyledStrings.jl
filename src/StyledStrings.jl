# This file is a part of Julia. License is MIT: https://julialang.org/license

module StyledStrings

using PrecompileTools: @compile_workload, @recompile_invalidations

export @styled_str

include("compat.jl")
using .Compat

const ncodeunits = Compat.ncodeunits

@recompile_invalidations include("strings/strings.jl")
include("terminfo.jl")

using .AnnotatedStrings: AnnotatedString, AnnotatedChar, annotations,
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
    if !isempty(DEPOT_PATH)
        userfaces = joinpath(first(DEPOT_PATH), "config", "faces.toml")
        isfile(userfaces) && loaduserfaces!(userfaces)
    end
    Legacy.load_env_colors!()
end

@compile_workload include("precompile.jl")

@compile_workload begin
    "a" * "b"
    "a" * "b" * "c"
    "a" * "b" * "c" * "d"
    join(["a", "b", "c"])
    join(["a", 'b'])
    join(("a", "b", "c"))
    join((Char(i) for i in 63:65))
    join(["a", "b", "c"], ", ")
    join(["a", 'b'], ", ")
    join(("a", "b", "c"), ", ")
    join((Char(i) for i in 63:65), ", ")
    join(["a", "b", "c"], ", ", ", and")
    join(["a", 'b'], ", ", ", and")
    join(("a", "b", "c"), ", ", ", and")
    join((Char(i) for i in 63:65), ", ", ", and")
end

end
