# This file is a part of Julia. License is MIT: https://julialang.org/license

module StyledStrings

# While these are imported from Base, we claim them as part of the `StyledStrings` API.
export AnnotatedString, AnnotatedChar, AnnotatedIOBuffer, annotations, annotate!, annotatedstring

export @styled_str

include("compat.jl")
using .Compat

const ncodeunits = Compat.ncodeunits

include("strings/strings.jl")
include("terminfo.jl")

using .AnnotatedStrings: AnnotatedString, AnnotatedChar, AnnotatedIOBuffer, annotations, annotate!, annotatedstring,

include("faces.jl")
include("regioniterator.jl")
include("io.jl")
include("styledmarkup.jl")
include("legacy.jl")

using .StyledMarkup

const HAVE_LOADED_CUSTOMISATIONS = Base.Threads.Atomic{Bool}(false)

"""
    load_customisations!(; force::Bool=false)

Load customisations from the user's `faces.toml` file, if it exists as well as
the current environment.

This function should be called before producing any output in situations where
the user's customisations should be considered. This is called automatically
when printing text or HTML output, and when calling `withfaces`, but may need
to be called manually in unusual situations.

Unless `force` is set, customisations are only applied when this function is
called for the first time, and subsequent calls are a no-op.
"""
function load_customisations!(; force::Bool=false)
    !force && HAVE_LOADED_CUSTOMISATIONS[] && return
    (function ()
         @noinline
         if !isempty(DEPOT_PATH)
             userfaces = joinpath(first(DEPOT_PATH), "config", "faces.toml")
             isfile(userfaces) && loaduserfaces!(userfaces)
         end
         Legacy.load_env_colors!()
         HAVE_LOADED_CUSTOMISATIONS[] = true
     end)()
    nothing
end

function __init__()
    term_env = get(ENV, "TERM", @static Sys.iswindows() ? "" : "dumb")
    global current_terminfo = load_terminfo(term_env)
    load_customisations!()
end

include("precompile.jl")

end
