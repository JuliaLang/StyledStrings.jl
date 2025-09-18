# This file is a part of Julia. License is MIT: https://julialang.org/license

# StyledStrings provides a new way of handling colored output, however the
# ~decade of colored printing in Julia before now has left us with a legacy,
# and we should try to maintain compatibility as much as reasonable possible.

module Legacy

using ..StyledStrings: SimpleColor, Face, loadface!, face!, AnnotatedIOBuffer, annotatedstring

"""
    legacy_color(color::Union{String, Symbol, Int})

Attempt to obtain a `SimpleColor` for a "legacy" color value `color`.

When this is not possible, `nothing` is returned.
"""
function legacy_color end

"""
A mapping from 256-color codes indicies to 8-bit colours.
"""
const ANSI_256_COLORS =
    map(SimpleColor,
        [0x000000, 0x800000, 0x008000, 0x808000, 0x000080, 0x800080, 0x008080,
         0xc0c0c0, 0x808080, 0xff0000, 0x00ff00, 0xffff00, 0x0000ff, 0xff00ff,
         0x00ffff, 0xffffff, 0x000000, 0x00005f, 0x000087, 0x0000af, 0x0000d7,
         0x0000ff, 0x005f00, 0x005f5f, 0x005f87, 0x005faf, 0x005fd7, 0x005fff,
         0x008700, 0x00875f, 0x008787, 0x0087af, 0x0087d7, 0x0087ff, 0x00af00,
         0x00af5f, 0x00af87, 0x00afaf, 0x00afd7, 0x00afff, 0x00d700, 0x00d75f,
         0x00d787, 0x00d7af, 0x00d7d7, 0x00d7ff, 0x00ff00, 0x00ff5f, 0x00ff87,
         0x00ffaf, 0x00ffd7, 0x00ffff, 0x5f0000, 0x5f005f, 0x5f0087, 0x5f00af,
         0x5f00d7, 0x5f00ff, 0x5f5f00, 0x5f5f5f, 0x5f5f87, 0x5f5faf, 0x5f5fd7,
         0x5f5fff, 0x5f8700, 0x5f875f, 0x5f8787, 0x5f87af, 0x5f87d7, 0x5f87ff,
         0x5faf00, 0x5faf5f, 0x5faf87, 0x5fafaf, 0x5fafd7, 0x5fafff, 0x5fd700,
         0x5fd75f, 0x5fd787, 0x5fd7af, 0x5fd7d7, 0x5fd7ff, 0x5fff00, 0x5fff5f,
         0x5fff87, 0x5fffaf, 0x5fffd7, 0x5fffff, 0x870000, 0x87005f, 0x870087,
         0x8700af, 0x8700d7, 0x8700ff, 0x875f00, 0x875f5f, 0x875f87, 0x875faf,
         0x875fd7, 0x875fff, 0x878700, 0x87875f, 0x878787, 0x8787af, 0x8787d7,
         0x8787ff, 0x87af00, 0x87af5f, 0x87af87, 0x87afaf, 0x87afd7, 0x87afff,
         0x87d700, 0x87d75f, 0x87d787, 0x87d7af, 0x87d7d7, 0x87d7ff, 0x87ff00,
         0x87ff5f, 0x87ff87, 0x87ffaf, 0x87ffd7, 0x87ffff, 0xaf0000, 0xaf005f,
         0xaf0087, 0xaf00af, 0xaf00d7, 0xaf00ff, 0xaf5f00, 0xaf5f5f, 0xaf5f87,
         0xaf5faf, 0xaf5fd7, 0xaf5fff, 0xaf8700, 0xaf875f, 0xaf8787, 0xaf87af,
         0xaf87d7, 0xaf87ff, 0xafaf00, 0xafaf5f, 0xafaf87, 0xafafaf, 0xafafd7,
         0xafafff, 0xafd700, 0xafd75f, 0xafd787, 0xafd7af, 0xafd7d7, 0xafd7ff,
         0xafff00, 0xafff5f, 0xafff87, 0xafffaf, 0xafffd7, 0xafffff, 0xd70000,
         0xd7005f, 0xd70087, 0xd700af, 0xd700d7, 0xd700ff, 0xd75f00, 0xd75f5f,
         0xd75f87, 0xd75faf, 0xd75fd7, 0xd75fff, 0xd78700, 0xd7875f, 0xd78787,
         0xd787af, 0xd787d7, 0xd787ff, 0xd7af00, 0xd7af5f, 0xd7af87, 0xd7afaf,
         0xd7afd7, 0xd7afff, 0xd7d700, 0xd7d75f, 0xd7d787, 0xd7d7af, 0xd7d7d7,
         0xd7d7ff, 0xd7ff00, 0xd7ff5f, 0xd7ff87, 0xd7ffaf, 0xd7ffd7, 0xd7ffff,
         0xff0000, 0xff005f, 0xff0087, 0xff00af, 0xff00d7, 0xff00ff, 0xff5f00,
         0xff5f5f, 0xff5f87, 0xff5faf, 0xff5fd7, 0xff5fff, 0xff8700, 0xff875f,
         0xff8787, 0xff87af, 0xff87d7, 0xff87ff, 0xffaf00, 0xffaf5f, 0xffaf87,
         0xffafaf, 0xffafd7, 0xffafff, 0xffd700, 0xffd75f, 0xffd787, 0xffd7af,
         0xffd7d7, 0xffd7ff, 0xffff00, 0xffff5f, 0xffff87, 0xffffaf, 0xffffd7,
         0xffffff, 0x080808, 0x121212, 0x1c1c1c, 0x262626, 0x303030, 0x3a3a3a,
         0x444444, 0x4e4e4e, 0x585858, 0x626262, 0x6c6c6c, 0x767676, 0x808080,
         0x8a8a8a, 0x949494, 0x9e9e9e, 0xa8a8a8, 0xb2b2b2, 0xbcbcbc, 0xc6c6c6,
         0xd0d0d0, 0xdadada, 0xe4e4e4, 0xeeeeee])

legacy_color(color256::Int) = get(ANSI_256_COLORS, color256+1, nothing)

"""
A list of all named colors recognised, including both the old `light_*` and new
`bright_*` named colors.
"""
const NAMED_COLORS =
    (:black, :red, :green, :yellow, :blue, :magenta, :cyan, :white,
     :bright_black, :grey, :gray, :bright_red, :bright_green, :bright_yellow,
     :bright_blue, :bright_magenta, :bright_cyan, :bright_white, :light_black,
     :light_red, :light_green, :light_yellow, :light_blue, :light_magenta,
     :light_cyan, :light_white)

"""
A mapping from old named colours to the new names, specifically from `light_*`
to `bright_*`.
"""
const RENAMED_COLORS = Dict{Symbol, Symbol}(
    :light_black   => :bright_black,
    :light_red     => :bright_red,
    :light_green   => :bright_green,
    :light_yellow  => :bright_yellow,
    :light_blue    => :bright_blue,
    :light_magenta => :bright_magenta,
    :light_cyan    => :bright_cyan,
    :light_white   => :bright_white)

legacy_color(color::Symbol) =
    if color in NAMED_COLORS
        SimpleColor(get(RENAMED_COLORS, color, color))
    end

function legacy_color(color::String)
    namedcolours = map(String, NAMED_COLORS)
    if color in namedcolours
        legacy_color(Symbol(color))
    elseif 0 <= (color256 = something(tryparse(Int, color), -1)) <= 255
        legacy_color(color256)
    else
        nothing
    end
end

"""
    load_env_colors!()

Try to emulate the effect of the various `*_color()` functions of `Base`, by
loading any specified colours as foregrounds of the relevant faces.
"""
function load_env_colors!()
    for (fname, envkey) in ((:error,     "JULIA_ERROR_COLOR"),
                            (:warn,      "JULIA_WARN_COLOR"),
                            (:info,      "JULIA_INFO_COLOR"),
                            (:log_debug, "JULIA_DEBUG_COLOR"))
        if haskey(ENV, envkey)
            ecolor = legacy_color(ENV[envkey])
            if !isnothing(ecolor)
                loadface!(fname => Face(foreground = ecolor))
            end
        end
    end
end

# Part of the inference barrier around `Base.printstyled`
function Base.AnnotatedDisplay.styled_print(io::AnnotatedIOBuffer, @nospecialize(msg::Tuple), @nospecialize(kwargs::Base.Pairs))
    str = annotatedstring(msg...)
    for attr in (:bold, :italic, :underline)
        get(kwargs, attr, false)::Bool && face!(str, attr)
    end
    get(kwargs, :reverse, false)::Bool && face!(str, :inverse)
    color = get(kwargs, :color, :normal)::Symbol
    color !== :normal && face!(str, Face(foreground=legacy_color(color)))
    write(io, str)
    nothing
end

end
