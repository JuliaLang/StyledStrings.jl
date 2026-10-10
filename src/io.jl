# This file is a part of Julia. License is MIT: https://julialang.org/license

# This isn't so much type piracy as type privateering 😉

"""
A mapping between ANSI named colours and indices in the standard 256-color
table. The standard colors are 0-7, and high intensity colors 8-15.

The high intensity colors are prefixed by "bright_". The "bright_black" color is
given two aliases: "grey" and "gray".
"""
const ANSI_4BIT_COLORS = IdDict{Face, UInt8}(
    face"black"          => 0,
    face"red"            => 1,
    face"green"          => 2,
    face"yellow"         => 3,
    face"blue"           => 4,
    face"magenta"        => 5,
    face"cyan"           => 6,
    face"white"          => 7,
    face"bright_black"   => 8,
    face"grey"           => 8,
    face"gray"           => 8,
    face"bright_red"     => 9,
    face"bright_green"   => 10,
    face"bright_yellow"  => 11,
    face"bright_blue"    => 12,
    face"bright_magenta" => 13,
    face"bright_cyan"    => 14,
    face"bright_white"   => 15)

const FGBG_FACES =
    (foreground = FACES.pool[:foreground],
     background = FACES.pool[:background])

# The low `nb` bytes of `word`, in memory order, in one call
function writebytes(io::IO, word::UInt64, nb::Integer)
    bytes = Ref(htol(word))
    GC.@preserve bytes unsafe_write(io, Ptr{UInt8}(pointer_from_objref(bytes)), nb)
end

# The decimal digits of `num` packed little-endian, first digit lowest, and their count
function packdigits(num::UInt8)
    hundreds, rest = divrem(num, UInt8(100))
    tens, ones = divrem(rest, UInt8(10))
    ndigits = 0x1 + (num >= UInt8(10)) + (num >= UInt8(100))
    zero = UInt64(UInt8('0'))
    ((zero + hundreds) | (zero + tens) << 8 | (zero + ones) << 16) >> (8 * (0x3 - ndigits)), ndigits
end

function writedigits(io::IO, num::UInt8, suffix::Char = '\0')
    digits, ndigits = packdigits(num)
    writebytes(io, digits | UInt64(suffix) << (8 * ndigits), ndigits + (suffix != '\0'))
end

"""
    ansi_4bit(code::UInt8, background::Bool=false)

Provide the color code (30-37, 40-47, 90-97, 100-107) for `code` (0–15).

When `background` is set the background variant will be provided, otherwise
the provided code is for setting the foreground color.
"""
function ansi_4bit(code::UInt8, background::Bool=false)
    code >= UInt8(8) && (code += UInt8(52))
    background && (code += UInt8(10))
    code + UInt8(30)
end

"""
    termcolor8bit(io::IO, color::RGBTuple, category::Char)

Print to `io` the best 8-bit SGR color code that sets the `category` color to
be close to `color`.
"""
function termcolor8bit(io::IO, (; r, g, b)::RGBTuple, category::Char)
    # RGB values are mapped to a 6x6x6 "colour cube", which (mapped to
    # 24-bit colour space), jumps up from a black level of 0 in each
    # component to 95, then takes 4 steps of 40 to reach 255.
    function cdistsq(r2, g2, b2) # The squared "redmean" colour distance function
        rr = (r + r2) / 2
        (2 + rr/256) * (r - r2)^2 + 4 * (g - g2)^2 + (2 + (255 - rr)/256) * (b - b2)^2
    end
    to6cube(value) = if value < 48 0 elseif value < 115 1 else (value - 35) ÷ 40 end # Nearest of 0, 95, 135, 175, 215, 255
    from6cube(r6, g6, b6) = 16 + 6^2 * r6 + 6^1 * g6 + 6^0 * b6
    sixcube = (0, 95:40:255...)
    r6cube, g6cube, b6cube = to6cube(r), to6cube(g), to6cube(b)
    rnear, gnear, bnear = sixcube[r6cube+1], sixcube[g6cube+1], sixcube[b6cube+1]
    colorcode = if r == rnear && g == gnear && b == bnear
        from6cube(r6cube, g6cube, b6cube)
    else
        # There aren't many greys in the 6x6x6 colour cube, so the remaining
        # space in the 256-colour range not taken up by the 16 "named" 4-bit
        # colours and the 6 colour cube is used for 24 shades of grey (`8:10:238`).
        # `cdistsq` weights green, so the grey nearest the mean may not be the nearest
        mean_level = min(23, (sum(Int, (r, g, b)) ÷ 3 - 3) ÷ 10)
        grey_level = argmin(l -> cdistsq(8 + 10l, 8 + 10l, 8 + 10l), max(0, mean_level - 1):min(23, mean_level + 1))
        greynear = 8 + 10 * grey_level
        if cdistsq(greynear, greynear, greynear) <= cdistsq(rnear, gnear, bnear)
            16 + 6^3 + grey_level
        else
            from6cube(r6cube, g6cube, b6cube)
        end
    end
    write(io, if category == '3' "\e[38;5;" elseif category == '4' "\e[48;5;" else "\e[58;5;" end)
    writedigits(io, UInt8(colorcode), 'm')
end

"""
    termcolor24bit(io::IO, color::RGBTuple, category::Char)

Print to `io` the 24-bit SGR color code that sets the `category` color to `color`.
"""
function termcolor24bit(io::IO, color::RGBTuple, category::Char)
    write(io, if category == '3' "\e[38;2;" elseif category == '4' "\e[48;2;" else "\e[58;2;" end)
    writedigits(io, color.r, ';')
    writedigits(io, color.g, ';')
    writedigits(io, color.b, 'm')
end

"""
    termcolor(io::IO, color::SimpleColor, category::Char)

Print to `io` the SGR code to set the `category`'s slot to `color`,
where `category` is set as follows:
- `'3'` sets the foreground color
- `'4'` sets the background color
- `'5'` sets the underline color

The color is final, as in a face from `getface`. A base color is written as its
code in `ANSI_4BIT_COLORS`, and any other face resets the color.

An `RGBTuple` is written as 24-bit color when `get_have_truecolor()` returns true.
Otherwise, an 8-bit approximation of it is used.
"""
function termcolor(io::IO, color::SimpleColor, category::Char)
    value = color.value
    if category == '4' # Background
        if value === FGBG_FACES.background
            return termcolor(io, nothing, '4')
        elseif value === FGBG_FACES.foreground
            return print(io, "\e[47m") # Technically not quite[1], but close enough
        end
    elseif value === FGBG_FACES.foreground
        return termcolor(io, nothing, category)
    elseif category == '3' && value === FGBG_FACES.background
        return print(io, "\e[30m") # Technically not quite[1], but close enough
    end
    # [1]: There is no true way to selectively set the fg/bg in the terminal to the
    # bg/fg colour, but with the way most terminals/terminal themes treat white/black
    # we can often get a close result with them.
    if value isa Face
        ansi = get(ANSI_4BIT_COLORS, value, nothing)
        isnothing(ansi) && return termcolor(io, nothing, category)
        if category == '5'
            write(io, "\e[58;5;")
            writedigits(io, ansi, 'm')
        else # The whole sequence fits one word
            digits, ndigits = packdigits(ansi_4bit(ansi, category == '4'))
            writebytes(io, UInt64(0x5b1b) | digits << 16 | UInt64(UInt8('m')) << (16 + 8 * ndigits), ndigits + 3)
        end
    elseif Base.get_have_truecolor()
        termcolor24bit(io, value, category)
    else
        termcolor8bit(io, value, category)
    end
end

"""
    termcolor(io::IO, ::Nothing, category::Char)

Print to `io` the SGR code to reset the color for `category`.
"""
termcolor(io::IO, ::Nothing, category::Char) = # "\e[<category>9m" as one word
    writebytes(io, UInt64(0x5b1b) | UInt64(UInt8(category)) << 16 | UInt64(0x6d39) << 24, 5)

const ANSI_STYLE_CODES = (
    bold_weight = "\e[1m",
    dim_weight = "\e[2m", # Unused
    normal_weight = "\e[22m",
    start_italics = "\e[3m",
    end_italics = "\e[23m",
    start_underline = "\e[4m",
    end_underline = "\e[24m",
    start_reverse = "\e[7m",
    end_reverse = "\e[27m",
    start_strikethrough = "\e[9m",
    end_strikethrough = "\e[29m"
)

function termstyle(io::IO, face::FaceDef, lastface::FaceDef=resolvedef(STANDARD_FACES.default))
    face.foreground === lastface.foreground ||
        termcolor(io, faceproperty(face, :foreground), '3')
    face.background === lastface.background ||
        termcolor(io, faceproperty(face, :background), '4')
    face.weight == lastface.weight || begin
        normal = attrbyte(:weight, :normal)
        if lastface.weight != normal && face.weight != normal
            print(io, ANSI_STYLE_CODES.normal_weight) # Reset before changing
        end
        weight = face.weight
        print(io, if weight < normal
                  get(Base.current_terminfo(), :dim, "")
              elseif weight == normal || isnothingflavour(weight)
                  ANSI_STYLE_CODES.normal_weight
              else
                  ANSI_STYLE_CODES.bold_weight
              end)
    end
    face.slant == lastface.slant ||
        let slanted = face.slant < attrbyte(:slant, :normal) # italic or oblique
            if haskey(Base.current_terminfo(), :enter_italics_mode)
                print(io, ifelse(slanted, ANSI_STYLE_CODES.start_italics, ANSI_STYLE_CODES.end_italics))
            elseif slanted && face.underline_style >= NO_UNDERLINE
                print(io, ANSI_STYLE_CODES.start_underline)
            elseif !slanted && lastface.underline_style >= NO_UNDERLINE
                print(io, ANSI_STYLE_CODES.end_underline)
            end
        end
    # Kitty fancy underlines, see <https://sw.kovidgoyal.net/kitty/underlines>
    # Supported in Kitty, VTE, iTerm2, Alacritty, and Wezterm.
    (face.underline === lastface.underline && face.underline_style == lastface.underline_style) ||
        if haskey(Base.current_terminfo(), :set_underline_style) || get(Base.current_terminfo(), :can_style_underline, false)
            ul, ulstyle = face.underline, face.underline_style
            lastul, lastulstyle = lastface.underline, lastface.underline_style
            # The named styles come before `NO_UNDERLINE`, and the nothing bytes after it
            if ulstyle != lastulstyle && ulstyle < NO_UNDERLINE
                if lastulstyle >= NO_UNDERLINE && ulstyle == attrbyte(:underline, :straight)
                    print(io, ANSI_STYLE_CODES.start_underline)
                else # Kitty numbers the styles from 1 in `ATTRIBUTES.underlines` order
                    print(io, "\e[4:", Char(UInt8('1') + ulstyle), 'm')
                end
            end
            ulcolor(c) = if isnothingflavour(c) FGBG_FACES.foreground else c end # No colour is the text's own
            ulcolor(ul) === ulcolor(lastul) || termcolor(io, SimpleColor(ulcolor(ul)), '5')
            if ulstyle >= NO_UNDERLINE && lastulstyle < NO_UNDERLINE
                print(io, ANSI_STYLE_CODES.end_underline)
            end
        elseif face.underline_style < NO_UNDERLINE
            print(io, ANSI_STYLE_CODES.start_underline)
        elseif haskey(Base.current_terminfo(), :enter_italics_mode) || face.slant >= attrbyte(:slant, :normal) # Not standing in for italics
            print(io, ANSI_STYLE_CODES.end_underline)
        end
    face.strikethrough == lastface.strikethrough || !haskey(Base.current_terminfo(), :smxx) ||
        print(io, ifelse(face.strikethrough == 0x1,
                         ANSI_STYLE_CODES.start_strikethrough,
                         ANSI_STYLE_CODES.end_strikethrough))
    face.inverse == lastface.inverse || !haskey(Base.current_terminfo(), :enter_reverse_mode) ||
        print(io, ifelse(face.inverse == 0x1,
                         ANSI_STYLE_CODES.start_reverse,
                         ANSI_STYLE_CODES.end_reverse))
end

@static if isdefined(Base, :unannotate)
    # This function uses SubString and AnnotatedString internals, but for current
    # and future versions, Base.unannotate is defined, and so this cannot break
    # in the future.
    const unannotate = Base.unannotate
else
    function unannotate(s::SubString{AnnotatedString{S}}) where S
        SubString{S}(s.string.string, s.offset, s.ncodeunits, Val(:noshift))
    end
end

"""
    safeuri(uri::String, uribytes::AbstractVector{UInt8}, allowedspecials::NTuple{N, UInt8}) where {N}

Percent-encode `uri` as necessary to ensure it is a valid URI.
"""
function safeuri(uri::String, uribytes::AbstractVector{UInt8}, allowedspecials::NTuple{N, UInt8}) where {N}
    isalphnum(c::UInt8) = (c ∈ UInt8('a'):UInt8('z')) || (c ∈ UInt8('A'):UInt8('Z')) || (c ∈ UInt8('0'):UInt8('9'))
    nib2hex(n::UInt8) = UInt8('0') + n + (((n + 0x6) >> 4) * 0x7)
    escbytes = 0
    for b in uribytes
        if !isalphnum(b) && b ∉ allowedspecials
            escbytes += 1
        end
    end
    if iszero(escbytes)
        uri
    else
        off, buf = 0, Base.StringMemory(length(uribytes) + 2 * escbytes)
        for (i, b) in enumerate(uribytes)
            b = uribytes[i]
            if isalphnum(b) || b ∈ allowedspecials
                buf[i+off] = b
            else
                buf[i+off] = UInt8('%')
                buf[i+off+1] = nib2hex(b >> 4)
                buf[i+off+2] = nib2hex(b & 0xf)
                off += 2
            end
        end
        Base.unsafe_takestring(buf)
    end
end

# The characters other than letters and digits that a URI may hold as they are
const URI_SPECIAL_CHARS = map(UInt8, Tuple("-_.!~*'():@&=+\$,%/?#[];"))

"""
    uriformat(link::String)

Ensure that `link` is a properly formatted URI.

If link does not start with an [RFC 2396](https://www.ietf.org/rfc/rfc2396.txt) compliant `protocol://`
prefix, it is treated as a file path and converted to a `file://` URI.

Otherwise, the link is percent-encoded as necessary to ensure it is a valid URI.
"""
function uriformat(link::String)
    isalphnum(c::UInt8) = (c ∈ UInt8('a'):UInt8('z')) || (c ∈ UInt8('A'):UInt8('Z')) || (c ∈ UInt8('0'):UInt8('9'))
    i, bytes = 1, codeunits(link)
    while i <= length(bytes)
        b = bytes[i]
        if isalphnum(b) || b ∈ map(UInt8, ('+', '-', '.'))
            i += 1
        elseif b == UInt8(':') && (i > 2 || get(bytes, i+1, 0x00) ∉ (UInt8('\\'), UInt8('/'))) # Skip Windows drive letters
            return safeuri(link, bytes, URI_SPECIAL_CHARS)
        else
            break
        end
    end
    Base.Filesystem.uripath(link)
end

function _ansi_writer(string_writer::F, io::IO, s::Union{<:AnnotatedString, SubString{<:AnnotatedString}}) where {F}
    # We need to make sure that the customisations are loaded
    # before we start outputting any styled content.
    load_customisations!()
    if get(io, :color, false)::Bool &&
        (!isempty(annotations(if s isa SubString s.string else s end)) || getface() !== STANDARD_FACES.default)
        # Make sure to (re)use a buffer to coalesce writes
        raw = first(Base.unwrapcontext(io))
        buf = if raw isa IOBuffer && !raw.append raw else IOBuffer() end # `position` is where an appending buffer reads
        start = position(buf)
        lastface = STANDARD_FACES.default.f
        lastlink::Union{String, Nothing} = nothing
        cache = FACES.cache[]
        for (str, styles) in eachregion(s)
            face = resolvedef(styles, cache)
            link = let idx = findfirst(a -> a.label === :link && a.value isa AbstractString, styles)
                if !isnothing(idx) String(styles[idx].value::AbstractString) end
            end
            if link != lastlink # One hyperlink for all of a link's regions
                isnothing(lastlink) || write(buf, "\e]8;;\e\\")
                isnothing(link) || write(buf, "\e]8;;", uriformat(link), "\e\\")
                lastlink = link
            end
            termstyle(buf, face, lastface)
            string_writer(buf, str)
            lastface = face
        end
        isnothing(lastlink) || write(buf, "\e]8;;\e\\")
        termstyle(buf, STANDARD_FACES.default.f, lastface)
        bytes = position(buf) - start
        buf === raw || write(io, seekstart(buf))
        bytes
    elseif s isa AnnotatedString
        string_writer(io, s.string)
    elseif s isa SubString
        string_writer(io, unannotate(s))
    end
end

# ------------
# Hook into the AnnotatedDisplay style dispatch

"""
    Styled

The [`AnnotatedDisplay.AnnotationStyle`](@ref) of `Face`: annotated strings whose
values include `Face`s are displayed by StyledStrings. Another annotation value type can
be displayed the same way by declaring `Styled()` as its style, and defining
`convert(Face, value)` for its values.
"""
struct Styled <: AnnotatedDisplay.AbstractAnnotationStyle end

AnnotatedDisplay.AnnotationStyle(::Type{Face}) = Styled()

# Another loaded copy of StyledStrings has a `Styled` of its own, and each copy renders the
# other's faces (see `foreignface`), so either may display them both
function AnnotatedDisplay.AnnotationStyle(a::Styled, b::AnnotatedDisplay.AbstractAnnotationStyle)
    B = typeof(b)
    if nameof(B) === :Styled && nameof(parentmodule(B)) === :StyledStrings a end
end

AnnotatedDisplay.awrite(textwriter::F, ::Styled, io::IO, s::Union{<:AnnotatedString, <:SubString{<:AnnotatedString}}) where {F} =
    _ansi_writer(textwriter, io, s)

AnnotatedDisplay.awrite(::Styled, io::IO, ::MIME"text/html", s::Union{<:AnnotatedString, <:SubString{<:AnnotatedString}}) =
    show_html(io, s)

# Also see `legacy.jl:126` for `styled_write`.

# End AnnotatedDisplay hooks
# ------------

const HTML_FGBG = (
    foreground = "#000000",
    background = "#ffffff"
)

function htmlcolor(io::IO, color::SimpleColor, background::Bool = false)
    default = resolvedef(STANDARD_FACES.default)
    if color.value === FGBG_FACES.background || color.value == default.background
        if background
            return print(io, "initial")
        elseif default.background === FGBG_FACES.background
            return print(io, HTML_FGBG.background)
        end
    elseif color.value === FGBG_FACES.foreground || color.value == default.foreground
        if !background
            return print(io, "initial")
        elseif default.foreground === FGBG_FACES.foreground
            return print(io, HTML_FGBG.foreground)
        end
    end
    rgb = if color.value isa RGBTuple color.value else get(FACES.basecolors, color.value, UNRESOLVED_COLOR_FALLBACK) end
    print(io, '#')
    bytes2hex(io, rgb)
end

# Indexed as `ATTRIBUTES.weights` and `ATTRIBUTES.underlines`
const HTML_WEIGHTS = (100, 200, 300, 300, 400, 500, 600, 700, 800, 900)
const HTML_UNDERLINE_STYLES = ("solid", "double", "wavy", "dotted", "dashed")

function cssattrs(io::IO, face::FaceDef, lastface::FaceDef=resolvedef(STANDARD_FACES.default))
    priorattr = Ref(false)
    function printattr(io, attr, valparts...)
        if priorattr[]
            print(io, "; ")
        else
            priorattr[] = true
        end
        print(io, attr, ": ", valparts...)
    end
    if faceproperty(face, :font) != faceproperty(lastface, :font)
        printattr(io, "font-family", '\'') # Escaped for CSS, then for HTML
        replace(io, faceproperty(face, :font), '\\' => "\\\\", '\'' => "\\'", '&' => "&amp;", '"' => "&quot;", '<' => "&lt;")
        print(io, '\'')
    end
    if face.height !== lastface.height
        height, lastheight = faceproperty(face, :height), faceproperty(lastface, :height)
        if height isa Integer
            points, tenths = divrem(height, 10)
            if iszero(tenths)
                printattr(io, "font-size", points, "pt")
            else
                printattr(io, "font-size", points, '.', tenths, "pt")
            end
        elseif height isa AbstractFloat # Relative to the enclosing span
            relheight = if lastheight isa AbstractFloat height / lastheight else height end
            printattr(io, "font-size", round(Int, 100 * relheight), "%")
        end
    end
    face.weight == lastface.weight ||
        printattr(io, "font-weight", get(HTML_WEIGHTS, face.weight + 1, 400))
    face.slant == lastface.slant ||
        printattr(io, "font-style", String(get(ATTRIBUTES.slants, face.slant + 1, :normal)))
    function cssfgbg(def::FaceDef)
        fg, bg = faceproperty(def, :foreground), faceproperty(def, :background)
        if def.inverse == 0x1 (bg, fg) else (fg, bg) end
    end
    foreground, background = cssfgbg(face)
    lastforeground, lastbackground = cssfgbg(lastface)
    if foreground != lastforeground
        printattr(io, "color")
        htmlcolor(io, foreground)
    end
    if background != lastbackground
        printattr(io, "background-color")
        htmlcolor(io, background, true)
    end
    if face.underline !== lastface.underline || face.underline_style != lastface.underline_style ||
        face.strikethrough != lastface.strikethrough
        color, style = face.underline, face.underline_style
        parts = String[]
        if style < NO_UNDERLINE
            if !isnothingflavour(color)
                csscolor = sprint(htmlcolor, SimpleColor(color))
                csscolor == "initial" || push!(parts, csscolor) # Invalid here; without it, the line takes the text's colour
            end
            style != attrbyte(:underline, :straight) && push!(parts, HTML_UNDERLINE_STYLES[style + 1])
            push!(parts, "underline")
        end
        face.strikethrough == 0x1 && push!(parts, "line-through")
        printattr(io, "text-decoration", if isempty(parts) "none" else join(parts, ' ') end)
    end
end

function htmlstyle(io::IO, face::FaceDef, lastface::FaceDef=resolvedef(STANDARD_FACES.default))
    print(io, "<span style=\"")
    cssattrs(io, face, lastface)
    print(io, "\">")
end

function show_html(io::IO, s::Union{<:AnnotatedString, SubString{<:AnnotatedString}})
    # We need to make sure that the customisations are loaded
    # before we start outputting any styled content.
    load_customisations!()
    htmlescape(str) = replace(str, '&' => "&amp;", '<' => "&lt;", '>' => "&gt;")
    raw = first(Base.unwrapcontext(io))
    buf = if raw isa IOBuffer raw else IOBuffer() end
    # As faces appear in CSS, where inverse swaps the colours
    appearance = (face -> face.font, face -> face.height, face -> face.weight, face -> face.slant,
                  face -> if face.inverse == 0x1 face.background else face.foreground end,
                  face -> if face.inverse == 0x1 face.foreground else face.background end,
                  face -> if face.underline_style < NO_UNDERLINE # A plain underline has the colour of its text
                      if face.underline !== FGBG_FACES.foreground face.underline
                      elseif face.inverse == 0x1 face.background else face.foreground end
                  end,
                  face -> face.underline_style, face -> face.strikethrough)
    default = resolvedef(STANDARD_FACES.default)
    spans = FaceDef[]
    link, linkdepth = nothing, 0
    cache = FACES.cache[]
    for (str, styles) in eachregion(s)
        face = resolvedef(styles, cache)
        newlink = let idx = findfirst(a -> a.label === :link && a.value isa AbstractString, styles)
            if !isnothing(idx) # A link without a scheme is relative to the page
                href = String(styles[idx].value::AbstractString)
                safeuri(href, codeunits(href), URI_SPECIAL_CHARS)
            end
        end
        # What a span sets cannot be unset within it
        keep = 0
        for span in spans
            parent = if keep == 0 default else spans[keep] end
            all(attr -> attr(span) === attr(parent) || attr(span) === attr(face), appearance) || break
            keep += 1
        end
        if !isnothing(link) && (newlink != link || keep < linkdepth)
            print(buf, "</span>" ^ (length(spans) - linkdepth), "</a>")
            resize!(spans, linkdepth)
            link = nothing
        end
        keep = min(keep, length(spans))
        print(buf, "</span>" ^ (length(spans) - keep))
        resize!(spans, keep)
        if isnothing(link) && !isnothing(newlink)
            print(buf, "<a href=\"", htmlescape(newlink), "\">")
            link, linkdepth = newlink, keep
        end
        base = if isempty(spans) default else last(spans) end
        if !all(attr -> attr(base) === attr(face), appearance)
            htmlstyle(buf, face, base)
            push!(spans, face)
        end
        print(buf, htmlescape(str))
    end
    if !isnothing(link)
        print(buf, "</span>" ^ (length(spans) - linkdepth), "</a>")
        resize!(spans, linkdepth)
    end
    print(buf, "</span>" ^ length(spans))
    buf === raw || write(io, take!(buf))
    nothing
end
