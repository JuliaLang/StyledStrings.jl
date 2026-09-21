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

"""
    writebytes(io::IO, word::UInt64, nb::Integer)

Write the low `nb` bytes of `word`, as they lie in memory, to `io` in one call.
"""
function writebytes(io::IO, word::UInt64, nb::Integer)
    bytes = Ref(htol(word))
    GC.@preserve bytes unsafe_write(io, Ptr{UInt8}(pointer_from_objref(bytes)), nb)
end

"""
    packdigits(num::UInt8) -> (digits::UInt64, ndigits)

The decimal digits of `num` packed little-endian, first digit lowest, and their count.
"""
function packdigits(num::UInt8)
    hundreds, rest = divrem(num, UInt8(100))
    tens, ones = divrem(rest, UInt8(10))
    ndigits = 0x1 + (num >= UInt8(10)) + (num >= UInt8(100))
    zero = UInt64(UInt8('0'))
    ((zero + hundreds) | (zero + tens) << 8 | (zero + ones) << 16) >> (8 * (0x3 - ndigits)), ndigits
end

"""
    writedigits(io::IO, num::UInt8, suffix::Char = '\\0')

Efficiently write an 8-bit unsigned number (`num`) to `io` as a decimal, followed by
`suffix` when one is given.
"""
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
        (2 + r/256) * (r - r2)^2 + 4 * (g - g2)^2 + (2 + (255 - rr)/256) * (b - b2)^2
    end
    to6cube(value) = max(0, value - 35) ÷ 40
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
        grey = sum((r, g, b)) ÷ 3
        grey_level = min(23, (grey - 3) ÷ 10)
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

Print to `io` the 24-bit SGR color code to set the `category`8 slot to `color`.
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

If `color` is a `SimpleColor{Symbol}`, the value should be a a member of
`ANSI_4BIT_COLORS`. Any other value will cause the color to be reset.

If `color` is a `SimpleColor{RGBTuple}` and `get_have_truecolor()` returns true,
24-bit color is used. Otherwise, an 8-bit approximation of `color` is used.

If `color` is unknown, no output is produced.
"""
function termcolor(io::IO, color::SimpleColor, category::Char)
    value = color.value
    if category == '4' # Background
        if value === FGBG_FACES.background || value isa RGBTuple && value == FACES.basecolors[FGBG_FACES.background]
            return termcolor(io, nothing, '4')
        elseif value === FGBG_FACES.foreground
            return print(io, "\e[47m") # Technically not quite[1], but close enough
        end
    elseif value === FGBG_FACES.foreground || value isa RGBTuple && value == FACES.basecolors[FGBG_FACES.foreground]
        return termcolor(io, nothing, category)
    elseif category == '3' && value === FGBG_FACES.background
        return print(io, "\e[30m") # Technically not quite[1], but close enough
    end
    # [1]: There is no true way to selectively set the fg/bg in the terminal to the
    # bg/fg colour, but with the way most terminals/terminal themes treat white/black
    # we can often get a close result with them.
    cfinal = finalcolor(color)
    if cfinal isa Face
        ansi = get(ANSI_4BIT_COLORS, cfinal, nothing)
        isnothing(ansi) && return # Unknown color
        if category == '5'
            write(io, "\e[58;5;")
            writedigits(io, ansi, 'm')
        else # The whole sequence fits one word
            digits, ndigits = packdigits(ansi_4bit(ansi, category == '4'))
            writebytes(io, UInt64(0x5b1b) | digits << 16 | UInt64(UInt8('m')) << (16 + 8 * ndigits), ndigits + 3)
        end
    elseif cfinal isa RGBTuple
        if Base.get_have_truecolor()
            termcolor24bit(io, cfinal, category)
        else
            termcolor8bit(io, cfinal, category)
        end
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

function termstyle(io::IO, face::Face, lastface::Face=getface())
    face.f.foreground === lastface.f.foreground ||
        termcolor(io, face.foreground, '3')
    face.f.background === lastface.f.background ||
        termcolor(io, face.background, '4')
    face.f.weight == lastface.f.weight || begin
        if lastface.f.weight != NORMAL_WEIGHT && face.f.weight != NORMAL_WEIGHT
            print(io, ANSI_STYLE_CODES.normal_weight) # Reset before changing
        end
        weight = face.f.weight
        print(io, if weight < NORMAL_WEIGHT
                  get(Base.current_terminfo(), :dim, "")
              elseif weight == NORMAL_WEIGHT || isnothingflavour(weight)
                  ANSI_STYLE_CODES.normal_weight
              else
                  ANSI_STYLE_CODES.bold_weight
              end)
    end
    face.f.slant == lastface.f.slant ||
        let slanted = face.f.slant < NORMAL_SLANT # italic or oblique
            if haskey(Base.current_terminfo(), :enter_italics_mode)
                print(io, ifelse(slanted, ANSI_STYLE_CODES.start_italics, ANSI_STYLE_CODES.end_italics))
            elseif slanted && isnothing(face.underline)
                print(io, ANSI_STYLE_CODES.start_underline)
            elseif !slanted && isnothing(lastface.underline)
                print(io, ANSI_STYLE_CODES.end_underline)
            end
        end
    # Kitty fancy underlines, see <https://sw.kovidgoyal.net/kitty/underlines>
    # Supported in Kitty, VTE, iTerm2, Alacritty, and Wezterm.
    (face.f.underline === lastface.f.underline && face.f.underline_style == lastface.f.underline_style) ||
        if haskey(Base.current_terminfo(), :set_underline_style) || get(Base.current_terminfo(), :can_style_underline, false)
            ul, ulstyle = face.f.underline, face.f.underline_style
            lastul, lastulstyle = lastface.f.underline, lastface.f.underline_style
            if ulstyle != lastulstyle && !isnothingflavour(ulstyle)
                if isnothingflavour(lastulstyle) && ulstyle == STRAIGHT_UNDERLINE
                    print(io, ANSI_STYLE_CODES.start_underline)
                else # Kitty numbers the styles from 1 in `UNDERLINE_STYLE_NAMES` order
                    print(io, "\e[4:", Char(UInt8('1') + ulstyle), 'm')
                end
            end
            if !isnothingflavour(ul)
                termcolor(io, SimpleColor(ul), '5')
            elseif !isnothingflavour(lastul)
                termcolor(io, SimpleColor(FGBG_FACES.foreground), '5')
            end
            if isnothingflavour(ulstyle) && !isnothingflavour(lastulstyle)
                print(io, ANSI_STYLE_CODES.end_underline)
            end
        elseif isnothing(face.underline)
            print(io, ANSI_STYLE_CODES.end_underline)
        else
            print(io, ANSI_STYLE_CODES.start_underline)
        end
    face.f.strikethrough == lastface.f.strikethrough || !haskey(Base.current_terminfo(), :smxx) ||
        print(io, ifelse(face.f.strikethrough == 0x1,
                         ANSI_STYLE_CODES.start_strikethrough,
                         ANSI_STYLE_CODES.end_strikethrough))
    face.f.inverse == lastface.f.inverse || !haskey(Base.current_terminfo(), :enter_reverse_mode) ||
        print(io, ifelse(face.f.inverse == 0x1,
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
            return safeuri(link, bytes, ((UInt8(c) for c in "-_.!~*'():@&=+\$,%/?#[]@")...,))
        else
            break
        end
    end
    Base.Filesystem.uripath(link)
end

function _ansi_writer(string_writer::F, io::IO, s::Union{<:AnnotatedString, SubString{<:AnnotatedString}}) where {F <: Function}
    # We need to make sure that the customisations are loaded
    # before we start outputting any styled content.
    load_customisations!()
    if get(io, :color, false)::Bool && !isempty(annotations(if s isa SubString s.string else s end))
        # Make sure to (re)use a buffer to coalesce writes
        raw = first(Base.unwrapcontext(io))
        buf = if raw isa IOBuffer raw else IOBuffer() end
        start = position(buf)
        lastface::Face = STANDARD_FACES.default
        lastlink::Union{String, Nothing} = nothing
        for (str, styles) in eachregion(s)
            face = getface(styles)
            link = let idx = findfirst(==(:link) ∘ first, styles)
                if !isnothing(idx) String(styles[idx].value) end
            end
            if link != lastlink # A link spans its regions as one hyperlink
                isnothing(lastlink) || write(buf, "\e]8;;\e\\")
                isnothing(link) || write(buf, "\e]8;;", uriformat(link), "\e\\")
                lastlink = link
            end
            termstyle(buf, face, lastface)
            string_writer(buf, str)
            lastface = face
        end
        isnothing(lastlink) || write(buf, "\e]8;;\e\\")
        termstyle(buf, STANDARD_FACES.default, lastface)
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
be displayed the same way by declaring `Styled()` as its style, provided
[`getface`](@ref) can interpret its values.
"""
struct Styled <: AnnotatedDisplay.AbstractAnnotationStyle end

AnnotatedDisplay.AnnotationStyle(::Type{Face}) = Styled()

AnnotatedDisplay.awrite(textwriter::F, ::Styled, io::IO, s::Union{<:AnnotatedString, <:SubString{<:AnnotatedString}}) where {F} =
    _ansi_writer(textwriter, io, s)

function AnnotatedDisplay.awrite(textwriter::F, ::Styled, io::IO, c::AnnotatedChar) where {F}
    if get(io, :color, false) == true
        termstyle(io, getface(c), getface())
        bytes = textwriter(io, c.char)
        termstyle(io, getface(), getface(c))
        bytes
    else
        textwriter(io, c.char)
    end
end

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
    function writehex(byte::UInt8)
        digits = b"0123456789abcdef"
        write(io, @inbounds digits[byte >> 4 + 1])
        write(io, @inbounds digits[byte & 0xf + 1])
    end
    default = getface()
    if color.value ∈ (FGBG_FACES.background, default.background)
        if background
            return print(io, "initial")
        elseif default.background.value == FGBG_FACES.background
            return print(io, HTML_FGBG.background)
        end
    elseif color.value ∈ (FGBG_FACES.foreground, default.foreground)
        if !background
            return print(io, "initial")
        elseif default.foreground.value == FGBG_FACES.foreground
            return print(io, HTML_FGBG.foreground)
        end
    end
    (; r, g, b) = rgbcolor(color)
    print(io, '#')
    writehex(r); writehex(g); writehex(b)
end

# Indexed as `WEIGHT_NAMES` and `UNDERLINE_STYLE_NAMES`
const HTML_WEIGHTS = (100, 200, 300, 300, 400, 500, 600, 700, 800, 900)
const HTML_UNDERLINE_STYLES = ("solid", "double", "wavy", "dotted", "dashed")

function cssattrs(io::IO, face::Face, lastface::Face=getface())
    priorattr = Ref(false)
    function printattr(io, attr, valparts...)
        if priorattr[]
            print(io, "; ")
        else
            priorattr[] = true
        end
        print(io, attr, ": ", valparts...)
    end
    if face.font != lastface.font
        printattr(io, "font-family")
        print(io, "\"")
        replace(io, face.font, '"' => "&quot;", '&' => "&amp;", '<' => "&lt;", '\\' => "\\\\")
        print(io, "\"")
    end
    face.height == lastface.height ||
        printattr(io, "font-size", string(face.height ÷ 10), "pt")
    face.f.weight == lastface.f.weight ||
        printattr(io, "font-weight", get(HTML_WEIGHTS, face.f.weight + 1, 400))
    face.f.slant == lastface.f.slant ||
        printattr(io, "font-style", String(get(SLANT_NAMES, face.f.slant + 1, :normal)))
    foreground, background =
        ifelse(face.inverse === true,
               (face.background, face.foreground),
               (face.foreground, face.background))
    lastforeground, lastbackground =
        ifelse(lastface.inverse === true,
               (lastface.background, lastface.foreground),
               (lastface.foreground, lastface.background))
    if foreground != lastforeground
        printattr(io, "color")
        htmlcolor(io, foreground)
    end
    if background != lastbackground
        printattr(io, "background-color")
        htmlcolor(io, background, true)
    end
    if (face.f.underline !== lastface.f.underline || face.f.underline_style != lastface.f.underline_style) &&
        !(isnothingflavour(face.f.underline_style) && isnothingflavour(lastface.f.underline_style))
        color, style = face.f.underline, face.f.underline_style
        printattr(io, "text-decoration")
        if isnothingflavour(style)
            print(io, "none")
        elseif !isnothingflavour(color)
            htmlcolor(io, SimpleColor(color))
            print(io, ' ')
        elseif !isnothingflavour(lastface.f.underline)
            print(io, "currentcolor ")
        end
        if isnothingflavour(style)
        elseif style == STRAIGHT_UNDERLINE && (isnothingflavour(lastface.f.underline_style) || lastface.f.underline_style == STRAIGHT_UNDERLINE)
            print(io, "underline")
        else
            print(io, HTML_UNDERLINE_STYLES[style + 1], " underline")
        end
    end
    face.strikethrough == lastface.strikethrough ||
        !face.strikethrough && face.underline !== false ||
        printattr(io, "text-decoration", ifelse(face.strikethrough, "line-through", "none"))
end

function htmlstyle(io::IO, face::Face, lastface::Face=getface())
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
    lastface::Face = getface()
    stylestackdepth = 0
    for (str, styles) in eachregion(s)
        face = getface(styles)
        link = let idx=findfirst(==(:link) ∘ first, styles)
            if !isnothing(idx)
                uriformat(String(styles[idx].value))
            end
        end
        !isnothing(link) && print(buf, "<a href=\"", link, "\">")
        if face == getface()
            print(buf, "</span>" ^ stylestackdepth)
            stylestackdepth = 0
        elseif (lastface.inverse, lastface.foreground, lastface.background) !=
            (face.inverse, face.foreground, face.background)
            # We can't un-inherit colors well, so we just need to reset and apply
            print(buf, "</span>" ^ stylestackdepth)
            htmlstyle(buf, face, getface())
            stylestackdepth = 1
        else
            htmlstyle(buf, face, lastface)
            stylestackdepth += 1
        end
        print(buf, htmlescape(str))
        !isnothing(link) && print(buf, "</a>")
        lastface = face
    end
    print(buf, "</span>" ^ stylestackdepth)
    buf === raw || write(io, take!(buf))
    nothing
end
