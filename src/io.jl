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

function writedigits(io::IO, num::UInt8, suffix::Char)
    digits, ndigits = packdigits(num)
    writebytes(io, digits | UInt64(suffix) << (8 * ndigits), ndigits + 1)
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
termcolor(io::IO, color::SimpleColor, category::Char) = termcolor(io, color.value, category)
termcolor(io::IO, ::Union{WeakNothing, StrongNothing}, category::Char) = termcolor(io, nothing, category)

function termcolor(io::IO, value::Union{Face, RGBTuple}, category::Char)
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

function AnnotatedDisplay.transition(io::IO, ::MIME"text/plain", lastface::FaceDef, face::FaceDef)
    face.foreground === lastface.foreground || termcolor(io, face.foreground, '3')
    face.background === lastface.background || termcolor(io, face.background, '4')
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
            ul === lastul || termcolor(io, ul, '5')
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

"""
    Link(uri::String)

A hyperlink to `uri`, as the value of a `:link` annotation. Styled output makes the
annotated text a link: an OSC 8 hyperlink in a terminal, or an `<a>` element in HTML.

A string can be converted to a `Link`, as `{link=…}` in styled markup does.

# Examples

```julia
julia> s = styled"see {link={https://julialang.org}:the website}";

julia> annotations(s)[1].value
StyledStrings.Link("https://julialang.org")
```
"""
struct Link
    uri::String
end

Base.convert(::Type{Link}, uri::AbstractString) = Link(uri)

"""
    StyledValue

The type of the annotation values of styled strings: a [`Face`](@ref) or a [`Link`](@ref).
Styled strings are `AnnotatedString{String, StyledValue}`. Markup with other `key=value` pairs
widens it to hold their values too.

# Examples

```jldoctest; setup = :(using StyledStrings: StyledValue)
julia> styled"{red:x} {link={https://julialang.org}:y}" isa AnnotatedString{String, StyledValue}
true
```
"""
const StyledValue = Union{Face, Link}


const HTML_ESCAPES = ('&' => "&amp;", '<' => "&lt;", '>' => "&gt;")
const CSS_FONT_ESCAPES = ('\\' => "\\\\", '\'' => "\\'", '&' => "&amp;", '"' => "&quot;", '<' => "&lt;") # For CSS, then HTML

function writeescaped(io::IO, str::AbstractString, escapes::Tuple{Vararg{Pair{Char, String}}})
    for c in str
        i = findfirst(e -> first(e) == c, escapes)
        if isnothing(i) print(io, c) else print(io, last(escapes[i])) end
    end
end

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

# ------------
# How faces and links display, through `AnnotatedDisplay`

AnnotatedDisplay.context(::MIME, ::Type{Face}) = (load_customisations!(); FACES.cache[])

AnnotatedDisplay.reduction(::MIME"text/plain", ::Type{Face}, cache, annotations) = resolvedef(annotations, cache)

# The face as CSS shows it: inverse in its colours, and a line's colour only with a line,
# a plain line taking the text's
function AnnotatedDisplay.reduction(::MIME"text/html", ::Type{Face}, cache, annotations)
    face = resolvedef(annotations, cache)
    fg, bg = if face.inverse == 0x1
        face.background, face.foreground
    else
        face.foreground, face.background
    end
    ul = if face.underline_style >= NO_UNDERLINE
        WeakNothing()
    elseif face.underline === FGBG_FACES.foreground
        fg
    else
        face.underline
    end
    FaceDef(face.font, fg, bg, ul, face.height, face.weight, face.slant,
            face.underline_style, face.strikethrough, 0x0, face.inherit)
end

function AnnotatedDisplay.transition(io::IO, ::MIME"text/html", from::FaceDef, to::FaceDef)
    print(io, "<span style=\"")
    if to.font !== from.font
        print(io, "font-family: '")
        writeescaped(io, to.font, CSS_FONT_ESCAPES)
        print(io, "';")
    end
    if to.height !== from.height
        height, outer = faceproperty(to, :height), faceproperty(from, :height)
        if height isa Integer # Tenths of a point
            points, tenths = divrem(height, 10)
            print(io, "font-size: ", points)
            iszero(tenths) || print(io, '.', tenths)
            print(io, "pt;")
        elseif height isa AbstractFloat # Relative to the enclosing span
            relative = if outer isa AbstractFloat height / outer else height end
            print(io, "font-size: ", round(Int, 100 * relative), "%;")
        end
    end
    to.weight == from.weight || print(io, "font-weight: ", get(HTML_WEIGHTS, to.weight + 1, 400), ';')
    to.slant == from.slant || print(io, "font-style: ", get(ATTRIBUTES.slants, to.slant + 1, :normal), ';')
    if to.foreground !== from.foreground
        print(io, "color: ")
        htmlcolor(io, SimpleColor(to.foreground))
        print(io, ';')
    end
    if to.background !== from.background
        print(io, "background-color: ")
        htmlcolor(io, SimpleColor(to.background), true)
        print(io, ';')
    end
    if to.underline !== from.underline || to.underline_style != from.underline_style ||
        to.strikethrough != from.strikethrough
        underlined, struck = to.underline_style < NO_UNDERLINE, to.strikethrough == 0x1
        print(io, "text-decoration:")
        if underlined
            # A line in the text's colour needs no colour of its own
            if !isnothingflavour(to.underline) && to.underline !== to.foreground
                color = sprint(htmlcolor, SimpleColor(to.underline))
                color == "initial" || print(io, ' ', color)
            end
            to.underline_style == attrbyte(:underline, :straight) ||
                print(io, ' ', HTML_UNDERLINE_STYLES[to.underline_style + 1])
            print(io, " underline")
        end
        struck && print(io, " line-through")
        underlined || struck || print(io, " none")
        print(io, ';')
    end
    print(io, "\">")
end
AnnotatedDisplay.transition(io::IO, ::MIME"text/html", ::FaceDef, ::Nothing) = print(io, "</span>")

# Links, where `Link("")` is no link

function AnnotatedDisplay.reduction(::MIME, ::Type{Link}, _, annotations)
    link = Link("")
    for ann in annotations
        ann.value isa Link && (link = ann.value)
    end
    link
end

function AnnotatedDisplay.transition(io::IO, ::MIME"text/plain", from::Link, to::Link)
    from.uri == to.uri && return
    isempty(from.uri) || write(io, "\e]8;;\e\\")
    isempty(to.uri) || write(io, "\e]8;;", uriformat(to.uri), "\e\\")
end

function AnnotatedDisplay.transition(io::IO, ::MIME"text/html", ::Link, to::Link)
    print(io, "<a href=\"")
    writeescaped(io, safeuri(to.uri, codeunits(to.uri), URI_SPECIAL_CHARS), HTML_ESCAPES)
    print(io, "\">")
end
AnnotatedDisplay.transition(io::IO, ::MIME"text/html", ::Link, ::Nothing) = print(io, "</a>")
