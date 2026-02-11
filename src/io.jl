# This file is a part of Julia. License is MIT: https://julialang.org/license

# This isn't so much type piracy as type privateering 😉

"""
A mapping between ANSI named colours and indices in the standard 256-color
table. The standard colors are 0-7, and high intensity colors 8-15.

The high intensity colors are prefixed by "bright_". The "bright_black" color is
given two aliases: "grey" and "gray".
"""
const ANSI_4BIT_COLORS = IdDict{Face, Int}(
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
    ansi_4bit(color::Integer, background::Bool=false)

Provide the color code (30-37, 40-47, 90-97, 100-107) for `color` (0–15).

When `background` is set the background variant will be provided, otherwise
the provided code is for setting the foreground color.
"""
function ansi_4bit(code::Integer, background::Bool=false)
    code >= 8 && (code += 52)
    background && (code += 10)
    code + 30
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
    print(io, "\e[", category, "8;5;", string(colorcode), 'm')
end

"""
    termcolor24bit(io::IO, color::RGBTuple, category::Char)

Print to `io` the 24-bit SGR color code to set the `category`8 slot to `color`.
"""
function termcolor24bit(io::IO, color::RGBTuple, category::Char)
    print(io, "\e[", category, "8;2;",
          string(color.r), ';',
          string(color.g), ';',
          string(color.b), 'm')
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
    if category == '4' # Background
        if color.value ∈ (FGBG_FACES.background, FACES.basecolors[FGBG_FACES.background])
            return print(io, "\e[49m")
        elseif color.value == FGBG_FACES.foreground
            return print(io, "\e[47m") # Technically not quite[1], but close enough
        end
    elseif color.value ∈ (FGBG_FACES.foreground, FACES.basecolors[FGBG_FACES.foreground])
        return print(io, "\e[", category, "9m")
    elseif category == '3' && color.value == FGBG_FACES.background
        return print(io, "\e[30m") # Technically not quite[1], but close enough
    end
    # [1]: There is no true way to selectively set the fg/bg in the terminal to the
    # bg/fg colour, but with the way most terminals/terminal themes treat white/black
    # we can often get a close result with them.
    cfinal = finalcolor(color)
    if cfinal isa Face
        ansi = get(ANSI_4BIT_COLORS, cfinal, nothing)
        isnothing(ansi) && return # Unknown color
        print(io, "\e[")
        if category == '3' || category == '4'
            print(io, ansi_4bit(ansi, category == '4'))
        elseif category == '5'
            print(io, "58;5;", ansi)
        end
        print(io, 'm')
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
termcolor(io::IO, ::Nothing, category::Char) =
    print(io, "\e[", category, '9', 'm')

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
    face.foreground == lastface.foreground ||
        termcolor(io, face.foreground, '3')
    face.background == lastface.background ||
        termcolor(io, face.background, '4')
    face.weight == lastface.weight || begin
        if lastface.weight != :normal && face.weight != :normal
            print(io, ANSI_STYLE_CODES.normal_weight) # Reset before changing
        end
        print(io, if face.weight ∈ (:medium, :semibold, :bold, :extrabold, :black)
                  ANSI_STYLE_CODES.bold_weight
              elseif face.weight ∈ (:semilight, :light, :extralight, :thin)
                  get(Base.current_terminfo(), :dim, "")
              else # :normal
                  ANSI_STYLE_CODES.normal_weight
              end)
    end
    face.slant == lastface.slant ||
        if haskey(Base.current_terminfo(), :enter_italics_mode)
            print(io, ifelse(face.slant ∈ (:italic, :oblique),
                             ANSI_STYLE_CODES.start_italics,
                             ANSI_STYLE_CODES.end_italics))
        elseif face.slant ∈ (:italic, :oblique) && isnothing(face.underline)
            print(io, ANSI_STYLE_CODES.start_underline)
        elseif face.slant ∉ (:italic, :oblique) && isnothing(lastface.underline)
            print(io, ANSI_STYLE_CODES.end_underline)
        end
    # Kitty fancy underlines, see <https://sw.kovidgoyal.net/kitty/underlines>
    # Supported in Kitty, VTE, iTerm2, Alacritty, and Wezterm.
    (face.f.underline == lastface.f.underline && face.f.underline_style == lastface.f.underline_style) ||
        if haskey(Base.current_terminfo(), :set_underline_style) || get(Base.current_terminfo(), :can_style_underline, false)
            ul, ulstyle = face.f.underline, face.f.underline_style
            lastul, lastulstyle = lastface.f.underline, lastface.f.underline_style
            if ulstyle != lastulstyle && !isnothingflavour(ulstyle)
                if isnothingflavour(lastulstyle) && ulstyle == :straight
                    print(io, ANSI_STYLE_CODES.start_underline)
                else
                    print(io, "\e[4:",
                          if ulstyle == :straight;   '1'
                          elseif ulstyle == :double; '2'
                          elseif ulstyle == :curly;  '3'
                          elseif ulstyle == :dotted; '4'
                          elseif ulstyle == :dashed; '5'
                          else '0' end, 'm')
                end
            end
            if !isnothingflavour(ul)
                termcolor(io, ul, '5')
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
    face.strikethrough == lastface.strikethrough || !haskey(Base.current_terminfo(), :smxx) ||
        print(io, ifelse(face.strikethrough === true,
                         ANSI_STYLE_CODES.start_strikethrough,
                         ANSI_STYLE_CODES.end_strikethrough))
    face.inverse == lastface.inverse || !haskey(Base.current_terminfo(), :enter_reverse_mode) ||
        print(io, ifelse(face.inverse === true,
                         ANSI_STYLE_CODES.start_reverse,
                         ANSI_STYLE_CODES.end_reverse))
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
        elseif b == UInt8(':') && (i > 2 || get(bytes, i+1, 0x00) ∉ (UInt8('\\', '/'))) # Skip Windows drive letters
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
    if get(io, :color, false)::Bool
        buf = IOBuffer() # Avoid the overhead in repeatedly printing to `stdout`
        lastface::Face = STANDARD_FACES.default
        for (str, styles) in eachregion(s)
            face = getface(styles)
            link = let idx = findfirst(==(:link) ∘ first, styles)
                if !isnothing(idx)
                    uriformat(string(styles[idx].value)::String)
                end
            end
            !isnothing(link) && write(buf, "\e]8;;", link, "\e\\")
            termstyle(buf, face, lastface)
            string_writer(buf, str)
            !isnothing(link) && write(buf, "\e]8;;\e\\")
            lastface = face
        end
        termstyle(buf, STANDARD_FACES.default, lastface)
        write(io, seekstart(buf))
    elseif s isa AnnotatedString
        string_writer(io, s.string)
    elseif s isa SubString
        string_writer(
            io, SubString(s.string.string, s.offset, s.ncodeunits, Val(:noshift)))
    end
end

# ------------
# Hook into the AnnotatedDisplay invalidation barrier

Base.AnnotatedDisplay.ansi_write(f::F, io::IO, s::Union{<:AnnotatedString{<:Any, >:Face}, <:SubString{<:AnnotatedString{<:Any, >:Face}}}) where {F <: Function} =
    _ansi_writer(f, io, s)

function Base.AnnotatedDisplay.ansi_write(::typeof(write), io::IO, c::AnnotatedChar{<:Any, >:Face})
    if get(io, :color, false) == true
        termstyle(io, getface(c), getface())
        bytes = write(io, c.char)
        termstyle(io, getface(), getface(c))
        bytes
    else
        write(io, c.char)
    end
end

function Base.AnnotatedDisplay.show_annot(io::IO, c::AnnotatedChar{<:Any, >:Face})
    if get(io, :color, false) == true
        out = IOBuffer()
        show(out, c.char)
        cstr = AnnotatedString(
            String(take!(out)[2:end-1]),
            [(1:ncodeunits(c), a...) for a in c.annotations])
        print(io, ''', cstr, ''')
    else
        show(io, c.char)
    end
end

Base.AnnotatedDisplay.show_annot(io::IO, ::MIME"text/html", s::Union{<:AnnotatedString{<:Any, >:Face}, <:SubString{<:AnnotatedString{<:Any, >:Face}}}) =
    show_html(io, s)

# Also see `legacy.jl:126` for `styled_write`.

# End AnnotatedDisplay hooks
# ------------

const HTML_FGBG = (
    foreground = "#000000",
    background = "#ffffff"
)

function htmlcolor(io::IO, color::SimpleColor, background::Bool = false)
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
    r < 0x10 && print(io, '0')
    print(io, string(r, base=16))
    g < 0x10 && print(io, '0')
    print(io, string(g, base=16))
    b < 0x10 && print(io, '0')
    print(io, string(b, base=16))
end

const HTML_WEIGHT_MAP = Dict{Symbol, Int}(
    :thin => 100,
    :extralight => 200,
    :light => 300,
    :semilight => 300,
    :normal => 400,
    :medium => 500,
    :semibold => 600,
    :bold => 700,
    :extrabold => 800,
    :black => 900)

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
    face.weight == lastface.weight ||
        printattr(io, "font-weight", get(HTML_WEIGHT_MAP, face.weight, 400))
    face.slant == lastface.slant ||
        printattr(io, "font-style", String(face.slant))
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
    if (face.f.underline != lastface.f.underline || face.f.underline_style != lastface.f.underline_style) &&
        !(isnothingflavour(face.f.underline_style) && isnothingflavour(lastface.f.underline_style))
        color, style = face.f.underline, face.f.underline_style
        printattr(io, "text-decoration")
        if isnothingflavour(style)
            print(io, "none")
        elseif !isnothingflavour(color)
            htmlcolor(io, color)
            print(io, ' ')
        elseif !isnothingflavour(lastface.f.underline)
            print(io, "currentcolor ")
        end
        if isnothingflavour(style)
        elseif style == :straight && (isnothingflavour(lastface.f.underline_style) || lastface.f.underline_style == :straight)
            print(io, "underline")
        else
            print(io, if style == :straight "solid "
                  elseif style == :double   "double "
                  elseif style == :curly    "wavy "
                  elseif style == :dotted   "dotted "
                  elseif style == :dashed   "dashed "
                  else "" end, "underline")
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
    buf = IOBuffer() # Avoid potential overhead in repeatadly printing a more complex IO
    lastface::Face = getface()
    stylestackdepth = 0
    for (str, styles) in eachregion(s)
        face = getface(styles)
        link = let idx=findfirst(==(:link) ∘ first, styles)
            if !isnothing(idx)
                uriformat(string(styles[idx].value)::String)
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
    write(io, take!(buf))
    nothing
end
