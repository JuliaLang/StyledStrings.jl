# This file is a part of Julia. License is MIT: https://julialang.org/license

# This isn't so much type piracy as type privateering 😉

"""
A mapping between ANSI named colours and indices in the standard 256-color
table. The standard colors are 0-7, and high intensity colors 8-15.

The high intensity colors are prefixed by "bright_". The "bright_black" color is
given two aliases: "grey" and "gray".
"""
const ANSI_4BIT_COLORS = Dict{Symbol, Int}(
    :black => 0,
    :red => 1,
    :green => 2,
    :yellow => 3,
    :blue => 4,
    :magenta => 5,
    :cyan => 6,
    :white => 7,
    :bright_black => 8,
    :grey => 8,
    :gray => 8,
    :bright_red => 9,
    :bright_green => 10,
    :bright_yellow => 11,
    :bright_blue => 12,
    :bright_magenta => 13,
    :bright_cyan => 14,
    :bright_white => 15)

"""
    ansi_4bit_color_code(color::Symbol, background::Bool=false)

Provide the color code (30-37, 40-47, 90-97, 100-107) for `color`, as a string.
When `background` is set the background variant will be provided, otherwise
the provided code is for setting the foreground color.
"""
function ansi_4bit_color_code(color::Symbol, background::Bool=false)
    if haskey(ANSI_4BIT_COLORS, color)
        code = ANSI_4BIT_COLORS[color]
        code >= 8 && (code += 52)
        background && (code += 10)
        string(code + 30)
    else
        ifelse(background, "49", "39")
    end
end

"""
    termcolor8bit(io::IO, color::RGBTuple, category::Char)

Print to `io` the best 8-bit SGR color code that sets the `category` color to
be close to `color`.
"""
function termcolor8bit(io::IO, (; r, g, b)::RGBTuple, category::Char)
    # Magic numbers? Lots.
    cdistsq(r1, g1, b1) = (r1 - r)^2 + (g1 - g)^2 + (b1 - b)^2
    to6cube(value) = if value < 48; 1
    elseif value < 114; 2
    else 1 + (value - 35) ÷ 40 end
    r6cube, g6cube, b6cube = to6cube(r), to6cube(g), to6cube(b)
    sixcube = (0, 95, 135, 175, 215, 255)
    rnear, gnear, bnear = sixcube[r6cube], sixcube[g6cube], sixcube[b6cube]
    colorcode = if r == rnear && g == gnear && b == bnear
        16 + 35 * r6cube + 6 * g6cube + b6cube
    else
        grey_avg = Int(r + g + b) ÷ 3
        grey_index = if grey_avg > 238 23 else (grey_avg - 3) ÷ 10 end
        grey = 8 + 10 * grey_index
        if cdistsq(grey, grey, grey) <= cdistsq(rnear, gnear, bnear)
            232 + grey
        else
            16 + 35 * r6cube + 6 * g6cube + b6cube
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
"""
function termcolor(io::IO, color::SimpleColor, category::Char)
    if color.value isa RGBTuple
        if Base.get_have_truecolor()
            termcolor24bit(io, color.value, category)
        else
            termcolor8bit(io, color.value, category)
        end
    elseif (fg = get(FACES.current[], color.value, getface()).foreground) != SimpleColor(color.value)
        termcolor(io, fg, category)
    else
        print(io, "\e[",
              if category == '3' || category == '4'
                  ansi_4bit_color_code(color.value, category == '4')
              elseif category == '5'
                  if haskey(ANSI_4BIT_COLORS, color.value)
                      string("58;5;", ANSI_4BIT_COLORS[color.value])
                  else "59" end
              end,
              'm')
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
    dim_weight = "\e[2m",
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
    face.weight == lastface.weight ||
        print(io, if face.weight ∈ (:medium, :semibold, :bold, :extrabold, :black)
                  get(Base.current_terminfo, :bold, "\e[1m")
              elseif face.weight ∈ (:semilight, :light, :extralight, :thin)
                  get(Base.current_terminfo, :dim, "")
              else # :normal
                  ANSI_STYLE_CODES.normal_weight
              end)
    face.slant == lastface.slant ||
        if haskey(Base.current_terminfo, :enter_italics_mode)
            print(io, ifelse(face.slant ∈ (:italic, :oblique),
                             ANSI_STYLE_CODES.start_italics,
                             ANSI_STYLE_CODES.end_italics))
        elseif face.slant ∈ (:italic, :oblique) && face.underline ∈ (nothing, false)
            print(io, ANSI_STYLE_CODES.start_underline)
        elseif face.slant ∉ (:italic, :oblique) && lastface.underline ∈ (nothing, false)
            print(io, ANSI_STYLE_CODES.end_underline)
        end
    # Kitty fancy underlines, see <https://sw.kovidgoyal.net/kitty/underlines>
    # Supported in Kitty, VTE, iTerm2, Alacritty, and Wezterm.
    face.underline == lastface.underline ||
        if get(Base.current_terminfo, :Su, false) # Color/style capabilities
            if face.underline isa Tuple # Color and style
                color, style = face.underline
                print(io, "\e[4:",
                        if style == :straight;   '1'
                        elseif style == :double; '2'
                        elseif style == :curly;  '3'
                        elseif style == :dotted; '4'
                        elseif style == :dashed; '5'
                        else '0' end, 'm')
                !isnothing(color) && termcolor(io, color, '5')
            elseif face.underline isa SimpleColor
                if !(lastface.underline isa SimpleColor || lastface.underline == true)
                    print(io, ANSI_STYLE_CODES.start_underline)
                end
                termcolor(io, face.underline, '5')
            else
                if lastface.underline isa SimpleColor || lastface.underline isa Tuple && first(lastface.underline) isa SimpleColor
                    termcolor(io, SimpleColor(:none), '5')
                end
                print(io, ifelse(face.underline == true,
                                ANSI_STYLE_CODES.start_underline,
                                ANSI_STYLE_CODES.end_underline))
            end
        else
            print(io, ifelse(face.underline !== false,
                             ANSI_STYLE_CODES.start_underline,
                             ANSI_STYLE_CODES.end_underline))
        end
    face.strikethrough == lastface.strikethrough || !haskey(Base.current_terminfo, :smxx) ||
        print(io, ifelse(face.strikethrough === true,
                         ANSI_STYLE_CODES.start_strikethrough,
                         ANSI_STYLE_CODES.end_strikethrough))
    face.inverse == lastface.inverse || !haskey(Base.current_terminfo, :enter_reverse_mode) ||
        print(io, ifelse(face.inverse === true,
                         ANSI_STYLE_CODES.start_reverse,
                         ANSI_STYLE_CODES.end_reverse))
end

function _ansi_writer(io::IO, s::Union{<:TaggedString, SubString{<:TaggedString}},
                      string_writer::Function)
    if get(io, :color, false)::Bool
        buf = IOBuffer() # Avoid the overhead in repeatadly printing to `stdout`
        lastface::Face = FACES.default[:default]
        for (str, styles) in eachregion(s)
            face = getface(styles)
            link = let idx=findfirst(==(:link) ∘ first, styles)
                if !isnothing(idx)
                    string(last(styles[idx]))::String
                end end
            !isnothing(link) && write(buf, "\e]8;;", link, "\e\\")
            termstyle(buf, face, lastface)
            string_writer(buf, str)
            !isnothing(link) && write(buf, "\e]8;;\e\\")
            lastface = face
        end
        termstyle(buf, FACES.default[:default], lastface)
        write(io, take!(buf))
    elseif s isa TaggedString
        string_writer(io, s.string)
    elseif s isa SubString
        string_writer(
            io, SubString(s.string.string, s.offset, s.ncodeunits, Val(:noshift)))
    end
end

write(io::IO, s::Union{<:TaggedString, SubString{<:TaggedString}}) =
    _ansi_writer(io, s, write)

print(io::IO, s::Union{<:TaggedString, SubString{<:TaggedString}}) =
    (write(io, s); nothing)

escape_string(io::IO, s::Union{<:TaggedString, SubString{<:TaggedString}},
              esc = ""; keep = ()) =
    (_ansi_writer(io, s, (io, s) -> escape_string(io, s, esc; keep)); nothing)

function write(io::IO, c::TaggedChar)
    if get(io, :color, false) == true
        termstyle(io, getface(c), getface())
        print(io, c.char)
        termstyle(io, getface(), getface(c))
    else
        print(io, c.char)
    end
end

print(io::IO, c::TaggedChar) = (write(io, c); nothing)

function show(io::IO, c::TaggedChar)
    if get(io, :color, false) == true
        out = IOBuffer()
        show(out, c.char)
        print(io, ''', TaggedString(String(take!(out)[2:end-1]), c.properties), ''')
    else
        show(io, c.char)
    end
end

"""
A mapping between ANSI named colors and 8-bit colors for use in HTML
representations.
"""
const HTML_BASIC_COLORS = Dict{Symbol, SimpleColor}(
    :black => SimpleColor(0x00, 0x00, 0x00),
    :red => SimpleColor(0x80, 0x00, 0x00),
    :green => SimpleColor(0x00, 0x80, 0x00),
    :yellow => SimpleColor(0x80, 0x80, 0x00),
    :blue => SimpleColor(0x00, 0x00, 0x80),
    :magenta => SimpleColor(0x80, 0x00, 0x80),
    :cyan => SimpleColor(0x00, 0x80, 0x80),
    :white => SimpleColor(0xc0, 0xc0, 0xc0),
    :bright_black => SimpleColor(0x80, 0x80, 0x80),
    :grey => SimpleColor(0x80, 0x80, 0x80),
    :gray => SimpleColor(0x80, 0x80, 0x80),
    :bright_red => SimpleColor(0xff, 0x00, 0x00),
    :bright_green => SimpleColor(0x00, 0xff, 0x00),
    :bright_yellow => SimpleColor(0xff, 0xff, 0x00),
    :bright_blue => SimpleColor(0x00, 0x00, 0xff),
    :bright_magenta => SimpleColor(0xff, 0x00, 0xff),
    :bright_cyan => SimpleColor(0x00, 0xff, 0xff),
    :bright_white => SimpleColor(0xff, 0xff, 0xff))

function htmlcolor(io::IO, color::SimpleColor)
    if color.value isa Symbol
        if color.value === :default
            print(io, "initial")
        elseif (fg = get(FACES.current[], color.value, getface()).foreground) != SimpleColor(color.value)
            htmlcolor(io, fg)
        else
            htmlcolor(io, get(HTML_BASIC_COLORS, color.value, SimpleColor(:default)))
        end
    else
        (; r, g, b) = color.value
        print(io, '#')
        r < 0x10 && print(io, '0')
        print(io, string(r, base=16))
        g < 0x10 && print(io, '0')
        print(io, string(g, base=16))
        b < 0x10 && print(io, '0')
        print(io, string(b, base=16))
    end
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

function htmlstyle(io::IO, face::Face, lastface::Face=getface())
    print(io, "<span style=\"")
    face.font == lastface.font ||
        print(io, "font-family: \"",
              replace(face.font, '"' => "&quot;", ''' => "&#39;"), '"')
    face.height == lastface.height ||
        print(io, "font-size: ", string(face.height ÷ 10), "pt;")
    face.weight == lastface.weight ||
        print(io, "font-weight: ", get(HTML_WEIGHT_MAP, face.weight, 400), ';')
    face.slant == lastface.slant ||
        print(io, "font-style: ", String(face.slant), ';')
    foreground, background =
        ifelse(face.inverse === true,
               (face.background, face.foreground),
               (face.foreground, face.background))
    lastforeground, lastbackground =
        ifelse(lastface.inverse === true,
               (lastface.background, lastface.foreground),
               (lastface.foreground, lastface.background))
    if foreground != lastforeground
        print(io, "color: ")
        htmlcolor(io, foreground)
        print(io, ';')
    end
    if background != lastbackground
        print(io, "background-color: ")
        htmlcolor(io, background)
        print(io, ';')
    end
    face.underline == lastface.underline ||
        if face.underline isa Tuple # Color and style
            color, style = face.underline
            print(io, "text-decoration: ")
            if !isnothing(color)
                htmlcolor(io, color)
                print(io, ' ')
            end
            print(io, if style == :straight "solid  "
                  elseif style == :double   "double "
                  elseif style == :curly    "wavy   "
                  elseif style == :dotted   "dotted "
                  elseif style == :dashed   "dashed "
                  else "" end)
            print(io, "underline;")
        elseif face.underline isa SimpleColor
            print(io, "text-decoration: ")
            htmlcolor(io, face.underline)
            if lastface.underline isa Tuple && last(lastface.underline) != :straight
                print(io, " solid")
            end
            print(io, " underline;")
        else # must be a Bool
            print(io, "text-decoration: ")
            if lastface.underline isa SimpleColor
                print(io, "currentcolor ")
            elseif lastface.underline isa Tuple
                first(lastface.underline) isa SimpleColor &&
                    print(io, "currentcolor ")
                last(lastface.underline) != :straight &&
                    print(io, "straight ")
            end
            print(io, ifelse(face.underline, "underline;", "none;"))
        end
    face.strikethrough == lastface.strikethrough ||
        print(io, ifelse(face.strikethrough,
                         "text-decoration: line-through",
                         ifelse(face.underline === false,
                                "text-decoration: none", "")))
    print(io, "\">")
end

function show(io::IO, ::MIME"text/html", s::Union{<:TaggedString, SubString{<:TaggedString}}; wrap::Symbol=:pre)
    htmlescape(str) = replace(str, '&' => "&amp;", '<' => "&lt;", '>' => "&gt;")
    buf = IOBuffer() # Avoid potential overhead in repeatadly printing a more complex IO
    wrap == :none ||
        print(buf, '<', String(wrap), '>')
    lastface::Face = getface()
    stylestackdepth = 0
    for (str, styles) in eachregion(s)
        face = getface(styles)
        link = let idx=findfirst(==(:link) ∘ first, styles)
            if !isnothing(idx)
                string(last(styles[idx]))::String
            end end
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
        if wrap == :p
            newpara = false
            for para in eachsplit(str, "\n\n")
                newpara && print(buf, "</p>\n<p>")
                print(buf, htmlescape(para))
                newpara = true
            end
        else
            print(buf, htmlescape(str))
        end
        !isnothing(link) && print(buf, "</a>")
        lastface = face
    end
    print(buf, "</span>" ^ stylestackdepth)
    wrap == :none ||
        print(buf, "</", String(wrap), '>')
    write(io, take!(buf))
    nothing
end
