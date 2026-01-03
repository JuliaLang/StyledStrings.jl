# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

using StyledStrings: StyledStrings, Legacy, SimpleColor, FACES, Face,
    @styled_str, styled, StyledMarkup, @face_str, getface, addface!, loadface!, withfaces, resetfaces!,
    rgbcolor, blend, recolor, setface!, setcolors!,
    AnnotatedString, AnnotatedChar, AnnotatedIOBuffer, annotations
using .StyledMarkup: MalformedStylingMacro

const NON_STDLIB_TESTS = Main == @__MODULE__
if NON_STDLIB_TESTS
    include("styfuzz.jl") # For use in the "Styled Markup" testset
else
    styfuzz() = nothing
end

# For output testing

const vt100 = Base.TermInfo(read(joinpath(@__DIR__, "terminfos", "vt100"), Base.TermInfoRaw))
const fancy_term = Base.TermInfo(read(joinpath(@__DIR__, "terminfos", "fancy"), Base.TermInfoRaw))

function with_terminfo(fn::Function, tinfo::Base.TermInfo)
    # HACK: Directly modifying the value inside `Base.current_terminfo`
    # (a `OncePerProcess`) as we do here relies on private implementation
    # details, which is ill-advised outside of low-stakes testing scenarios.
    # This is a fragile shortcut to avoid modifying the environment and
    # starting a process.
    prev_terminfo = Base.current_terminfo()
    prev_truecolor = getglobal(Base, :have_truecolor)
    @lock Base.current_terminfo.lock try
        Base.current_terminfo.value = tinfo
        setglobal!(Base, :have_truecolor, haskey(tinfo, :setrgbf))
        fn()
    finally
        Base.current_terminfo.value = prev_terminfo
        setglobal!(Base, :have_truecolor, prev_truecolor)
    end
end

StyledStrings.setcolors!([
    :foreground     => (r = 0xf6, g = 0xf5, b = 0xf4),
    :background     => (r = 0x24, g = 0x1f, b = 0x31),
    :black          => (r = 0x1c, g = 0x1a, b = 0x23),
    :red            => (r = 0xa5, g = 0x1c, b = 0x2c),
    :green          => (r = 0x25, g = 0xa2, b = 0x68),
    :yellow         => (r = 0xe5, g = 0xa5, b = 0x09),
    :blue           => (r = 0x19, g = 0x5e, b = 0xb3),
    :magenta        => (r = 0x80, g = 0x3d, b = 0x9b),
    :cyan           => (r = 0x00, g = 0x97, b = 0xa7),
    :white          => (r = 0xdd, g = 0xdc, b = 0xd9),
    :bright_black   => (r = 0x76, g = 0x75, b = 0x7a),
    :bright_red     => (r = 0xed, g = 0x33, b = 0x3b),
    :bright_green   => (r = 0x33, g = 0xd0, b = 0x79),
    :bright_yellow  => (r = 0xf6, g = 0xd2, b = 0x2c),
    :bright_blue    => (r = 0x35, g = 0x83, b = 0xe4),
    :bright_magenta => (r = 0xbf, g = 0x60, b = 0xca),
    :bright_cyan    => (r = 0x26, g = 0xc6, b = 0xda),
    :bright_white   => (r = 0xf6, g = 0xf5, b = 0xf4)
])

const HACKY_FACES = Symbol[]

function hacky_addface!(name::Symbol, face::Face, theme::Symbol = :base)
    # HACK: Directly modifying the `FACES.pool` dictionary to add faces
    # without going through the normal API, for testing purposes.
    if theme == :base
        FACES.pool[name] = face
        FACES.names[face] = name
        push!(HACKY_FACES, name)
    else
        base = FACES.pool[name]
        FACES.themes[theme][base] = face
    end
    face
end

function cleanup_hacky_faces!()
    for name in HACKY_FACES
        f = FACES.pool[name]
        delete!(FACES.pool, name)
        delete!(FACES.names, f)
        for (_, theme) in pairs(FACES.themes)
            delete!(theme, f)
        end
    end
    empty!(HACKY_FACES)
end

"""
    astmatch(template::Expr, expr::Expr)

Check whether `expr` matches the structure of `template`.

The `template` expression may contain the following special forms:
- `_` matches any single expression
- `_...` matches one or more expressions, and may be placed at the start, middle, or end of an argument list
- `_<name>` matches any symbol, and binds it to `_<name>` for consistency checking in subsequent matches
- `_!<name>` matches any symbol starting with `<name>`, ignoring trailing `#<number>` suffixes (for matching generated symbols)
"""
function astmatch(template::Expr, expr::Expr, path::String, bindings::Dict{Symbol, Symbol})
    function pathpart(ex::Expr, argn::Int)
        if ex.head == :call
            if argn == 1
                "$(ex.args[1])()"
            else
                "$(ex.args[1])(.$argn)"
            end
        elseif ex.head == :vect
            "[.$argn]"
        elseif ex.head == :tuple
            "(.$(argn))"
        elseif ex.head == :curly
            if argn == 1
                "$(ex.args[1]){}"
            else
                "$(ex.args[1]){.$argn}"
            end
        else
            "->$(ex.head).$argn"
        end
    end
    template.head == expr.head || return false
    targs = filter(e -> !(e isa LineNumberNode), template.args)
    eargs = filter(e -> !(e isa LineNumberNode), expr.args)
    isempty(targs) && isempty(eargs) && return true
    for (i, e) in enumerate(eargs)
        if e isa Type || e isa Function
            eargs[i] = nameof(e)
        elseif e isa GlobalRef
            eargs[i] = e.name
        end
    end
    t, e = firstindex(targs), firstindex(eargs)
    while t <= lastindex(targs)
        targ, earg = targs[t], eargs[e]
        if targ == :(_...)
            t == lastindex(targs) && return true
            e < lastindex(eargs) || return false
            bindcopy = copy(bindings)
            if astmatch(targs[t+1], eargs[e+1], path * pathpart(expr, e+1), bindcopy) &&
                astmatch(Expr(template.head, targs[t+2:end]...), Expr(expr.head, eargs[e+2:end]...), path * pathpart(expr, e+2), bindcopy)
                merge!(bindings, bindcopy)
                return true
            else
                e += 1
            end
        elseif targ == :_ || astmatch(targ, earg, path * pathpart(expr, e), bindings)
            t, e = t + 1, e + 1
        else
            if !haskey(bindings, :__inner_match_failure_sigil)
                tshow, tdesc = if targ isa Expr; ("`$targ`", targ.head) else (sprint(show, targ), typeof(targ)) end
                eshow, edesc = if earg isa Expr; ("`$earg`", earg.head) else (sprint(show, earg), typeof(earg)) end
                @warn "AST mismatch at $path$(pathpart(expr, e)): expected $tshow ($tdesc), got $eshow ($edesc)"
                bindings[:__inner_match_failure_sigil] = :yep
            end
            return false
        end
    end
    length(targs) == length(eargs)
end

function astmatch(template::Symbol, expr::Symbol, ::String, bindings::Dict{Symbol, Symbol})
    if startswith(String(template), "_!")
        String(template)[3:end] == last(filter(x -> !all(isdigit, x), split(String(expr), '#', keepempty=false)))
    elseif startswith(String(template), '_')
        bind = get(bindings, template, nothing)
        return if isnothing(bind)
            bindings[template] = expr
            true
        else
            expr == bind
        end
    else
        template == expr
    end
end

astmatch(a, b, ::String, ::Dict{Symbol, Symbol}) = a == b

astmatch(a, b) = astmatch(a, b, "", Dict{Symbol, Symbol}())

"""
     stylazy""

A runtime-evaluated version of `@styled_str` macro for use in tests.
"""
macro stylazy_str(s::String)
    esc(:(Core.eval($__module__, :(@styled_str $$s))))
end

# When tested as part of the stdlib, the package prefix can start appearing in show methods.
pkgstrip(s::String) = replace(s, "StyledStrings." => "")

@testset "SimpleColor" begin
    @test SimpleColor(0x01, 0x02, 0x03).value == (r=0x01, g=0x02, b=0x03)
    @test SimpleColor((r=0x01, g=0x02, b=0x03)).value == (r=0x01, g=0x02, b=0x03)
    @test SimpleColor(0x010203).value == (r=0x01, g=0x02, b=0x03)
    @test tryparse(SimpleColor, "green") == SimpleColor(face"green")
    @test tryparse(SimpleColor, "#010203") == SimpleColor(0x010203)
    @test tryparse(SimpleColor, "#12345g") === nothing
    @test tryparse(SimpleColor, "!not a color") === nothing
    @test parse(SimpleColor, "blue") == SimpleColor(face"blue")
    @test_throws ArgumentError parse(SimpleColor, "!not a color")
    @test sprint(show, SimpleColor(face"blue")) |> pkgstrip ==
        "SimpleColor(face\"blue\")"
    @test sprint(show, SimpleColor(0x123456)) |> pkgstrip ==
        "SimpleColor(0x123456)"
    @test sprint(show, MIME("text/plain"), SimpleColor(face"blue")) |> pkgstrip ==
        "SimpleColor(blue)"
    @test sprint(show, MIME("text/plain"), SimpleColor(face"blue"), context = :color => true) |> pkgstrip ==
        "SimpleColor(\e[34m■\e[39m blue)"
    @test sprint(show, MIME("text/plain"), SimpleColor(face"blue"), context = (:color => true, :typeinfo => SimpleColor)) ==
        "\e[34m■\e[39m blue"
end

@testset "Faces" begin
    # Construction
    @test Face() == Face()
    @test all(p -> isnothing(getproperty(Face(), p)), setdiff(propertynames(Face()), (:inherit,)))
    @test isempty(Face().inherit)
    @test Face(font="font").font == "font"
    @test Face(height=1).height == 1
    @test Face(height=0.5).height == 0.5
    @test Face(weight=:bold).weight == :bold
    @test Face(slant=:italic).slant == :italic
    @test Face(foreground=SimpleColor(face"red")).foreground == SimpleColor(face"red")
    @test Face(foreground=face"red").foreground == SimpleColor(face"red")
    @test Face(foreground=0xff0000).foreground == SimpleColor(0xff0000)
    @test Face(foreground="#ff0000").foreground == SimpleColor(0xff0000)
    @test Face(background=SimpleColor(face"red")).background == SimpleColor(face"red")
    @test Face(background=0xff0000).background == SimpleColor(0xff0000)
    @test Face(underline=true).underline == (nothing, :straight)
    @test Face(underline=face"red").underline == (SimpleColor(face"red"), :straight)
    @test Face(underline=(nothing, :curly)).underline == (nothing, :curly)
    @test Face(underline=(face"red", :curly)).underline == (SimpleColor(face"red"), :curly)
    @test Face(strikethrough=true).strikethrough == true
    @test Face(inverse=true).inverse == true
    @test Face(inherit=face"blue").inherit  == [face"blue"]
    @test Face(inherit=[face"blue", face"green"]).inherit == [face"blue", face"green"]
    @test Face(height=1) == Face(height=1)
    @test Face(height=1) != Face(height=2)
    @test Face(inherit=face"red") != Face(inherit=face"blue")
    # Adding a face then resetting
    testface = hacky_addface!(:testface, copy(Face()))
    @test setface!(testface => Face(font="test")) == Face(font="test")
    @test get(FACES.current[], testface, nothing) == Face(font="test")
    @test setface!(face"bold" => Face(weight=:extrabold)) == Face(weight=:extrabold)
    @test FACES.current[][face"bold"] == Face(weight=:extrabold)
    resetfaces!(face"bold")
    @test !haskey(FACES.current[], face"bold")
    @test setface!(testface => Face(height=2.0)) == Face(font="test", height=2.0)
    @test get(FACES.current[], testface, nothing) == Face(font="test", height=2.0)
    resetfaces!(testface)
    @test get(FACES.current[], testface, nothing) === nothing
    # Loading from TOML (a Dict)
    anotherface = hacky_addface!(:anotherface, copy(Face()))
    @test StyledStrings.loaduserfaces!(Dict{String, Any}("anotherface" =>
        Dict{String, Any}("font" => "afont",
                          "height" => 123,
                          "weight" => "semibold",
                          "slant" => "oblique",
                          "foreground" => "green",
                          "background" => "magenta",
                          "underline" => ["blue", "curly"],
                          "strikethrough" => true,
                          "inverse" => true,
                          "inherit" => ["testface"]))) isa Any
    anotherface_customised = Face(
        font = "afont", height = 123, weight = :semibold,
        slant = :oblique, foreground = face"green", background = face"magenta",
        underline = (face"blue", :curly), strikethrough = true,
        inverse = true, inherit = [testface])
    @test get(FACES.current[], anotherface, nothing) == anotherface_customised
    resetfaces!()
    @test haskey(FACES.current[], face"bold") == false
    @test haskey(FACES.current[], testface) == false
    @test haskey(FACES.current[], anotherface) == false
    # `withfaces`
    @test withfaces(testface => Face(font="test2")) do
        get(FACES.current[], testface, nothing)
    end == Face(font="test2")
    @test haskey(FACES.current[], testface) == false
    @test withfaces(face"red" => face"green") do
        get(FACES.current[], face"red", nothing)
    end == face"green"
    @test withfaces(Dict(face"green" => Face(foreground=face"blue"))) do
        get(FACES.current[], face"green", nothing)
    end == Face(foreground=face"blue")
    @test withfaces(() -> 1) == 1
    cleanup_hacky_faces!()
    # Basic merging
    let f1 = Face(height=140, weight=:bold, inherit=[face"bold"])
        f2 = Face(height=1.5, weight=:light, inherit=[face"italic"])
        f3 = Face(height=1.2, slant=:italic)
        @test merge(f1, f2, f3) == Face(height=252, weight=:light, slant=:italic, inherit=[face"bold"]) #\ @test merge(f2, f3) == Face(height=210, weight=:light, slant=:italic, inherit=[:b])
        @test merge(f3, f2, f1) == Face(height=140, weight=:bold, slant=:italic)
        @test merge(f3, f1) == Face(height=140, weight=:bold, slant=:italic)
        @test merge(f3, f2) == Face(height=Float32(1.5) * Float32(1.2), weight=:light, slant=:italic)
    end
    # Merging, inheritence, and canonicalisation
    let aface = hacky_addface!(:a, Face(font="a", height=1.2))
        bface = hacky_addface!(:b, Face(font="b", height=1.1, weight=:light, inherit=aface))
        cface = hacky_addface!(:c, Face(font="c", foreground=face"red", inherit=bface))
        dface = hacky_addface!(:d, Face(font="d", foreground=face"blue", weight=:bold))
        eface = hacky_addface!(:e, Face(font="e", inherit = [cface, dface]))
        fface = hacky_addface!(:f, Face(font="f", inherit = [dface, cface]))
        @test getface(cface) == merge(face"default", aface, bface, Face(height=120), cface)
        @test getface(bface) == merge(face"default", aface, Face(height=120), bface)
        @test getface(aface) == merge(face"default", aface)
        @test getface([cface]) == getface(cface)
        @test getface(bface) == getface(bface)
        @test getface(cface) == getface(cface)
        @test getface([cface, dface]).foreground.value == face"blue"
        @test getface([[cface, dface]]).foreground.value == face"red"
        @test getface(eface).foreground.value == face"red"
        @test getface([dface, cface]).foreground.value == face"red"
        @test getface([[dface, cface]]).foreground.value == face"blue"
        @test getface(fface).foreground.value == face"blue"
        resetfaces!()
        cleanup_hacky_faces!()
    end
    # Equality/hashing equivalence
    let testfaces = [Face(foreground=face"blue"),
                     Face(background=face"blue"),
                     Face(inherit=face"red"),
                     Face(inherit=face"red")]
        for f1 in testfaces, f2 in testfaces
            @test (f1 == f2) == (hash(f1) == hash(f2))
        end
    end
    # Pretty display
    @test sprint(show, MIME("text/plain"), getface()) |> pkgstrip ==
        """
        Face default (sample)
                  font: monospace
                height: 120
                weight: normal
                 slant: normal
            foreground: foreground
            background: background
             underline: false
         strikethrough: false
               inverse: false\
        """
    @test sprint(show, MIME("text/plain"), getface(), context = :color => true) |> pkgstrip ==
        """
        Face \e[1mdefault\e[22m (sample)
                  font: monospace
                height: 120
                weight: normal
                 slant: normal
            foreground: ■ foreground
            background: \e[30m■\e[39m background
             underline: false
         strikethrough: false
               inverse: false\
        """
    @test sprint(show, MIME("text/plain"), face"red", context = :color => true) |> pkgstrip ==
        """
        Face \e[1mred\e[22m (\e[31msample\e[39m)
            foreground: \e[31m■\e[39m red\
        """
    @test sprint(show, face"red") |> pkgstrip == "face\"red\""
    @test sprint(show, copy(face"red")) |> pkgstrip ==
        "Face(foreground = face\"red\")"
    @test sprint(show, MIME("text/plain"), copy(face"red"), context = :compact => true) |> pkgstrip ==
        "Face(foreground = face\"red\")"
    @test sprint(show, MIME("text/plain"), copy(face"red"), context = (:compact => true, :color => true)) |> pkgstrip ==
        "Face(foreground = face\"\e[31mred\e[39m\")"
    @test sprint(show, MIME("text/plain"), copy(face"highlight"), context = :compact => true) |> pkgstrip ==
        "Face(inverse = true, inherit = [face\"emphasis\"])"
    with_terminfo(vt100) do # Not truecolor capable
        @test sprint(show, MIME("text/plain"), copy(face"region"), context = :color => true) |> pkgstrip ==
            """
            Face (\e[48;5;241msample\e[49m)
                background: \e[38;5;241m■\e[39m #636363\
            """
    end
    with_terminfo(fancy_term) do # Truecolor capable
        @test sprint(show, MIME("text/plain"), copy(face"region"), context = :color => true) |> pkgstrip ==
            """
            Face (\e[48;2;99;99;99msample\e[49m)
                background: \e[38;2;99;99;99m■\e[39m #636363\
            """
    end
    with_terminfo(vt100) do # Ensure `enter_reverse_mode` exists
        @test sprint(show, MIME("text/plain"), copy(face"highlight"), context = :color => true) |> pkgstrip ==
            """
            Face (\e[34m\e[7msample\e[39m\e[27m)
                   inverse: true
                   inherit: emphasis(\e[34m*\e[39m)\
            """
    end
end

@testset "Styled Markup" begin
    # FIXME: Since the 'aface'/'bface' references are seen at parse-time,
    # styled"" doesn't see them in time. We want to make macroexpansion be run
    # at runtime instead of parse-time for this to work as intended.
    aface = hacky_addface!(:aface, copy(Face()))
    bface = hacky_addface!(:bface, copy(Face()))
    # Preservation of an unstyled string
    @test styled"some string" == AnnotatedString("some string")
    # Basic styled constructs
    @test styled"{thing=val:some} string" == AnnotatedString("some string", [(1:4, :thing, "val")])
    @test styled"some {thing=val:string}" == AnnotatedString("some string", [(6:11, :thing, "val")])
    @test styled"some {a=1:s}trin{b=2:g}" == AnnotatedString("some string", [(6:6, :a, "1"), (11:11, :b, "2")])
    @test styled"{thing=val with spaces:some} string" == AnnotatedString("some string", [(1:4, :thing, "val with spaces")])
    @test stylazy"{aface:some} string" == AnnotatedString("some string", [(1:4, :face, aface)])
    # Annotation prioritisation
    @test stylazy"{aface,bface:some} string" ==
        AnnotatedString("some string", [(1:4, :face, aface), (1:4, :face, bface)])
    @test stylazy"{aface:{bface:some}} string" ==
        AnnotatedString("some string", [(1:4, :face, aface), (1:4, :face, bface)])
    @test stylazy"{aface,bface:$(1)} string" ==
        AnnotatedString("1 string", [(1:1, :face, aface), (1:1, :face, bface)])
    @test stylazy"{aface:{bface:$(1)}} string" ==
        AnnotatedString("1 string", [(1:1, :face, aface), (1:1, :face, bface)])
    # Inline face attributes
    @test styled"{(slant=italic):some} string" ==
        AnnotatedString("some string", [(1:4, :face, Face(slant=:italic))])
    @test styled"{(foreground=magenta,background=#555555):some} string" ==
        AnnotatedString("some string", [(1:4, :face, Face(foreground=face"magenta", background=0x555555))])
    # Inline face attributes: empty attribute lists are legal
    @test styled"{():}" == styled"{( ):}" == AnnotatedString("", [(1:0, :face, Face())])
    # Inline face attributes: leading/trailing whitespace
    @test styled"{ ( fg=red , ) :a}" == AnnotatedString("a", [(1:1, :face, Face(foreground=face"red"))])
    # Inline face attributes: each recognised key
    @test styled"{(font=serif):a}" == AnnotatedString("a", [(1:1, :face, Face(font="serif"))])
    @test styled"{(font=some serif):a}" == AnnotatedString("a", [(1:1, :face, Face(font="some serif"))])
    @test styled"{(font=\"some serif\"):a}" == AnnotatedString("a", [(1:1, :face, Face(font="some serif"))])
    @test styled"{(font=\"{},):\"):a}" == AnnotatedString("a", [(1:1, :face, Face(font="{},):"))])
    @test styled"{(height=120):a}" == AnnotatedString("a", [(1:1, :face, Face(height=120))])
    @test styled"{(height=1.2):a}" == AnnotatedString("a", [(1:1, :face, Face(height=1.2))])
    @test styled"{(weight=normal):a}" == AnnotatedString("a", [(1:1, :face, Face(weight=:normal))])
    @test styled"{(weight=bold):a}" == AnnotatedString("a", [(1:1, :face, Face(weight=:bold))])
    @test styled"{(slant=italic):a}" == AnnotatedString("a", [(1:1, :face, Face(slant=:italic))])
    @test styled"{(fg=red):a}" == AnnotatedString("a", [(1:1, :face, Face(foreground=face"red"))])
    @test styled"{(foreground=red):a}" == AnnotatedString("a", [(1:1, :face, Face(foreground=face"red"))])
    @test styled"{(bg=red):a}" == AnnotatedString("a", [(1:1, :face, Face(background=face"red"))])
    @test styled"{(background=red):a}" == AnnotatedString("a", [(1:1, :face, Face(background=face"red"))])
    @test styled"{(underline=true):a}" == AnnotatedString("a", [(1:1, :face, Face(underline=true))])
    @test styled"{(underline=cyan):a}" == AnnotatedString("a", [(1:1, :face, Face(underline=face"cyan"))])
    @test styled"{(underline=(cyan,curly)):a}" == AnnotatedString("a", [(1:1, :face, Face(underline=(face"cyan", :curly)))])
    @test styled"{(strikethrough=true):a}" == AnnotatedString("a", [(1:1, :face, Face(strikethrough=true))])
    @test styled"{(inverse=true):a}" == AnnotatedString("a", [(1:1, :face, Face(inverse=true))])
    @test stylazy"{(inherit=bface):a}" == AnnotatedString("a", [(1:1, :face, Face(inherit=bface))])
    @test stylazy"{(inherit=[aface,bface]):a}" == AnnotatedString("a", [(1:1, :face, Face(inherit=[aface, bface]))])
    # Curly bracket escaping
    @test styled"some \{string" == AnnotatedString("some {string")
    @test styled"some string\}" == AnnotatedString("some string}")
    @test styled"some \{string\}" == AnnotatedString("some {string}")
    @test styled"some \{str:ing\}" == AnnotatedString("some {str:ing}")
    @test styled"some \{{bold:string}\}" == AnnotatedString("some {string}", [(7:12, :face, face"bold")])
    @test styled"some {bold:string \{other\}}" == AnnotatedString("some string {other}", [(6:19, :face, face"bold")])
    # Nesting
    @test styled"{bold:nest{italic:ed st{red:yling}}}" ==
        AnnotatedString(
            "nested styling", [(1:14, :face, face"bold"), (5:14, :face, face"italic"), (10:14, :face, face"red")])
    # Production of a `(AnnotatedString)` value instead of an expression when possible
    @test AnnotatedString("val") == @macroexpand styled"val"
    @test AnnotatedString("val", [(1:3, :face, aface)]) == @macroexpand styled"{aface:val}"
    # Interpolation
    @test astmatch(
        :(let ;
              _!val_str = string(val)
              _!offset_val = ncodeunits(_!val_str)
              _!annots = _[]
              _!interp_annot_count = 0
              _...
              _!interp_annots = if _
                  _!annots
              else
                  Vector{_}(_!annots)
              end
              _...
              AnnotatedString(_!val_str, _!interp_annots)
          end),
        @macroexpand styled"$val")
    @test astmatch(
        :(let ;
              _...
              AnnotatedString(string("a", _!val_str), _!interp_annots)
          end),
        @macroexpand styled"a$val")
    @test astmatch(
        :(let ;
              _...
              AnnotatedString(string("a", _!val_str, "b"), _!interp_annots)
          end),
        @macroexpand styled"a$(val)b")
    @test astmatch(
        :(let ;
              _r = FACES.remapping[]
              _!face_red = get(_r, $(face"red"), $(face"red"))
              _!val_str = string(val)
              _!offset_val = ncodeunits(_!val_str)
              _!annots = _[(; region = 1:0 + _!offset_val, label = :face, value = _!face_red)]
              _...
              AnnotatedString(_!val_str, _!interp_annots)
          end),
        @macroexpand styled"{red:$val}")
    @test astmatch(
        :(let ;
              _i = interpface(face, false)
              _r = FACES.remapping[]
              _f = get(_r, _i, _i)
              AnnotatedString("val", _[(; region = 1:3, label = :face, value = _f)])
          end),
        @macroexpand styled"{$face:val}")
    @test astmatch(
        :(let ;
              _i1 = interpface(f1, false)
              _r = FACES.remapping[]
              _f1 = get(_r, _i1, _i1)
              _i2 = interpface(f2, false)
              _f2 = get(_r, _i2, _i2)
              AnnotatedString("v1v2", _[(; region = 1:2, label = :face, value = _f), (; region = 3:4, label = :face, value = _f2)])
          end),
        @macroexpand styled"{$f1:v1}{$f2:v2}")
    @test astmatch(
        :(let ;
              _...
              AnnotatedString("text", _[(; region = 1:4, label = key, value = "val")])
          end),
        @macroexpand styled"{$key=val:text}")
    @test astmatch(
        :(let ;
              _...
              AnnotatedString("text", _[(; region = 1:4, label = key, value = val)])
          end),
        @macroexpand styled"{$key=$val:text}")
    @test astmatch(
        :(let ;
              AnnotatedString("val", _[(; region = 1:3, label = :face, value = Face(foreground = color))])
          end),
        @macroexpand styled"{(foreground=$color):val}"
    )
    # Partial annotation termination with interpolation
    @test styled"{green:a}{red:{blue:b}$('c')}" ==
        AnnotatedString{String}("abc", [(1:1, :face, face"green"),
                                        (2:3, :face, face"red"),
                                        (2:2, :face, face"blue")])

    # Trailing (and non-trailing) Backslashes
    @test String(styled"\\") == "\\"
    @test String(styled"\\\\") == "\\\\"
    @test String(styled"\\\\\\") == "\\\\\\"
    @test String(styled".\\") == ".\\"
    @test String(styled".\\\\") == ".\\\\"
    @test String(styled".\\\\\\") == ".\\\\\\"

    # newlines
    strlines = "abc\
                def"
    stylines = styled"abc\
                      def"
    @test strlines == stylines == "abcdef"

    strlines = "abc\\ndef"
    stylines = styled"abc\\ndef"
    @test strlines == stylines == "abc\\ndef"

    strlines = eval(Meta.parse("\"abc\\\n \tdef\""))
    stylines = eval(Meta.parse("styled\"abc\\\n \tdef\""))
    @test strlines == stylines == "abcdef"

    strlines = eval(Meta.parse("\"abc\\\r\n  def\""))
    stylines = eval(Meta.parse("styled\"abc\\\r\n  def\""))
    @test strlines == stylines == "abcdef"

    # The function form. As this uses the same FSM as the macro,
    # we don't need many tests to verify it's behaving sensibly.
    @test styled("{red:hey} {blue:there}") == styled"{red:hey} {blue:there}"
    @test styled("\\{green:hi\\}") == styled"\{green:hi\}"
    @test styled("\$hey") == styled"\$hey"

    # Various kinds of syntax errors that should be reported
    @test_throws MalformedStylingMacro styled("{incomplete")
    @test_throws MalformedStylingMacro styled("{unterminated:")
    # @test_throws LoadError styled("$") # FIXME still throws 😢
    @test_throws MalformedStylingMacro styled("}")
    @test_throws MalformedStylingMacro styled("{(:}")
    @test_throws MalformedStylingMacro styled("{(underline=()):}")
    @test_throws MalformedStylingMacro styled("{(underline=(_)):}")
    @test_throws MalformedStylingMacro styled("{(underline=(_,invalid)):}")
    @test_throws MalformedStylingMacro styled("{(height=invalid):}")
    @test_throws MalformedStylingMacro styled("{(weight=invalid):}")
    @test_throws MalformedStylingMacro styled("{(slant=invalid):}")
    @test_throws MalformedStylingMacro styled("{(invalid=):}")
    # Test the error printing too
    aio = AnnotatedIOBuffer()
    try
        styled("{")
    catch err
        showerror(aio, err)
    end
    errstr = read(seekstart(aio), AnnotatedString)
    # @test errstr ==
    #     styled"MalformedStylingMacro\n\
    #            {error:│} Incomplete annotation (missing closing '{warning:\}}'):\n\
    #            {error:│}  {bright_green:\"\{\"}\n\
    #            {error:│}   {info:╰─╴starts here}\n\
    #            {error:┕} {light,italic:1 issue}\n"
end

# Markup fuzzing!
styfuzz()

@testset "AnnotatedIOBuffer" begin
    aio = AnnotatedIOBuffer()
    @test write(aio, styled"{red:hey} {blue:there}") == 9
    buf = IOBuffer()
    @test write(buf, seekstart(aio)) == 9
    @test String(take!(buf)) == "hey there"
    cbuf = IOContext(buf, :color => true)
    @test write(cbuf, seekstart(aio)) == 29
    @test String(take!(buf)) == "\e[31mhey\e[39m \e[34mthere\e[39m"
end

@testset "ANSI encoding" begin
    # 4-bit color
    @test StyledStrings.ansi_4bit(
        StyledStrings.ANSI_4BIT_COLORS[face"cyan"], false) == 36
    @test StyledStrings.ansi_4bit(
        StyledStrings.ANSI_4BIT_COLORS[face"cyan"], true) == 46
    @test StyledStrings.ansi_4bit(
        StyledStrings.ANSI_4BIT_COLORS[face"bright_cyan"], false) == 96
    @test StyledStrings.ansi_4bit(
        StyledStrings.ANSI_4BIT_COLORS[face"bright_cyan"], true) == 106
    # 8-bit color
    @test sprint(StyledStrings.termcolor8bit, (r=0x40, g=0x63, b=0xd8), '3') == "\e[38;5;26m"
    @test sprint(StyledStrings.termcolor8bit, (r=0x38, g=0x98, b=0x26), '3') == "\e[38;5;28m"
    @test sprint(StyledStrings.termcolor8bit, (r=0x95, g=0x58, b=0xb2), '3') == "\e[38;5;97m"
    @test sprint(StyledStrings.termcolor8bit, (r=0xcb, g=0x3c, b=0x33), '3') == "\e[38;5;160m"
    @test sprint(StyledStrings.termcolor8bit, (r=0xee, g=0xee, b=0xee), '3') == "\e[38;5;255m"
    # 24-bit color
    @test sprint(StyledStrings.termcolor24bit, (r=0x40, g=0x63, b=0xd8), '3') == "\e[38;2;64;99;216m"
    @test sprint(StyledStrings.termcolor24bit, (r=0x38, g=0x98, b=0x26), '3') == "\e[38;2;56;152;38m"
    @test sprint(StyledStrings.termcolor24bit, (r=0x95, g=0x58, b=0xb2), '3') == "\e[38;2;149;88;178m"
    @test sprint(StyledStrings.termcolor24bit, (r=0xcb, g=0x3c, b=0x33), '3') == "\e[38;2;203;60;51m"
    # The color reset method
    @test sprint(StyledStrings.termcolor, nothing, '3') == "\e[39m"
    # ANSI attributes
    function ansi_change(; attrs...)
        face = getface(Face(; attrs...))
        dface = getface()
        sprint(StyledStrings.termstyle, face, dface),
        sprint(StyledStrings.termstyle, dface, face)
    end
    with_terminfo(vt100) do
        @test ansi_change(foreground=face"cyan") == ("\e[36m", "\e[39m")
        @test ansi_change(background=face"cyan") == ("\e[46m", "\e[49m")
        @test ansi_change(weight=:bold) == ("\e[1m", "\e[22m")
        @test ansi_change(weight=:extrabold) == ("\e[1m", "\e[22m")
        @test ansi_change(inverse=true) == ("\e[7m", "\e[27m")
        # Reduced-capability behaviours
        @test ansi_change(foreground=(r=0x40, g=0x63, b=0xd8)) == ("\e[38;5;26m", "\e[39m")
        @test ansi_change(background=(r=0x40, g=0x63, b=0xd8)) == ("\e[48;5;26m", "\e[49m")
        @test ansi_change(weight=:light) == ("", "\e[22m")
        @test ansi_change(slant=:italic) == ("\e[4m", "\e[24m")
        @test ansi_change(underline=true) == ("\e[4m", "\e[24m")
        @test ansi_change(underline=face"green") == ("\e[4m", "\e[24m")
        @test ansi_change(strikethrough=true) == ("", "")
    end
    with_terminfo(fancy_term) do
        # Extra-capability behaviours
        @test ansi_change(foreground=(r=0x40, g=0x63, b=0xd8)) == ("\e[38;2;64;99;216m", "\e[39m")
        @test ansi_change(background=(r=0x40, g=0x63, b=0xd8)) == ("\e[48;2;64;99;216m", "\e[49m")
        @test ansi_change(weight=:light) == ("\e[2m", "\e[22m")
        @test ansi_change(slant=:italic) == ("\e[3m", "\e[23m")
        @test ansi_change(underline=face"green") == ("\e[4m\e[58;5;2m", "\e[59m\e[24m")
        @test ansi_change(underline=:straight) == ("\e[4m", "\e[24m")
        @test ansi_change(underline=:double) == ("\e[4:2m", "\e[24m")
        @test ansi_change(underline=:curly)  == ("\e[4:3m", "\e[24m")
        @test ansi_change(underline=:dotted) == ("\e[4:4m", "\e[24m")
        @test ansi_change(underline=:dashed) == ("\e[4:5m", "\e[24m")
        @test ansi_change(underline=(face"cyan", :double)) == ("\e[4:2m\e[58;5;6m", "\e[59m\e[24m")
        @test ansi_change(strikethrough=true) == ("\e[9m", "\e[29m")
    end
    # AnnotatedChar
    @test sprint(print, AnnotatedChar('a')) == "a"
    @test sprint(print, AnnotatedChar('a', [(:face, face"red")]), context = :color => true) == "\e[31ma\e[39m"
    @test sprint(show, AnnotatedChar('a')) == "'a'"
    @test sprint(show, AnnotatedChar('a', [(:face, face"red")]), context = :color => true) == "'\e[31ma\e[39m'"
    # Might as well put everything together for a final test
    fancy_string = styled"The {magenta:`{green:StyledStrings}`} package {italic:builds}\
        {bold: on top} of the {magenta:`{green:AnnotatedString}`} {link={https://en.wikipedia.org/wiki/Type_system}:type} \
        to provide a {(underline=(red,curly)):full-fledged} textual {(bg=#4063d8,fg=#adbdf8,inherit=[bold,strikethrough]):styling} \
        system, suitable for {inverse:terminal} and graphical displays."
    @test sprint(print, fancy_string) == "The `StyledStrings` package builds on top of \
        the `AnnotatedString` type to provide a full-fledged textual styling system, suitable \
        for terminal and graphical displays."
    @test sprint(print, fancy_string[1:27], context = :color => true) ==
        "The \e[35m`\e[32mStyledStrings\e[35m`\e[39m package"
    with_terminfo(vt100) do
        @test sprint(print, fancy_string, context = :color => true) ==
            "The \e[35m`\e[32mStyledStrings\e[35m`\e[39m package \e[4mbuilds\
             \e[1m\e[24m on top\e[22m of the \e[35m`\e[32mAnnotatedString\e[35m`\e[39m \
             \e]8;;https://en.wikipedia.org/wiki/Type_system\e\\type\e]8;;\e\\ to provide \
             a \e[4mfull-fledged\e[24m textual \e[38;5;147m\e[48;5;26m\e[1mstyling\e[39m\e[49m\e[22m \
             system, suitable for \e[7mterminal\e[27m and graphical displays."
    end
    with_terminfo(fancy_term) do
        @test sprint(print, fancy_string, context = :color => true) ==
            "The \e[35m`\e[32mStyledStrings\e[35m`\e[39m package \
            \e[3mbuilds\e[1m\e[23m on top\e[22m of the \e[35m`\e[32mAnnotatedString\
            \e[35m`\e[39m \e]8;;https://en.wikipedia.org/wiki/Type_system\e\\type\e]8;;\e\
            \\ to provide a \e[4:3m\e[58;5;1mfull-fledged\e[59m\e[24m textual \
            \e[38;2;173;189;248m\e[48;2;64;99;216m\e[1m\e[9mstyling\e[39m\e[49m\e[22m\e[29m system, \
            suitable for \e[7mterminal\e[27m and graphical displays."
    end
end

@testset "HTML encoding" begin
    @test sprint(StyledStrings.htmlcolor, SimpleColor(face"black")) == "#1c1a23"
    @test sprint(StyledStrings.htmlcolor, SimpleColor(face"green")) == "#25a268"
    @test sprint(StyledStrings.htmlcolor, SimpleColor(face"warning")) == "#e5a509"
    @test sprint(StyledStrings.htmlcolor, SimpleColor(Face())) == "#ff00ff"
    @test sprint(StyledStrings.htmlcolor, SimpleColor(0x40, 0x63, 0xd8)) == "#4063d8"
    function html_change(; attrs...)
        face = getface(Face(; attrs...))
        sprint(StyledStrings.htmlstyle, face)
    end
    @test html_change(foreground=face"cyan") == "<span style=\"color: #0097a7\">"
    @test html_change(background=face"cyan") == "<span style=\"background-color: #0097a7\">"
    @test html_change(weight=:bold) == "<span style=\"font-weight: 700\">"
    @test html_change(weight=:extrabold) == "<span style=\"font-weight: 800\">"
    @test html_change(weight=:light) == "<span style=\"font-weight: 300\">"
    @test html_change(foreground=face"blue", background=face"red", inverse=true) ==
        "<span style=\"color: #a51c2c; background-color: #195eb3\">"
    @test html_change(slant=:italic) == "<span style=\"font-style: italic\">"
    @test html_change(height=180) == "<span style=\"font-size: 18pt\">"
    @test html_change(underline=true) == "<span style=\"text-decoration: underline\">"
    @test html_change(underline=face"green") == "<span style=\"text-decoration: #25a268 underline\">"
    @test html_change(underline=:straight) == "<span style=\"text-decoration: underline\">"
    @test html_change(underline=:double) == "<span style=\"text-decoration: double underline\">"
    @test html_change(underline=:curly)  == "<span style=\"text-decoration: wavy underline\">"
    @test html_change(underline=:dotted) == "<span style=\"text-decoration: dotted underline\">"
    @test html_change(underline=:dashed) == "<span style=\"text-decoration: dashed underline\">"
    @test html_change(underline=(face"cyan", :double)) == "<span style=\"text-decoration: #0097a7 double underline\">"
    @test html_change(strikethrough=true) == "<span style=\"text-decoration: line-through\">"
    # Might as well put everything together for a final test
    fancy_string = styled"The {magenta:`{green:StyledStrings}`} package {italic:builds}\
        {bold: on top} of the {magenta:`{green:AnnotatedString}`} {link={https://en.wikipedia.org/wiki/Type_system}:type} \
        to provide a {(underline=(red,curly)):full-fledged} textual {(bg=#4063d8,fg=#adbdf8,inherit=[bold,strikethrough]):styling} \
        system, suitable for {inverse:terminal} and graphical displays."
    @test sprint(show, MIME("text/html"), fancy_string[1:27]) ==
        "The <span style=\"color: #803d9b\">`</span><span style=\"color: #25a268\">StyledStrings</span>\
        <span style=\"color: #803d9b\">`</span> package"
    @test sprint(show, MIME("text/html"), fancy_string) ==
        "The <span style=\"color: #803d9b\">`</span><span style=\"color: #25a268\">StyledStrings</span><span style=\"color: #803d9b\">`</span> \
        package <span style=\"font-style: italic\">builds<span style=\"font-weight: 700; font-style: normal\"> on top</span></span> of the \
        <span style=\"color: #803d9b\">`</span><span style=\"color: #25a268\">AnnotatedString</span><span style=\"color: #803d9b\">`</span> \
        <a href=\"https://en.wikipedia.org/wiki/Type_system\">type</a> to provide a <span style=\"text-decoration: #a51c2c wavy underline\">\
        full-fledged</span> textual <span style=\"font-weight: 700; color: #adbdf8; background-color: #4063d8; text-decoration: line-through\">\
        styling</span> system, suitable for <span style=\"color: $(StyledStrings.HTML_FGBG.background); background-color: $(StyledStrings.HTML_FGBG.foreground)\">terminal</span> and graphical displays."
end

@testset "Legacy" begin
    @test Legacy.legacy_color(:blue) == SimpleColor(face"blue")
    @test Legacy.legacy_color(:light_blue) == SimpleColor(face"bright_blue")
    @test Legacy.legacy_color(-1) === nothing
    @test Legacy.legacy_color(0) == SimpleColor(0x000000)
    @test Legacy.legacy_color(44) == SimpleColor(0x00d7d7)
    @test Legacy.legacy_color(255) == SimpleColor(0xeeeeee)
    @test Legacy.legacy_color(256) === nothing
    @test Legacy.legacy_color("blue") == SimpleColor(face"blue")
    @test Legacy.legacy_color("light_blue") == SimpleColor(face"bright_blue")
    @test Legacy.legacy_color("-1") === nothing
    @test Legacy.legacy_color("0") == SimpleColor(0x000000)
    @test Legacy.legacy_color("44") == SimpleColor(0x00d7d7)
    @test Legacy.legacy_color("255") == SimpleColor(0xeeeeee)
    @test Legacy.legacy_color("256") === nothing
    @test Legacy.legacy_color("invalid") === nothing
    withenv("JULIA_INFO_COLOR" => "magenta") do
        Legacy.load_env_colors!() isa Any
        @test getface(face"info").foreground.value == face"magenta"
        StyledStrings.resetfaces!()
    end
    aio = AnnotatedIOBuffer()
    @test printstyled(aio, "a", bold=true)      |> isnothing
    @test printstyled(aio, "b", italic=true)    |> isnothing
    @test printstyled(aio, "c", underline=true) |> isnothing
    @test printstyled(aio, "d", reverse=true)   |> isnothing
    @test printstyled(aio, "e", color=:green)   |> isnothing
    @test read(seekstart(aio), AnnotatedString) == styled"{bold:a}{italic:b}{underline:c}{inverse:d}{(fg=green):e}"
end

@testset "Recoloring" begin
    @testset "RGB" begin
        @test rgbcolor(face"red") == FACES.basecolors[face"red"]
        @test rgbcolor(SimpleColor(face"red")) == FACES.basecolors[face"red"]
        indirect = hacky_addface!(:indirect, copy(Face()))
        another = hacky_addface!(:another, copy(Face()))
        final = hacky_addface!(:final, copy(Face()))
        @test withfaces(() -> rgbcolor(SimpleColor(indirect)),
                        [indirect => Face(foreground=another),
                         another => Face(foreground=final),
                         final => Face(foreground=face"red")]) == FACES.basecolors[face"red"]
        @test rgbcolor(indirect) == StyledStrings.UNRESOLVED_COLOR_FALLBACK
    end
    @testset "Blending" begin
        @test blend((r = 0x00, g = 0x00, b = 0xff) => 0.5, (r = 0xff, g=0xff, b=0x00) => 0.5) ==
            (r = 0x6b, g = 0xaa, b = 0xc6)
        @test blend(SimpleColor(0x0000ff) => 0.5, SimpleColor(0xffff00) => 0.5) ==
            SimpleColor(0x6baac6)
        @test blend(SimpleColor(0x000000) => 0.2, SimpleColor(0xffffff) => 0.6, SimpleColor(0x00ff00) => 0.2) ==
            SimpleColor(0x9fbe9c)
        withfaces([face"blue" => Face(foreground=0x0000ff),
                   face"yellow" => Face(foreground=0xffff00)]) do
                       @test blend(face"blue" => 0.5, face"yellow" => 0.5) == SimpleColor(0x6baac6)
                   end
    end
    @testset "Theme change" begin
        lightfbg = [:foreground => (r = 0x00, g = 0x00, b = 0x00),
                    :background => (r = 0xff, g = 0xff, b = 0xff),
                    :yellow     => (r = 0xfc, g = 0xce, b = 0x7b)]
        darkfbg = [:foreground => (r = 0xff, g = 0xff, b = 0xff),
                   :background => (r = 0x00, g = 0x00, b = 0x00),
                   :yellow     => (r = 0xa7, g = 0x7e, b = 0x27)]
        setcolors!(lightfbg)
        counter = Ref(0)
        recolor() do
            counter[] += 1
        end
        @test counter[] == 0
        test_lightdark = hacky_addface!(:test_lightdark, Face(foreground=0x000001))
        hacky_addface!(:test_lightdark, Face(foreground=0x000002), :light)
        hacky_addface!(:test_lightdark, Face(foreground=0x000003), :dark)
        @test rgbcolor(SimpleColor(test_lightdark)).b == 0x01
        setcolors!(lightfbg)
        @test counter[] == 1
        @test rgbcolor(SimpleColor(test_lightdark)).b == 0x02
        setcolors!(darkfbg)
        @test counter[] == 2
        @test rgbcolor(SimpleColor(test_lightdark)).b == 0x03
        recolor() do
            setface!(test_lightdark => Face(foreground=blend(:background => 0.6, :foreground => 0.3, :yellow => 0.1)))
        end
        setcolors!(lightfbg)
        @test getface(test_lightdark).foreground.value == (r = 0x9d, g = 0x99, b = 0x92)
        setcolors!(darkfbg)
        @test getface(test_lightdark).foreground.value == (r = 0x43, g = 0x40, b = 0x3a)
        cleanup_hacky_faces!()
    end
end

if NON_STDLIB_TESTS
    @testset "Backwards compatability" begin
        @test convert(SimpleColor, :red).value == face"red"
        @test Face(foreground=:red).foreground == SimpleColor(face"red")
        @test Face(foreground="red").foreground == SimpleColor(face"red")
        @test Face(background=:red).background == SimpleColor(face"red")
        @test Face(background="red").background == SimpleColor(face"red")
        @test Face(underline=:red).underline == (SimpleColor(face"red"), :straight)
        @test Face(underline=(:red, :curly)).underline == (SimpleColor(face"red"), :curly)
        @test Face(inherit=:blue).inherit == [face"blue"]
        @test Face(inherit=[:blue, :green]).inherit == [face"blue", face"green"]
        @test withfaces(:red => :green) do
            get(FACES.current[], face"red", nothing)
        end == Face(foreground=:green)
        @test withfaces(:red => [:green, :inverse]) do
            get(FACES.current[], face"red", nothing)
        end == Face(inherit=[:green, :inverse])
    end
end
