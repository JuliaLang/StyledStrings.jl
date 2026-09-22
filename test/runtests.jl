# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

using StyledStrings: StyledStrings, Legacy, SimpleColor, FACES, Face,
    @styled_str, styled, StyledMarkup, @face_str, getface, addface!, loadface!, withfaces, resetfaces!,
    rgbcolor, blend, recolor, setface!, setcolors!,
    AnnotatedString, AnnotatedChar, AnnotatedIOBuffer, annotations
using .StyledMarkup: MalformedStylingMacro

@test isempty(Test.detect_closure_boxes(StyledStrings))

@test isempty(Test.detect_ambiguities(StyledStrings))

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
    @test Face(foreground = SubString("#010203", 1)) == Face(foreground = 0x010203)
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
    @test Face(height=typemax(Int32)).height == typemax(Int32)
    @test_throws ArgumentError Face(height=2^31)
    @test_throws ArgumentError Face(height=-1)
    @test_throws ArgumentError Face(height=Inf)
    @test_throws ArgumentError Face(height=1e300) # Beyond `Float32`
    @test_throws ArgumentError Face(height=1e-46) # Rounds to zero in `Float32`
    @test Face(weight=:bold).weight == :bold
    @test Face(slant=:italic).slant == :italic
    for (attr, names) in ((:weight, StyledStrings.ATTRIBUTES.weights), (:slant, StyledStrings.ATTRIBUTES.slants))
        for name in names
            @test getproperty(Face(; attr => name), attr) === name
        end
        @test_throws ArgumentError Face(; attr => :unknown)
    end
    for style in StyledStrings.ATTRIBUTES.underlines
        @test Face(underline=style).underline == (nothing, style)
        @test Face(underline=(face"red", style)).underline == (SimpleColor(face"red"), style)
    end
    @test_throws ArgumentError Face(underline=(nothing, :unknown))
    @test sizeof(StyledStrings.FaceDef) + sizeof(Int) <= 64 # A `Face` fits one 64-byte allocation
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
    @test Face(underline=false).underline === nothing
    @test merge(Face(underline=face"red"), Face(underline=false)).underline === nothing
    @test merge(Face(underline=face"red"), Face(underline=false), Face(underline=:curly)).underline == (nothing, :curly)
    # true and a bare style keep an inherited colour, a tuple or false set the whole underline
    @test merge(Face(underline=face"red"), Face(underline=true)).underline == (SimpleColor(face"red"), :straight)
    @test merge(Face(underline=face"red"), Face(underline=:curly)).underline == (SimpleColor(face"red"), :curly)
    @test merge(Face(underline=face"red"), Face(underline=(nothing, :straight))).underline == (nothing, :straight)
    @test merge(Face(underline=face"red"), Face(underline=(nothing, :curly))).underline == (nothing, :curly)
    @test Face(strikethrough=true).strikethrough == true
    @test Face(inverse=true).inverse == true
    @test Face(inherit=face"blue").inherit  == [face"blue"]
    @test Face(inherit=[face"blue", face"green"]).inherit == [face"blue", face"green"]
    @test Face(height=1) == Face(height=1)
    @test Face(height=1) != Face(height=2)
    @test Face(inherit=face"red") != Face(inherit=face"blue")
    # A vector with spare capacity is copied, not aliased
    let grown = Face[]
        push!(grown, face"italic")
        @test Face(inherit=grown).inherit == [face"italic"]
        @test getface(Face(inherit=grown)).slant == :italic
    end
    # Standard faces
    @test all(f -> f.weight == :bold, (face"log_error", face"log_warn", face"log_info", face"log_debug"))
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
    # Customising the default face
    setface!(face"default" => Face(font="custom"))
    @test getface().font == "custom"
    @test getface(face"red").font == "custom"
    resetfaces!(face"default")
    @test getface().font == "monospace"
    with_terminfo(vt100) do
        setface!(face"default" => Face(weight=:bold))
        @test sprint(print, styled"x{(weight=normal):y}", context = :color => true) == "\e[1mx\e[22my"
        resetfaces!(face"default")
    end
    # Loading from TOML (a Dict)
    @test convert(Face, Dict{String, Any}("underline" => true)) == Face(underline=true)
    @test convert(Face, Dict{String, Any}("underline" => false)) == Face(underline=false)
    @test convert(Face, Dict{String, Any}("weight" => "wobbly", "underline" => ["red", "wavy"])) ==
        Face(underline = face"red")   # Unknown names are left unset
    @test convert(Face, Dict{String, Any}("height" => 1.5)).height == 1.5
    @test convert(Face, Dict{String, Any}("height" => -3)).height === nothing
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
    # A face named by a `Symbol` in a list resolves through the current definitions
    @test withfaces(face"bold" => Face(foreground = face"red")) do
        getface([:bold]).foreground
    end == SimpleColor(face"red")
    @test withfaces(() -> 1) == 1
    # A face given as the new definition is taken as currently defined
    setface!(face"blue" => Face(font="bluefont"))
    @test withfaces(() -> getface(face"red").font, face"red" => face"blue") == "bluefont"
    @test withfaces(() -> getface(face"red").font, face"red" => [face"blue"]) == "bluefont"
    resetfaces!(face"blue")
    # `remapfaces`
    @test StyledStrings.remapfaces(styled"{red:a}{note=x:b}", face"red" => face"blue") ==
        AnnotatedString("ab", [(1:1, :face, face"blue"), (2:2, :note, "x")])
    # Only annotation values are substituted, not the attributes of a face
    @test StyledStrings.remapfaces(styled"{(foreground=red):a}", face"red" => face"blue") ==
        AnnotatedString("a", [(1:1, :face, Face(foreground = face"red"))])
    # The face cache serves an explicit instance, and evicts correctly under churn
    cache = StyledStrings.emptycache()
    @test getface(face"red", cache) == getface(face"red")
    adhoc = [Face(foreground = face"blue", height = i) for i in 1:2000]
    @test all(f -> getface(f, cache) == merge(getface(), f), adhoc)
    @test all(f -> getface(f) == merge(getface(), f), adhoc)
    @test getface(Face()) == getface()
    # Only the face annotations at a position count
    @test getface(styled"{link={https://x}:y}", 1) == getface()
    @test getface(styled"{red,note=x:y}", 1).foreground == SimpleColor(face"red")
    # Unknown face names
    @test getface([face"red", :nonexistent]) == getface(face"red")
    @test withfaces(face"red" => :nonexistent) do
        getface(face"red")
    end == getface()
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
    # An integer height scaled past `Int32` is clamped, and stays an integer
    @test merge(Face(height=typemax(Int32)), Face(height=2.0)).height === typemax(Int32)
    @test merge(Face(height=2^30), Face(height=3.0)).height === typemax(Int32)
    # Merge algebra: weak nothing is the identity, strong nothing an absorbing value
    let a = Face(weight=:bold, underline=true), b = Face(underline=face"red"), c = Face(underline=false)
        for f in (a, b, c, face"default")
            @test merge(f, f) == f
            @test merge(Face(), f) == f == merge(f, Face())
        end
        @test merge(merge(a, b), c) == merge(a, merge(b, c))
        @test merge(a, c).underline === nothing
        @test merge(face"default", c) == face"default"
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
    @test sprint(show, Face(underline=true)) |> pkgstrip == "Face(underline = true)"
    @test sprint(show, Face(underline=(nothing, :straight))) |> pkgstrip == "Face(underline = (nothing, :straight))"
    @test sprint(show, Face(underline=:curly)) |> pkgstrip == "Face(underline = :curly)"
    @test sprint(show, Face(underline=(nothing, :curly))) |> pkgstrip == "Face(underline = (nothing, :curly))"
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

# A palette declared out of dependency order, with a theme variant.
module TestPalette
    using StyledStrings
    @defpalette! begin
        topic = Face(foreground = sub, underline = (heading, :curly))
        sub = Face(inherit = heading, slant = :italic)
        heading = Face(weight = :bold)
        topic.dark = Face(foreground = heading)
    end
    __init__() = @registerpalette!
    const heading, sub, topic = face"heading", face"sub", face"topic"
end

# A variant referring to a sibling face that no base face depends on.
module TestPaletteVariant
    using StyledStrings
    @defpalette! begin
        spot = Face(weight = :bold)
        mark = Face()
        mark.dark = Face(foreground = spot)
    end
    __init__() = @registerpalette!
    const spot, mark = face"spot", face"mark"
end

module TestPaletteUser
    using StyledStrings
    using ..TestPalette
    @usepalettes! TestPalette
end

# Two palettes sharing a face name, one namespaced explicitly and referring across modules.
module TestPaletteA
    using StyledStrings
    @defpalette! begin
        shared = Face(weight = :bold)
        onlya = Face(slant = :italic)
    end
    __init__() = @registerpalette!
    const shared, onlya = face"shared", face"onlya"
end

module TestPaletteB
    using StyledStrings
    using ..TestPaletteA
    @defpalette! namespace = "custom" begin
        shared = Face(weight = :light)
        cross = Face(foreground = TestPaletteA.shared)
        std = Face(fg = red, bg = $(StyledStrings.SimpleColor(0x123456)), font = $(uppercase("mono")))
        chain = Face(inherit = [shared, cross])
        lazy = Face(inherit = zzz_undefined)
    end
    __init__() = @registerpalette!
    const shared, cross, std, chain, lazy = face"shared", face"cross", face"std", face"chain", face"lazy"
end

# Literal colours and unset attributes need no escaping.
module TestPaletteLiterals
    using StyledStrings
    @defpalette! begin
        hex = Face(foreground = 0xff0000, background = "#00ff00", underline = 0x0000ff)
        none = Face(foreground = nothing, underline = (nothing, :curly))
    end
    __init__() = @registerpalette!
    const hex, none = face"hex", face"none"
end

# A palette namespaced under another module.
module TestPaletteNamespaced
    using StyledStrings
    using ..TestPaletteA
    @defpalette! namespace = TestPaletteA begin
        nsface = Face()
    end
    __init__() = @registerpalette!
    const nsface = face"nsface"
end

# An importer whose own palette shadows an imported face.
module TestPaletteImporter
    using StyledStrings
    using ..TestPaletteA, ..TestPaletteB
    @defpalette! begin
        shared = Face(inverse = true)
    end
    @usepalettes! TestPaletteA TestPaletteB
    const own, qualified, imported = face"shared", face"TestPaletteA.shared", face"onlya"
end

@testset "Palettes" begin
    (; heading, sub, topic) = TestPalette
    @test sub.inherit == [heading]
    @test topic.foreground == SimpleColor(sub)
    @test topic.underline == (SimpleColor(heading), :curly)
    @test_throws StyledStrings.UnknownFaceError StyledStrings.lookupface(TestPalette, :headng)
    @test FACES.pool[Symbol(join(fullname(TestPalette), '_'), "_topic")] === topic
    @test FACES.themes.dark[topic] == Face(foreground = heading)
    @test FACES.themes.dark[TestPaletteVariant.mark].foreground.value === TestPaletteVariant.spot
    @test_throws ArgumentError macroexpand(TestPalette, :(@defpalette! begin x.dark = Face(weight = :bold) end))
    @test annotations(Core.eval(TestPalette, :(styled"{$(:heading):x}"))) == [(region = 1:1, label = :face, value = heading)]
    @test StyledStrings.facename(TestPaletteUser, heading) == :heading
    # A placeholder customised before use hands its customisation on to the registered face,
    # and once in use is displaced by it when interpolated
    setface!(StyledStrings.lookmakeface(:zzz_placeholder, false) => Face(font = "custom"))
    placeholder = StyledStrings.lookmakeface(:zzz_placeholder)
    registered = copy(Face())
    @lock FACES.lock StyledStrings.register_displace!(placeholder, registered, :zzz_placeholder)
    StyledStrings.relayer!(registered)
    @test getface(registered).font == "custom"
    @test FACES.displacements[placeholder] === registered
    @test only(annotations(styled"{$placeholder:x}")).value === registered
    @test !haskey(FACES.unregistered, :zzz_placeholder)
    resetfaces!(registered)
    # A recolouring of a placeholder moves with it
    recoloured = StyledStrings.lookmakeface(:zzz_displaced_recolour, false)
    FACES.recolors[recoloured] = Face(font = "recoloured")
    fresh = copy(Face())
    @lock FACES.lock StyledStrings.register_displace!(recoloured, fresh, :zzz_displaced_recolour)
    @test FACES.recolors[fresh].font == "recoloured" && !haskey(FACES.recolors, recoloured)
    delete!(FACES.recolors, fresh)
    @testset "Declaration errors" begin
        declerror(decl) = macroexpand(TestPalette, :(@defpalette! $decl))
        @test_throws r"Cyclic face dependencies" declerror(:(begin a = Face(inherit = b); b = Face(foreground = a) end))
        @test_throws r"theme must be light or dark" declerror(:(begin a = Face(); a.blue = Face() end))
        @test_throws r"Duplicate" declerror(:(begin a = Face(); a = Face() end))
        @test_throws r"cannot refer to it" declerror(:(begin a = Face(); a.dark = Face(inherit = a) end))
        @test_throws r"without a base variant" declerror(:(begin a.dark = Face() end))
        @test_throws r"must be a `Face\(...\)` expression" declerror(:(begin a = 1 end))
        @test_throws r"must be a face name" declerror(:(begin a = Face(foreground = :red) end))
        @test_throws r"must be a face name or a vector" declerror(:(begin a = Face(inherit = "b") end))
        @test_throws r"namespace must be" macroexpand(TestPalette, :(@defpalette! namespace = 1 begin a = Face() end))
    end
    @testset "References" begin
        (; shared, cross, std, chain, lazy) = TestPaletteB
        @test cross.foreground == SimpleColor(TestPaletteA.shared)
        @test std.foreground == SimpleColor(face"red")
        @test std.background == SimpleColor(0x123456)
        @test std.font == "MONO"
        @test chain.inherit == [shared, cross]
        # A reference to an unknown face is a lazily interned placeholder
        @test only(lazy.inherit) === FACES.unregistered[:zzz_undefined]
        @test getface(lazy) == getface(Face())
        (; hex, none) = TestPaletteLiterals
        @test hex.foreground == SimpleColor(0xff0000)
        @test hex.background == SimpleColor(0x00ff00)
        @test hex.underline == (SimpleColor(0x0000ff), :straight)
        @test none.foreground === nothing
        @test none.underline == (nothing, :curly)
    end
    @testset "Namespaces" begin
        @test FACES.pool[:custom_shared] === TestPaletteB.shared
        @test FACES.pool[Symbol(join(fullname(TestPaletteA), '_'), "_nsface")] === TestPaletteNamespaced.nsface
    end
    @testset "Imports" begin
        (; own, qualified, imported) = TestPaletteImporter
        @test own.inverse === true # The module's own palette shadows the imported face
        @test qualified === TestPaletteA.shared
        @test imported === TestPaletteA.onlya
        @test StyledStrings.facename(TestPaletteImporter, imported) == :onlya
        unknown = sprint(showerror, StyledStrings.UnknownFaceError(TestPaletteImporter, :nope))
        @test occursin("shared", unknown) && occursin("TestPaletteA", unknown) && occursin("TestPaletteB", unknown)
        @test occursin("No faces are defined", sprint(showerror, StyledStrings.UnknownFaceError(Main, :nope)))
        # Only modules and named palettes with a palette can be used
        @test_throws r"needs at least one module" macroexpand(TestPaletteUser, :(@usepalettes!))
        @test_throws r"has no palette to use" @eval module TestUseNoPalette
            using StyledStrings
            @usepalettes! Base
        end
        @test (@eval module TestUseStandard
            using StyledStrings
            @usepalettes! StyledStrings
            const region = face"region"
        end).region === face"region"
    end
    @testset "Registration" begin
        @test_logs (:warn, r"without a corresponding palette") @eval module TestNoPalette
            using StyledStrings
            @registerpalette!
        end
        # Re-evaluating a palette module carries customisations over to the new faces
        reeval = :(module TestPaletteReeval
            using StyledStrings
            @defpalette! begin r = Face(weight = :bold) end
            @registerpalette!
            const r = face"r"
        end)
        Core.eval(@__MODULE__, reeval)
        old = @eval TestPaletteReeval.r
        setface!(old => Face(font = "kept"))
        Core.eval(@__MODULE__, reeval)
        new = @eval TestPaletteReeval.r
        @test new !== old
        @test FACES.pool[Symbol(join(fullname(@eval TestPaletteReeval), '_'), "_r")] === new
        @test getface(new).font == "kept"
        @test !haskey(FACES.names, old)
        resetfaces!(new)
        # Evaluating a changed palette again, as Revise does, registers it without `__init__`
        @eval module TestPaletteRevised
            using StyledStrings
            @defpalette! begin
                r = Face(weight = :bold)
                r.dark = Face(font = "dark")
            end
            __init__() = @registerpalette!
        end
        old = @eval TestPaletteRevised.var"##styledstrings-defpalette-variable#".base.r
        setface!(old => Face(font = "kept"))
        Core.eval(@eval(TestPaletteRevised), :(@defpalette! begin r = Face(weight = :light) end))
        new = @eval TestPaletteRevised.var"##styledstrings-defpalette-variable#".base.r
        @test FACES.pool[Symbol(join(fullname(@eval TestPaletteRevised), '_'), "_r")] === new
        @test getface(new).weight == :light
        @test getface(new).font == "kept"
        @test !haskey(FACES.themes.dark, new) # The variant it no longer declares is gone
        resetfaces!(new)
        # A module with only named palettes registers them by name
        @test_logs @eval module TestNamedPalettes
            using StyledStrings
            @defpalette! extra begin
                thing = Face(font = "extra")
                thing.dark = Face(font = "dark")
            end
            @registerpalette! extra
        end
        named = (@eval TestNamedPalettes.extra).var"##styledstrings-defpalette-variable#".base.thing
        @test FACES.pool[Symbol(join(fullname(@eval TestNamedPalettes), '_'), "_extra_thing")] === named
        @test FACES.themes.dark[named].font == "dark"
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
    @test FACES.names[annotations(styled("{(fg=nocolour):x}"))[1].value.foreground.value] == :nocolour
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
    # Same-start nesting keeps order; only an identical directly-enclosing annotation is reused
    @test styled"{underline:{(underline=false):{underline:x}}}" ==
        AnnotatedString("x", [(1:1, :face, face"underline"), (1:1, :face, Face(underline=false)), (1:1, :face, face"underline")])
    @test styled"{red:{red:x}}" == AnnotatedString("x", [(1:1, :face, face"red")])
    @test astmatch(:(let ; AnnotatedString("val", _[]) end), @macroexpand styled"val")
    # Interpolation
    @test astmatch(
        :(let ;
              _!val_str = String(string(val))
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
              _!val_str = String(string(val))
              _!offset_val = ncodeunits(_!val_str)
              _!annots = _[(; region = 1:0 + _!offset_val, label = :face, value = $(face"red"))]
              _...
              AnnotatedString(_!val_str, _!interp_annots)
          end),
        @macroexpand styled"{red:$val}")
    @test astmatch(
        :(let ;
              _f = lookmakeface(_, :nonexistent_face)
              AnnotatedString("x", _[(; region = 1:1, label = :face, value = _f)])
          end),
        @macroexpand styled"{nonexistent_face:x}")
    @test astmatch(
        :(let ;
              _f = interpface(face, _, false)
              AnnotatedString("val", _[(; region = 1:3, label = :face, value = _f)])
          end),
        @macroexpand styled"{$face:val}")
    @test astmatch(
        :(let ;
              _f1 = interpface(f1, _, false)
              _f2 = interpface(f2, _, false)
              AnnotatedString("v1v2", _[(; region = 1:2, label = :face, value = _f1), (; region = 3:4, label = :face, value = _f2)])
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
    # Annotations following an annotated interpolation are kept
    annotated = styled"{red:x}"
    @test styled"$annotated{bold:b}" ==
        AnnotatedString("xb", [(1:1, :face, face"red"), (2:2, :face, face"bold")])
    @test styled"{bold:a}$annotated{italic:c}{underline:d}" ==
        AnnotatedString("axcd", [(1:1, :face, face"bold"), (2:2, :face, face"red"),
                                 (3:3, :face, face"italic"), (4:4, :face, face"underline")])
    other = styled"{blue:x}"
    @test styled"{bold:a $annotated}$other{italic:c}" ==
        AnnotatedString("a xxc", [(1:3, :face, face"bold"), (3:3, :face, face"red"),
                                  (4:4, :face, face"blue"), (5:5, :face, face"italic")])
    @test styled"$annotated" == annotated
    # Repeated interpolations
    @test styled"{bold:a $annotated}$annotated{italic:c}" ==
        AnnotatedString("a xxc", [(1:3, :face, face"bold"), (3:3, :face, face"red"),
                                  (4:4, :face, face"red"), (5:5, :face, face"italic")])
    plain = "x"
    @test styled"{bold:a $plain}$plain{italic:c}" ==
        AnnotatedString("a xxc", [(1:3, :face, face"bold"), (5:5, :face, face"italic")])
    # Interpolated variables cannot clash with the macro's own names for them
    lineof(offset, name) = styled"line $offset: $name"
    @test lineof(styled"{red:1}", styled"{bold:x}") ==
        AnnotatedString("line 1: x", [(6:6, :face, face"red"), (9:9, :face, face"bold")])
    pairof(annot, str) = styled"{bold:$annot} = $str"
    @test pairof(styled"{red:a}", styled"{blue:b}") ==
        AnnotatedString("a = b", [(1:1, :face, face"bold"), (1:1, :face, face"red"), (5:5, :face, face"blue")])
    # The generated code uses its own operators, not those of the module it is expanded in
    let mod = Module()
        Core.eval(mod, :(+(x, y) = nothing; annotated = $annotated))
        @test Core.eval(mod, Expr(:macrocall, GlobalRef(StyledStrings, Symbol("@styled_str")), nothing, "\$annotated y")) ==
            styled"$annotated y"
    end
    @test String(styled"αβ") == styled("αβ") == "αβ"
    # Any string can be styled, not only a `String`
    @test styled(SubString("x{bold:y}", 2)) == styled("{bold:y}")
    @test styled(strip("  {bold:y}  ")) == styled("{bold:y}")
    @test styled"{red:αβ}" == AnnotatedString("αβ", [(1:4, :face, face"red")])

    # Trailing (and non-trailing) Backslashes
    @test String(styled"\\") == "\\"
    @test String(styled"\\\\") == "\\\\"
    @test String(styled"\\\\\\") == "\\\\\\"
    @test String(styled".\\") == ".\\"
    @test String(styled".\\\\") == ".\\\\"
    @test String(styled".\\\\\\") == ".\\\\\\"
    # An escaped backslash is one literal backslash, not an escape for what follows
    bsval = "x"
    @test String(styled"\\$bsval") == "\\x"
    @test styled"\\{bold:x}" == AnnotatedString("\\x", [(2:2, :face, face"bold")])
    @test String(styled("a\\\\b")) == "a\\b"

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
    @test styled("{red:x}{note=n:y}") isa AnnotatedString{String, Union{Face, String}}
    # An unknown name is a placeholder that a later registration displaces, as in the macro
    @test annotations(styled("{zzz_fnface:x}"))[1].value === StyledStrings.lookmakeface(:zzz_fnface)
    # A name unknown at expansion time is looked up when the string is built
    lazyinline() = styled"{(fg=zzz_lazyinline):x}"
    @test annotations(lazyinline())[1].value.foreground.value === StyledStrings.lookmakeface(:zzz_lazyinline)
    lazyface = hacky_addface!(:zzz_lazyinline, Face(foreground=face"red"))
    @test annotations(lazyinline())[1].value.foreground.value === lazyface

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

struct CallableWriter end
(::CallableWriter)(io::IO, s) = print(io, s)

@testset "Annotation styles" begin
    AnnotationStyle, NoStyle = Base.AnnotatedDisplay.AnnotationStyle, Base.AnnotatedDisplay.NoStyle
    @test sprint(io -> Base.AnnotatedDisplay.awrite(CallableWriter(), StyledStrings.Styled(), io, styled"{red:a}"),
                 context = :color => true) == "\e[31ma\e[39m"
    @test AnnotationStyle(Face) === StyledStrings.Styled()
    @test AnnotationStyle(Union{Face, String}) === AnnotationStyle(Union{String, Int, Face}) === StyledStrings.Styled()
    @test AnnotationStyle(String) === NoStyle()
    red(V) = AnnotatedString{String, V}("x", [(1:1, :face, face"red"), (1:1, :n, 1)])
    @test sprint(print, red(Union{Face, Int}), context = :color => true) == "\e[31mx\e[39m"
    @test sprint(print, red(Any), context = :color => true) == "\e[31mx\e[39m"
    @test sprint(print, AnnotatedString{String, Int}("x", [(1:1, :n, 1)]), context = :color => true) == "x"
    # A styled char shows in HTML as a one-character string would
    @test sprint(show, MIME("text/html"), styled"{red:<}"[1]) == sprint(show, MIME("text/html"), styled"{red:<}")
    # A link whose value is not a string is skipped
    oddlink = AnnotatedString{String, Any}("x", [(1:1, :face, face"red"), (1:1, :link, 1)])
    @test sprint(print, oddlink, context = :color => true) == "\e[31mx\e[39m"
    @test !occursin("<a", sprint(show, MIME("text/html"), oddlink))
    # A link spanning several styled regions is one hyperlink
    @test sprint(print, styled"{link={https://x.org}:{bold:a}b} c", context = :color => true) ==
        "\e]8;;https://x.org\e\\\e[1ma\e[22mb\e]8;;\e\\ c"
    # Escaping is applied to each run of text as it is styled
    @test sprint(escape_string, styled"{red:a\nb}", context = :color => true) == "\e[31ma\\nb\e[39m"
end

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
    # A customised default face applies to unannotated text as to the rest
    setface!(face"default" => Face(foreground = face"green"))
    @test sprint(print, styled"plain", context = :color => true) == "\e[32mplain\e[39m"
    @test sprint(print, styled"plain {bold:x}", context = :color => true) == "\e[32mplain \e[1mx\e[39m\e[22m"
    resetfaces!(face"default")
    @test sprint(print, styled"plain", context = :color => true) == "plain"
    # Link formatting
    @test StyledStrings.uriformat("https://x.y/z w") == "https://x.y/z%20w"
    @test StyledStrings.uriformat("a:b") == "a:b"
    @test startswith(StyledStrings.uriformat("C:\\Users\\x"), "file://")
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
    @test sprint(StyledStrings.termcolor8bit, (r=0x40, g=0x63, b=0xd8), '3') == "\e[38;5;62m"
    @test sprint(StyledStrings.termcolor8bit, (r=0x38, g=0x98, b=0x26), '3') == "\e[38;5;64m"
    @test sprint(StyledStrings.termcolor8bit, (r=0x95, g=0x58, b=0xb2), '3') == "\e[38;5;97m"
    @test sprint(StyledStrings.termcolor8bit, (r=0xcb, g=0x3c, b=0x33), '3') == "\e[38;5;167m"
    @test sprint(StyledStrings.termcolor8bit, (r=0xee, g=0xee, b=0xee), '3') == "\e[38;5;255m"
    @test sprint(StyledStrings.termcolor8bit, (r=0x46, g=0x46, b=0xeb), '3') == "\e[38;5;63m" # 0x46 is nearer 95 than 0
    @test sprint(StyledStrings.termcolor8bit, (r=0xcb, g=0xc4, b=0xd2), '3') == "\e[38;5;251m"
    # 24-bit color
    @test sprint(StyledStrings.termcolor24bit, (r=0x40, g=0x63, b=0xd8), '3') == "\e[38;2;64;99;216m"
    @test sprint(StyledStrings.termcolor24bit, (r=0x38, g=0x98, b=0x26), '3') == "\e[38;2;56;152;38m"
    @test sprint(StyledStrings.termcolor24bit, (r=0x95, g=0x58, b=0xb2), '3') == "\e[38;2;149;88;178m"
    @test sprint(StyledStrings.termcolor24bit, (r=0xcb, g=0x3c, b=0x33), '3') == "\e[38;2;203;60;51m"
    # The color reset method
    @test sprint(StyledStrings.termcolor, nothing, '3') == "\e[39m"
    with_terminfo(vt100) do
        # A colour that chains to the default foreground, or cannot be resolved, resets
        chain = Face(foreground=Face(foreground=face"foreground"))
        @test sprint(print, styled"{red:a}{$chain:b}c", context = :color => true) == "\e[31ma\e[39mb\e[39mc"
        unresolvable = Face(foreground=Face())
        @test sprint(print, styled"{red:a}{$unresolvable:b}c", context = :color => true) == "\e[31ma\e[39mb\e[39mc"
        # A customised colour face is still its colour
        setface!(face"red" => Face(weight=:bold))
        @test sprint(print, styled"{(fg=red):x}", context = :color => true) == "\e[31mx\e[39m"
        resetfaces!(face"red")
    end
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
        @test ansi_change(foreground=(r=0x40, g=0x63, b=0xd8)) == ("\e[38;5;62m", "\e[39m")
        @test ansi_change(background=(r=0x40, g=0x63, b=0xd8)) == ("\e[48;5;62m", "\e[49m")
        # An explicit colour is kept, even when it matches the default's
        @test startswith(first(ansi_change(foreground=FACES.basecolors[face"foreground"])), "\e[38;5;")
        @test startswith(first(ansi_change(background=FACES.basecolors[face"background"])), "\e[48;5;")
        @test ansi_change(weight=:light) == ("", "\e[22m")
        @test ansi_change(slant=:italic) == ("\e[4m", "\e[24m")
        # The bytes written are counted in an appending buffer too
        pipe = PipeBuffer()
        @test write(IOContext(pipe, :color => true), styled"{red:hello}") == 15
        @test read(pipe, String) == "\e[31mhello\e[39m"
        # A char is written as a one-character string, against the same default face
        setface!(face"default" => Face(foreground = face"red"))
        c = Base.AnnotatedChar('x', [(label = :face, value = Face(foreground = face"red")), (label = :link, value = "https://x")])
        @test sprint(print, c, context = :color => true) ==
            sprint(print, AnnotatedString("x", [(1:1, :face, Face(foreground = face"red")), (1:1, :link, "https://x")]), context = :color => true)
        resetfaces!(face"default")
        # The underline that stands in for italics is kept
        @test sprint(StyledStrings.termstyle, getface(Face(slant=:italic)), getface(Face(underline=true))) == "\e[4m"
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
             a \e[4mfull-fledged\e[24m textual \e[38;5;147m\e[48;5;62m\e[1mstyling\e[39m\e[49m\e[22m \
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
    @test html_change(height=185) == "<span style=\"font-size: 18.5pt\">"
    @test html_change(font="Mono's \"x\"") == "<span style=\"font-family: 'Mono\\'s &quot;x&quot;'\">"
    @test html_change(underline=true) == "<span style=\"text-decoration: underline\">"
    @test html_change(underline=face"green") == "<span style=\"text-decoration: #25a268 underline\">"
    @test html_change(underline=:straight) == "<span style=\"text-decoration: underline\">"
    @test html_change(underline=:double) == "<span style=\"text-decoration: double underline\">"
    @test html_change(underline=:curly)  == "<span style=\"text-decoration: wavy underline\">"
    @test html_change(underline=(face"foreground", :curly)) == "<span style=\"text-decoration: wavy underline\">"
    @test html_change(underline=:dotted) == "<span style=\"text-decoration: dotted underline\">"
    @test html_change(underline=:dashed) == "<span style=\"text-decoration: dashed underline\">"
    @test html_change(underline=(face"cyan", :double)) == "<span style=\"text-decoration: #0097a7 double underline\">"
    @test html_change(strikethrough=true) == "<span style=\"text-decoration: line-through\">"
    @test html_change(underline=(face"red", :curly), strikethrough=true) ==
        "<span style=\"text-decoration: #a51c2c wavy underline line-through\">"
    @test sprint(StyledStrings.htmlstyle, getface(Face()), getface(Face(underline=true))) == "<span style=\"text-decoration: none\">"
    setface!(face"default" => Face(height=1.5))
    @test html_change(height=2.0) == "<span style=\"font-size: 200%\">"
    @test sprint(show, MIME("text/html"), styled"{(height=2.0):a}{bold:b}") ==
        "<span style=\"font-size: 200%\">a<span style=\"font-size: 50%; font-weight: 700\">b</span></span>"
    resetfaces!(face"default")
    # Text decorations cannot be removed within a nested span
    @test sprint(show, MIME("text/html"), styled"{underline:a}{bold:b}") ==
        "<span style=\"text-decoration: underline\">a</span><span style=\"font-weight: 700\">b</span>"
    # A link sits inside the styling of its region
    @test sprint(show, MIME("text/html"), styled"{red,link={https://x}:t}") ==
        "<span style=\"color: #a51c2c\"><a href=\"https://x\">t</a></span>"
    # The default face's colours are the page's own
    setface!(face"default" => Face(background=0x101010))
    @test sprint(show, MIME("text/html"), styled"{(bg=#101010):x}{(bg=#202020):y}") ==
        "x<span style=\"background-color: #202020\">y</span>"
    resetfaces!(face"default")
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
    @test printstyled(aio, "f", color=208) |> isnothing
    @test annotations(read(seekstart(aio), AnnotatedString))[end].value.foreground == SimpleColor(0xff8700)
    # A faulty faces.toml is reported once, and printing still works
    mktempdir() do depot
        mkpath(joinpath(depot, "config"))
        write(joinpath(depot, "config", "faces.toml"), "[[[")
        pushfirst!(DEPOT_PATH, depot)
        try
            @test_logs (:error, r"Could not load the face customisations") StyledStrings.load_customisations!(force = true)
            @test_logs StyledStrings.load_customisations!()
            @test sprint(print, styled"{red:x}", context = :color => true) == "\e[31mx\e[39m"
        finally
            popfirst!(DEPOT_PATH)
        end
    end
end

@testset "Recoloring" begin
    @testset "RGB" begin
        @test rgbcolor(face"red") == FACES.basecolors[face"red"]
        @test rgbcolor(SimpleColor(face"red")) == FACES.basecolors[face"red"]
        @test StyledStrings.finalcolor(face"red") === face"red"
        @test StyledStrings.finalcolor(Face(foreground=face"red")) === face"red"
        @test StyledStrings.finalcolor(Face()) === nothing
        indirect = hacky_addface!(:indirect, copy(Face()))
        another = hacky_addface!(:another, copy(Face()))
        final = hacky_addface!(:final, copy(Face()))
        @test withfaces(() -> rgbcolor(SimpleColor(indirect)),
                        [indirect => Face(foreground=another),
                         another => Face(foreground=final),
                         final => Face(foreground=face"red")]) == FACES.basecolors[face"red"]
        @test rgbcolor(indirect) == StyledStrings.UNRESOLVED_COLOR_FALLBACK
        # Customisations of inherited faces are followed
        setface!(face"emphasis" => Face(foreground=face"red"))
        @test StyledStrings.finalcolor(face"highlight") === face"red"
        @test rgbcolor(face"highlight") == FACES.basecolors[face"red"]
        resetfaces!(face"emphasis")
    end
    @testset "Blending" begin
        @test blend((r = 0x00, g = 0x00, b = 0xff) => 0.5, (r = 0xff, g=0xff, b=0x00) => 0.5) ==
            (r = 0x6b, g = 0xaa, b = 0xc6)
        @test blend(SimpleColor(0x0000ff) => 0.5, SimpleColor(0xffff00) => 0.5) ==
            SimpleColor(0x6baac6)
        @test blend(SimpleColor(0x000000) => 0.2, SimpleColor(0xffffff) => 0.6, SimpleColor(0x00ff00) => 0.2) ==
            SimpleColor(0x9fbe9c)
        @test blend(SimpleColor(0x123456)) == SimpleColor(0x123456)
        withfaces([face"blue" => Face(foreground=0x0000ff),
                   face"yellow" => Face(foreground=0xffff00)]) do
                       @test blend(face"blue" => 0.5, face"yellow" => 0.5) == SimpleColor(0x6baac6)
                   end
    end
    @testset "Theme change" begin
        hooks = copy(StyledStrings.recolor_hooks) # Restored at the end, so no hook outlives the testset
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
        @test counter[] == 1
        test_lightdark = hacky_addface!(:test_lightdark, Face(foreground=0x000001))
        hacky_addface!(:test_lightdark, Face(foreground=0x000002), :light)
        hacky_addface!(:test_lightdark, Face(foreground=0x000003), :dark)
        @test rgbcolor(SimpleColor(test_lightdark)).b == 0x01
        setcolors!(lightfbg)
        @test counter[] == 2
        @test rgbcolor(SimpleColor(test_lightdark)).b == 0x02
        setcolors!(darkfbg)
        @test counter[] == 3
        @test rgbcolor(SimpleColor(test_lightdark)).b == 0x03
        # Theme switch
        setcolors!(lightfbg)
        setface!(face"red" => Face(font="lightonly"), :light)
        @test getface(face"red").font == "lightonly"
        setcolors!(darkfbg)
        @test getface(face"red").font == "monospace"
        resetfaces!(face"red")
        # A reset keeps the variant of the current theme
        setcolors!(lightfbg)
        setface!(face"region" => Face(font="modified"))
        resetfaces!(face"region")
        @test getface(face"region").background == FACES.themes.light[face"region"].background
        setface!(face"region" => Face(font="modified"))
        resetfaces!()
        @test getface(face"region").background == FACES.themes.light[face"region"].background
        # Registering a palette refreshes a resolution cached before it
        @eval module TestPaletteLate
            using StyledStrings
            @defpalette! begin late = Face(weight = :bold); late.light = Face(slant = :italic) end
            const late = face"late"
        end
        setcolors!(lightfbg)
        @test getface(TestPaletteLate.late).slant == :normal
        Core.eval(TestPaletteLate, :(@registerpalette!))
        @test getface(TestPaletteLate.late).slant == :italic
        # Modifications and theme variants layer over the base face
        setface!(face"red" => Face(font="always"))
        setcolors!(darkfbg)
        @test getface(face"red").foreground.value === face"red"
        @test getface(face"red").font == "always"
        resetfaces!(face"red")
        test_variant = hacky_addface!(:test_variant, Face(font="base"))
        hacky_addface!(:test_variant, Face(foreground=0x000002), :light)
        setcolors!(lightfbg)
        @test getface(test_variant).font == "base"
        @test getface(test_variant).foreground.value.b == 0x02
        # Placeholder displacement
        placeholder = StyledStrings.lookmakeface(:zzz_displaced, false)
        setface!(placeholder => Face(font="lightmod"), :light)
        setcolors!(lightfbg)
        registered = Face(weight=:bold)
        @lock FACES.lock StyledStrings.register_displace!(placeholder, registered, :zzz_displaced)
        StyledStrings.relayer!(registered)
        @test getface(registered).font == "lightmod"
        resetfaces!(registered)
        # Resolved faces are cached, and a change to the current definitions is seen at once
        @test getface(face"red").font == "monospace"
        setface!(face"red" => Face(font = "changed"))
        @test getface(face"red").font == "changed"
        @test withfaces(() -> getface(face"red").font, face"red" => Face(font = "scoped")) == "scoped"
        @test getface(face"red").font == "changed"
        resetfaces!(face"red")
        @test getface(face"red").font == "monospace"
        # A variant registered before its base face follows it on displacement, at once
        setcolors!(darkfbg)
        StyledStrings.addface!(:zzz_early => Face(foreground=0x000004), :dark)
        early = Face(foreground=0x000005)
        StyledStrings.addface!(:zzz_early => early)
        push!(HACKY_FACES, :zzz_early)
        @test getface(early).foreground.value.b == 0x04
        # A customisation of a face not yet used applies from its first use
        StyledStrings.loaduserfaces!(Dict{String, Any}("zzz_configured" => Dict{String, Any}("font" => "configured")))
        @test getface(styled"{zzz_configured:x}", 1).font == "configured"
        # A palette registered after the last recolour has its variants applied at once
        @eval module ZzzLatePalette
            using StyledStrings
            @defpalette! begin
                late = Face(font = "base")
                late.dark = Face(font = "dark")
            end
            @registerpalette!
            const late = face"late"
        end
        @test getface(@eval ZzzLatePalette.late).font == "dark"
        recolor() do
            setface!(test_lightdark => Face(foreground=blend(:background => 0.6, :foreground => 0.3, :yellow => 0.1)))
        end
        setcolors!(lightfbg)
        @test getface(test_lightdark).foreground.value == (r = 0x9d, g = 0x99, b = 0x92)
        setcolors!(darkfbg)
        @test getface(test_lightdark).foreground.value == (r = 0x43, g = 0x40, b = 0x3a)
        # A recolor hook runs at once, beneath user customisations
        setface!(face"region" => Face(background = 0x112233))
        recolor(() -> setface!(face"region" => Face(background = 0x445566, font = "recoloured")))
        @test getface(face"region").background.value == (r = 0x11, g = 0x22, b = 0x33)
        @test getface(face"region").font == "recoloured"
        setcolors!(lightfbg)
        @test getface(face"region").background.value == (r = 0x11, g = 0x22, b = 0x33)
        resetfaces!()
        @test getface(face"region").background.value == (r = 0x44, g = 0x55, b = 0x66)
        # An unset attribute survives a later customisation of the same face
        setface!(face"emphasis" => Face(foreground = face"red"))
        setface!(face"emphasis" => convert(Face, Dict{String, Any}("foreground" => "inherit")))
        setface!(face"emphasis" => Face(font = "later"))
        @test getface(face"emphasis").foreground.value === face"foreground"
        StyledStrings.relayer!()
        @test getface(face"emphasis").foreground.value === face"foreground"
        # A hook's variant for another theme is not applied
        recolor(() -> setface!(face"region" => Face(font = "dark only"), :dark))
        @test FACES.current_theme[] === :light && getface(face"region").font != "dark only"
        pop!(StyledStrings.recolor_hooks)
        # A hook that fails at once is not registered
        nhooks = length(StyledStrings.recolor_hooks)
        @test_throws ErrorException recolor(() -> error("at once"))
        @test length(StyledStrings.recolor_hooks) == nhooks
        # A hook that fails later is logged, and the hooks after it still run
        failing = Ref(false)
        recolor(() -> if failing[] error("later") end)
        ran = Ref(0)
        recolor(() -> ran[] += 1)
        failing[] = true
        @test_logs (:error, "Recolor hook failed") setcolors!(darkfbg)
        @test ran[] == 2 && FACES.current_theme[] === :dark
        deleteat!(StyledStrings.recolor_hooks, nhooks+1:nhooks+2)
        # Customisations first loaded by a hook as it is registered are not taken for its recolours
        tomlface = hacky_addface!(:tomlface, copy(Face()))
        mktempdir() do depot
            mkpath(joinpath(depot, "config"))
            write(joinpath(depot, "config", "faces.toml"), "[tomlface]\nfont = \"customised\"\n")
            pushfirst!(DEPOT_PATH, depot)
            setglobal!(StyledStrings, :HAVE_LOADED_CUSTOMISATIONS, false)
            try
                recolor(() -> sprint(print, styled"{red:x}", context = :color => true))
                setcolors!(darkfbg)
                @test getface(tomlface).font == "customised"
            finally
                pop!(StyledStrings.recolor_hooks)
                popfirst!(DEPOT_PATH)
            end
        end
        copy!(StyledStrings.recolor_hooks, hooks)
        resetfaces!()
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

# Look-alikes for other copies of StyledStrings, whose `Face` is a distinct type: one with
# the pre-1.14 layout, and one loaded from our own sources.
module OtherCopy
    module StyledStrings
        struct SimpleColor
            value::Union{Symbol, NamedTuple}
        end
        Base.@kwdef struct Face
            font = nothing; height = nothing; weight = nothing; slant = nothing
            foreground = nothing; background = nothing; underline = nothing
            strikethrough = nothing; inverse = nothing; inherit = Symbol[]
        end
    end
end

module SourceCopy
    module StyledStrings
        using Base: AnnotatedString, AnnotatedChar, annotations, annotate!
        using Base.ScopedValues: ScopedValue, with, @with
        for file in ("faces.jl", "theme.jl", "palettes.jl")
            include(joinpath(@__DIR__, "..", "src", file))
        end
    end
end

# A whole other copy, with a `Styled` of its own, as with the REPL's private copy
module FullCopy
    include(joinpath(@__DIR__, "..", "src", "StyledStrings.jl"))
end

# A copy that routes its faces through ours, as a package with its own face-like type would.
Base.AnnotatedDisplay.AnnotationStyle(::Type{OtherCopy.StyledStrings.Face}) = StyledStrings.Styled()
Base.AnnotatedDisplay.AnnotationStyle(::Type{SourceCopy.StyledStrings.Face}) = StyledStrings.Styled()

@testset "Foreign faces" begin
    # The REPL's private copy of StyledStrings attaches faces of its own type (JuliaLang/julia#60034)
    render(f) = sprint(print, AnnotatedString("x", [(1:1, :face, f)]); context = :color => true)
    attrs = (font="mono", height=1.5, weight=:bold, strikethrough=false, inverse=true)
    @testset "Pre-1.14 layout" begin
        Other = OtherCopy.StyledStrings
        other = Other.Face(; attrs..., foreground=Other.SimpleColor(:red), background=Other.SimpleColor((r=0x01, g=0x02, b=0x03)),
                           underline=(Other.SimpleColor(:blue), :curly), inherit=[:emphasis, :bold])
        ours = Face(; attrs..., foreground=face"red", background=0x010203, underline=(face"blue", :curly),
                    inherit=[face"emphasis", face"bold"])
        @test StyledStrings._mergedface(other) == ours
        @test StyledStrings._mergedface(Other.Face()) == Face()
        @test StyledStrings._mergedface(Other.Face(underline=Other.SimpleColor(:green))) == Face(underline=face"green")
        @test StyledStrings._mergedface(Other.Face(underline=(nothing, :double))) == Face(underline=(nothing, :double))
        @test getface([face"emphasis", other]) == getface([face"emphasis", ours])
        @test render(other) == render(ours)
    end
    @testset "1.14 layout" begin
        Other = SourceCopy.StyledStrings
        other = Other.Face(; attrs..., foreground=Other.BASE_FACES.red, background=0x010203, underline=(Other.BASE_FACES.blue, :curly),
                           inherit=[Other.FACES.pool[:emphasis], Other.Face(slant=:italic)])
        ours = Face(; attrs..., foreground=face"red", background=0x010203, underline=(face"blue", :curly),
                    inherit=[face"emphasis", Face(slant=:italic)])
        got = StyledStrings._mergedface(other)
        @test got == ours
        @test got.f.foreground === face"red"
        @test got.f.inherit[1] === face"emphasis"
        @test StyledStrings._mergedface(Other.Face()) == Face()
        @test render(other) == render(ours)
        # Strong nothings, which only a field copy can carry over
        strong = Other.strongnothing
        sgot = StyledStrings._mergedface(Other.Face(Other.FaceDef(
            strong(String), strong(SimpleColor), strong(SimpleColor), strong(SimpleColor), strong(UInt32),
            strong(UInt8), strong(UInt8), strong(UInt8), strong(Bool), strong(Bool), Memory{Other.Face}()))).f
        @test all(f -> StyledStrings.isstrongnothing(getfield(sgot, f)), setdiff(fieldnames(StyledStrings.FaceDef), (:inherit,)))
    end
    @testset "Two whole copies" begin
        # Each copy has a `Styled` of its own, and strings holding faces of both display as one
        Full = FullCopy.StyledStrings
        theirs = Full.Face(weight = :bold, inverse = true)
        allours = AnnotatedString("ab", [(1:1, :face, face"red"), (2:2, :face, Face(weight = :bold, inverse = true))])
        mixed = AnnotatedString{String, Union{Face, Full.Face}}("ab", [(1:1, :face, face"red"), (2:2, :face, theirs)])
        @test sprint(print, mixed; context = :color => true) == sprint(print, allours; context = :color => true)
        @test sprint(show, MIME("text/html"), mixed) == sprint(show, MIME("text/html"), allours)
        @test Base.issingletontype(Base.infer_return_type(Base.AnnotatedDisplay.style, (typeof(mixed),)))
        # With an `Any` value type, whichever copy's face is met first
        for annots in ([(1:1, :face, face"red"), (2:2, :face, theirs)], [(2:2, :face, theirs), (1:1, :face, face"red")])
            @test sprint(print, AnnotatedString{String, Any}("ab", annots); context = :color => true) ==
                sprint(print, allours; context = :color => true)
        end
    end
    @test_throws MethodError getface([1])
    @testset "REPL mock load" begin
        # And with an actual second copy, loaded before ours the way the REPL's is. When the
        # package under test is the stdlib itself there is no second copy, and the script
        # reports so instead.
        script = """
            other = Base.require_stdlib(Base.PkgId(Base.UUID("f489334b-da3d-4c2e-b8f0-e476e12c162b"), "StyledStrings"))
            using StyledStrings
            other === StyledStrings && (print("same copy"); exit())
            face = other.Face(foreground = :red, weight = :bold)
            ours = StyledStrings.Face(foreground = face"red", weight = :bold)
            render(f) = sprint(print, Base.AnnotatedString("x", [(1:1, :face, f)]); context = :color => true)
            print(StyledStrings.getface([face]) == StyledStrings.getface([ours]), ",", render(face) == render(ours))
            """
        # Julia's own test suite runs this without an active project.
        project = if isnothing(Base.active_project()) `` else `--project=$(Base.active_project())` end
        cmd = `$(Base.julia_cmd()) --startup-file=no $project -e $script`
        out = read(pipeline(cmd; stderr), String)
        @test out in ("true,true", "same copy")
    end
end
