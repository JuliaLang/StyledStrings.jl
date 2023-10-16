# [StyledStrings](@id stdlib-styledstrings)

## [Styling](@id stdlib-styledstrings-styling)

When working with strings, formatting and styling often appear as a secondary
concern.

!!! note
    For instance, when printing to a terminal you might want to sprinkle [ANSI
    escape sequences](https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_(Select_Graphic_Rendition)_parameters)
    in the output, when outputting HTML styling constructs (`<span style="...">`,
    etc.) serve a similar purpose, and so on. It is possible to simply insert the
    raw styling constructs into the string next to the content itself, but it
    quickly becomes apparent that this is not well suited for anything but the most
    basic use-cases. Not all terminals support the same ANSI codes, the styling
    constructs need to be painstakingly removed when calculating the width of
    already-styled content, and that's before you even get into handling
    multiple output formats.

Instead of leaving this headache to be widely experienced downstream, it is
tackled head-on by the introduction of a special string type
([`AnnotatedString`](@ref Base.AnnotatedString)). This string type wraps any other
string type and allows for formating information to be applied to regions (e.g.
characters 1 through to 7 are bold and red).

Regions of a string are styled by applying [`Face`](@ref StyledStrings.Face)s to them —a
structure that holds styling information— (think "typeface"). As a
convenience, it is possible to name a face in the global faces dictionary
instead of giving the [`Face`](@ref StyledStrings.Face) directly.

Along with these capabilities, we also provide a convenient way for constructing
[`AnnotatedString`](@ref Base.AnnotatedString)s, detailed in [Styled String
Literals](@ref stdlib-styledstring-literals).

```julia-repl
julia> styled"{yellow:hello} {blue:there}"
"hello there" # prints with colour in the REPL
```

## Styling via [`AnnotatedString`](@ref Base.AnnotatedString)s

## [Faces](@id stdlib-styledstrings-faces)

### The `Face` type

A [`Face`](@ref StyledStrings.Face) specifies details of a typeface that text can be set in. It
covers a set of basic attributes that generalise well across different formats,
namely:

- `height`
- `weight`
- `slant`
- `foreground`
- `background`
- `underline`
- `strikethrough`
- `inverse`
- `inherit`

For details on the particular forms these attributes take, see the
[`Face`](@ref StyledStrings.Face) docstring, but of particular interest is `inherit` as it allows
you to _inherit_ attributes from other [`Face`](@ref StyledStrings.Face)s.

### The global faces dictionary

To make referring to particular styles more convenient, there is a global
`Dict{Symbol, Face}` that allows for [`Face`](@ref StyledStrings.Face)s to be
referred to simply by name. Packages can add faces to this dictionary via the
[`addface!`](@ref StyledStrings.addface!) function, and the loaded faces can be
easily [customised](@ref stdlib-styledstrings-face-toml).

!!! warning
    Any package registering new faces should ensure that they are prefixed
    by the package name, i.e. follow the format `mypackage_myface`.
    This is important for predictability, and to prevent name clashes.

There is one set of exemptions to the package-prefix rule, the set of basic
faces that are part of the default value of the faces dictionary.

#### [Basic faces](@id stdlib-styledstrings-basic-faces)

Basic faces are intended represent a general idea, that is widely applicable.

For setting some text with a certain attribute, we have the `bold`, `light`,
`italic`, `underline`, `strikethrough`, and `inverse` faces.

There are also named faces for the 16 terminal colours: `black`, `red`, `green`,
`yellow`, `blue`, `magenta`, `cyan`, `white`, `bright_black`/`grey`/`gray`,
`bright_red`, `bright_green`, `bright_blue`, `bright_magenta`, `bright_cyan`,
and `bright_white`.

For shadowed text (i.e. dim but there) there is the `shadow` face. To indicate a
selected region, there is the `region` face. Similarly for emphasis and
highlighting the `emphasis` and `highlight` faces are defined. There is also
`code` for code-like text.

For visually indicating the severity of messages the `error`, `warning`,
`success`, `info`, `note`, and `tip` faces are defined.

### [Customisation of faces (`Faces.toml`)](@id stdlib-styledstrings-face-toml)

It is good for the name faces in the global face dictionary to be customizable.
Theming and aesthetics are nice, and it is important for accessibility reasons
too. A TOML file can be parsed into a list of [`Face`](@ref StyledStrings.Face) specifications that
are merged with the pre-existing entry in the face dictionary.

A [`Face`](@ref StyledStrings.Face) is represented in TOML like so:

```toml
[facename]
attribute = "value"
...

[package.facename]
attribute = "value"
```

For example, if the `shadow` face is too hard to read it can be made brighter
like so:

```toml
[shadow]
foreground = "white"
```

### Applying faces to a `AnnotatedString`

By convention, the `:face` attributes of a [`AnnotatedString`](@ref
Base.AnnotatedString) hold information on the [`Face`](@ref StyledStrings.Face)s
that currently apply. This can be given in multiple forms, as a single `Symbol`
naming a [`Face`](@ref StyledStrings.Face)s in the global face dictionary, a
[`Face`](@ref StyledStrings.Face) itself, or a vector of either.

The `show(::IO, ::MIME"text/plain", ::AnnotatedString)` and `show(::IO,
::MIME"text/html", ::AnnotatedString)` methods both look at the `:face` attributes
and merge them all together when determining the overall styling.

We can supply `:face` attributes to a `AnnotatedString` during construction, add
them to the properties list afterwards, or use the convenient [Styled String
literals](@ref stdlib-styledstring-literals).

```jldoctest; setup = :(import Base.AnnotatedString; import StyledStrings: Face, @styled_str)
julia> str1 = AnnotatedString("blue text", [(1:9, :face => :blue)])
"blue text"

julia> str2 = styled"{blue:blue text}"
"blue text"

julia> str1 == str2
true

julia> sprint(print, str1, context = :color => true)
"\e[34mblue text\e[39m"

julia> sprint(show, MIME("text/html"), str1, context = :color => true)
"<pre><span style=\"color: #000080;\">blue text</span></pre>"
```

## [Styled String Literals](@id stdlib-styledstring-literals)

To ease construction of [`AnnotatedString`](@ref Base.AnnotatedString)s with [`Face`](@ref StyledStrings.Face)s applied,
the [`styled"..."`](@ref @styled_str) styled string literal allows for the content and
attributes to be easily expressed together via a custom grammar.

Within a [`styled"..."`](@ref @styled_str) literal, curly parenthesis are considered
special characters and must be escaped in normal usage (`\{`, `\}`). This allows
them to be used to express annotations with (nestable) `{annotations...:text}`
constructs.

The `annotations...` component is a comma-separated list of three types of annotations.
- Face names
- Inline `Face` expressions `(key=val,...)`
- `key=value` pairs

Interpolation is possible everywhere except for inline face keys.

For more information on the grammar, see the extended help of the
[`styled"..."`](@ref @styled_str) docstring.

## [API reference](@id stdlib-styledstrings-api)

```@docs
StyledStrings.@styled_str
StyledStrings.Face
StyledStrings.addface!
StyledStrings.SimpleColor
```
