# Internals

Everything documented in this page is internal and subject to breaking changes,
even in minor version updates of Julia or StyledStrings.jl. If you are curious
about the internals, read on, but if you want to depend on them, please consider
opening a pull request or issue to discuss making them part of the public API.

```@docs
StyledStrings.ANSI_4BIT_COLORS
StyledStrings.FACES
StyledStrings.MAX_COLOR_FORWARDS
StyledStrings.UNRESOLVED_COLOR_FALLBACK
StyledStrings.Legacy.ANSI_256_COLORS
StyledStrings.Legacy.NAMED_COLORS
StyledStrings.Legacy.RENAMED_COLORS
StyledStrings.Legacy.legacy_color
StyledStrings.Legacy.load_env_colors!
StyledStrings.ansi_4bit
StyledStrings.setcolors!
StyledStrings.face!
StyledStrings.getface
StyledStrings.load_customisations!
StyledStrings.loadface!
StyledStrings.loaduserfaces!
StyledStrings.resetfaces!
StyledStrings.rgbcolor
StyledStrings.termcolor
StyledStrings.termcolor24bit
StyledStrings.termcolor8bit
```

## Styled Markup parsing

While some of the internals above are useful outside `StyledStrings`, and
unlikely to be broken (but with no guarantees!), apart from the exported string
macro and `styled` function, the details of `StyledMarkup` documented below
consists entirely of implementation details that should under no circumstances
be referenced outside of `StyledStrings` .

If you're curious about how exactly styled markup strings are parsed, they
should provide some insight though.

```@docs
StyledStrings.StyledMarkup
StyledStrings.StyledMarkup.State
StyledStrings.StyledMarkup.isnextchar
StyledStrings.StyledMarkup.ismacro
StyledStrings.StyledMarkup.styerr!
StyledStrings.StyledMarkup.hygienic_eval
StyledStrings.StyledMarkup.addpart!
StyledStrings.StyledMarkup.escaped!
StyledStrings.StyledMarkup.interpolated!
StyledStrings.StyledMarkup.readexpr!
StyledStrings.StyledMarkup.skipwhitespace!
StyledStrings.StyledMarkup.read_while!
StyledStrings.StyledMarkup.begin_style!
StyledStrings.StyledMarkup.end_style!
StyledStrings.StyledMarkup.read_annotation!
StyledStrings.StyledMarkup.read_inlineface!
StyledStrings.StyledMarkup.read_face_or_keyval!
StyledStrings.StyledMarkup.run_state_machine!
StyledStrings.StyledMarkup.annotatedstring_optimize!
```
