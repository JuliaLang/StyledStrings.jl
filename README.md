# StyledStrings

<!-- [![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://JuliaLang.github.io/StyledStrings.jl/stable/) -->
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://JuliaLang.github.io/StyledStrings.jl/dev/)
[![Build Status](https://github.com/JuliaLang/StyledStrings.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/JuliaLang/StyledStrings.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage](https://codecov.io/gh/JuliaLang/StyledStrings.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/JuliaLang/StyledStrings.jl)

The `StyledStrings` package builds on top of the `AnnotatedString` type to
provide a full-fledged textual styling system, suitable for terminal and
graphical displays.

This is primarily intended for package authors, who may think of this as
`printstyled` on a (rather potent) steroids 😉. To get a better sense of what
this means in practice, start with the [examples
page](https://julialang.github.io/StyledStrings.jl/dev/examples/) of the
documentation and look for how other packages use `StyledStrings` in the Julia
ecosystem.

This is a standard library as of Julia 1.11. A version compatible with Julia 1.0
through to 1.10 has also been registered in General — allowing `StyledStrings`
to be used anywhere without compromising compatibility.
