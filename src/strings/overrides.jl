# This file is a part of Julia. License is MIT: https://julialang.org/license

# From io.jl
join(iterator) = _join_preserve_annotations(iterator)
join(iterator, delim) = _join_preserve_annotations(iterator, delim)
join(iterator, delim, last) = _join_preserve_annotations(iterator, delim, last)
