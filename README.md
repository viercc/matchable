## matchable

This package provides a type class `Matchable`, which represents
`zipMatch` operation which can zip two values.

`zipMatch` operation can fail, and it returns zipped value wrapped
in `Maybe`. For example, if you try to zip two lists with different
length, `zipMatch` returns `Nothing`.
Specifically, `zipMatch` returns zipped value if and only if two arguments
have exactly same shape.

