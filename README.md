## matchable

This package provides a type class `Matchable`, which represents
`zipMatch` operation which can zip two values.

`zipMatch` operation can fail, and it returns zipped value wrapped
in `Maybe`. Specifically, `zipMatch` returns zipped value if and only if two arguments
have exactly same shape.

### Example

``` haskell
>>> zipMatch [1,2] ['a','b']
Just [(1,'a'), (2,'b')]
>>> zipMatch [1,2,3] ['a','b']
Nothing
```

See [example](https://github.com/viercc/matchable/blob/master/example/README.md) also.
