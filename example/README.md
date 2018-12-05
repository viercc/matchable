# matchable-examples

This is (currently only one) example for the usage of `matchable` library.

In [Unification](src/Unification.hs), it implements an unification algorithm
generically using the `Matchable` type class and the `Free` monad.

In [MiniProlog](src/MiniProlog.hs), it implements very tiny logic programming DSL,
inspired by Prolog, using `Unification` module. The point is `Unification` module is
not tailored only for `MiniProlog`'s usage.

In [Main](src/Main.hs), it shows `MiniProlog` is working by examples.
