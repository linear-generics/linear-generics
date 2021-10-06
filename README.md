## `linear-generics`: Generic programming library with linearity support
[![Hackage](https://img.shields.io/hackage/v/linear-generics.svg)][Hackage: linear-generics]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/linear-generics.svg)](http://packdeps.haskellers.com/reverse/linear-generics)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Build Status](https://github.com/dreixel/linear-generics/workflows/Haskell-CI/badge.svg)](https://github.com/dreixel/linear-generics/actions?query=workflow%3AHaskell-CI)

[Hackage: linear-generics]:
  http://hackage.haskell.org/package/linear-generics
  "linear-generics package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

This package offers a version of
[`GHC.Generics`](https://hackage.haskell.org/package/base/docs/GHC-Generics.html)
with two important improvements:

1. The `to`, `from`, `to1`, and `from1` methods have multiplicity-polymorphic
   types, allowing them to be used with either traditional Haskell code or
   linearly typed code.

2. The representations used for `Generic1` are modified slightly. As a result,
   `to1` and `from1` never need to use `fmap`. This can
   [greatly improve performance](https://gitlab.haskell.org/ghc/ghc/-/issues/15969),
   and it is necessary to support multiplicity polymorphism,
   [as discussed here](https://github.com/tweag/linear-base/pull/316).
   A smaller change, approximately
   [as proposed by spl](https://gitlab.haskell.org/ghc/ghc/-/issues/7492)
   reduces the number of instances that must be written to actually use `Generic1`
   instances.

   For more details, see the `Generics.Linear` documentation.

This library is organized as follows:

* `Generics.Linear` defines the core functionality for generics,
  including the multiplicity-polymorphic `Generic(1)` classes and
  a replacement for the `:.:` composition type.

* `Generics.Linear.TH` implements Template Haskell functionality for
  deriving instances of `Generic(1)`.

* `Generics.Linear.ViaGHCGenerics` offers `DerivingVia` targets to
  derive `Generic` and (some) `Generic1` instances from their
  `GHC.Generics` counterparts. Because these instances necessarily
  use unsafe coercions, their use will likely inhibit full optimization
  of code using them.

Educational code: the educational modules exported by
[`generic-deriving`](https://hackage.haskell.org/package/generic-deriving)
have been copied into the `tests/Generic/Deriving` directory
in this repository, with the very few modifications required to
accommodate the differences between the `Generic1` representations
here and in `base`. All the same caveats apply as in the originals;
see that package's `README`.
