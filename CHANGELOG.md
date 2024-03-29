# next [????.??.??]

# 0.2.3
* Support building with `template-haskell-2.21.*` (GHC 9.8).

# 0.2.2
* Produce an orderly error message if someone gives us `type data`.
* Produce an error message much more eagerly when someone tries to
  use `GHCGenerically1` with an improperly shaped type.
* Place `INLINE [1]` pragmas on `from` and `to` implementations when types
  don't have too many constructors or fields, following the heuristics GHC
  has used for `Generic` deriving since version 9.2.

# 0.2.1
* Add a `Generic` instance for `Data.Void.Void`.

# 0.2
* The `Generic1` instance for `Generically1` no longer uses
  `GHC.Generics.Generic1`; it now uses `GHC.Generics.Generic` instead.  This
  allows far more instances to be derived.

# 0.1.0.1
* Make `Generic1` deriving properly polykinded. There was an old kind check
  that has not been valid for a long time.

* Improve error handling slightly.

* Adjust `README`.

* Refactor code in Generics.Linear.TH

* Improve comments.
# 0.1.0.0
* Initial fork from `generic-deriving`.
