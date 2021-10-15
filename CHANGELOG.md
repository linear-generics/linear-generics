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
