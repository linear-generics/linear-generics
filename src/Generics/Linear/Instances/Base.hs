{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# language TemplateHaskell #-}
{-# language DataKinds #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Generics.Linear.Instances.Base (
  -- Instances only
  ) where

import Control.Applicative
import Data.Complex (Complex)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Semigroup as S
import qualified Data.Monoid as M
import Data.Proxy (Proxy)
import Data.Version (Version)
import System.Exit (ExitCode)
import Data.Ord (Down)
import Control.Arrow (Kleisli)
import Generics.Linear.Class
import qualified GHC.Generics as GHCG
import qualified Data.Functor.Sum as FSum
import qualified Data.Functor.Product as FProd
import Data.Functor.Compose
import Foreign.Ptr (Ptr)
import Generics.Linear.TH

$(deriveGeneric1 ''Compose)

$(deriveGenericAnd1 ''(,))
$(deriveGenericAnd1 ''(,,))
$(deriveGenericAnd1 ''(,,,))
$(deriveGenericAnd1 ''(,,,,))
$(deriveGenericAnd1 ''(,,,,,))
$(deriveGenericAnd1 ''(,,,,,,))
$(deriveGenericAnd1 ''(,,,,,,,))
$(deriveGenericAnd1 ''(,,,,,,,,))
$(deriveGenericAnd1 ''(,,,,,,,,,))
$(deriveGenericAnd1 ''(,,,,,,,,,,))
$(deriveGenericAnd1 ''(,,,,,,,,,,,))
$(deriveGenericAnd1 ''(,,,,,,,,,,,,))
$(deriveGenericAnd1 ''(,,,,,,,,,,,,,))
$(deriveGenericAnd1 ''(,,,,,,,,,,,,,,))

$(deriveGenericAnd1 ''Identity)
$(deriveGenericAnd1 ''Kleisli)

$(deriveGenericAnd1 ''Down)

-----

$(deriveGeneric ''ExitCode)
$(deriveGeneric ''Version)
$(deriveGenericAnd1 ''(:+:))
$(deriveGenericAnd1 ''(:*:))
$(deriveGenericAnd1 ''(:.:))
$(deriveGenericAnd1 ''K1)
$(deriveGenericAnd1 ''M1)
$(deriveGenericAnd1 ''Par1)
$(deriveGenericAnd1 ''U1)
$(deriveGenericAnd1 ''V1)

-- These ones use data constructor names
$(deriveGenericAnd1 'UAddr)
$(deriveGenericAnd1 'UChar)
$(deriveGenericAnd1 'UDouble)
$(deriveGenericAnd1 'UFloat)
$(deriveGenericAnd1 'UInt)
$(deriveGenericAnd1 'UWord)

$(deriveGenericAnd1 ''Complex)
$(deriveGenericAnd1 ''Proxy)

-----

--------------------------------------------------------------------------------
-- Representations for base types
--------------------------------------------------------------------------------


$(deriveGeneric ''S.All)
$(deriveGeneric ''S.Any)

$(deriveGenericAnd1 ''S.Min)
$(deriveGenericAnd1 ''S.Max)
$(deriveGenericAnd1 ''M.Alt)
$(deriveGenericAnd1 ''S.Arg)

-----

$(deriveGenericAnd1 ''(GHCG.:.:))
$(deriveGenericAnd1 ''GHCG.Rec1)

-----

$(deriveGeneric ''Fixity)
$(deriveGeneric ''Associativity)
$(deriveGeneric ''DecidedStrictness)
$(deriveGeneric ''SourceStrictness)
$(deriveGeneric ''SourceUnpackedness)

-----

$(deriveGenericAnd1 ''Const)

-----

$(deriveGenericAnd1 ''S.Dual)

$(deriveGeneric ''S.Endo)

$(deriveGenericAnd1 ''S.First)
$(deriveGenericAnd1 ''M.First)

-----

$(deriveGenericAnd1 ''S.Last)

$(deriveGenericAnd1 ''M.Last)

-----

$(deriveGenericAnd1 ''S.Product)

$(deriveGenericAnd1 ''S.Sum)

$(deriveGenericAnd1 ''S.WrappedMonoid)

-----

$(deriveGenericAnd1 ''WrappedArrow)

$(deriveGenericAnd1 ''WrappedMonad)

$(deriveGenericAnd1 ''ZipList)

-----



-----

$(deriveGenericAnd1 ''[])
$(deriveGenericAnd1 ''NonEmpty)
$(deriveGenericAnd1 ''Either)
$(deriveGenericAnd1 ''Maybe)

-----

$(deriveGenericAnd1 ''FSum.Sum)

$(deriveGenericAnd1 ''FProd.Product)

-----

$(deriveGeneric ''Bool)
$(deriveGeneric ''Ordering)
$(deriveGeneric ''())

$(deriveGeneric ''Ptr)
$(deriveGeneric ''Char)
$(deriveGeneric ''Double)
$(deriveGeneric ''Int)
$(deriveGeneric ''Float)
$(deriveGeneric ''Word)
