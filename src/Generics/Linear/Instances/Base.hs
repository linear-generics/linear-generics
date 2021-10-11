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
import qualified GHC.Generics as GHCG
import qualified Data.Functor.Sum as FSum
import qualified Data.Functor.Product as FProd
import Data.Functor.Compose
import Foreign.Ptr (Ptr)
import Generics.Linear.TH
import GHC.Tuple (Solo)

-- GHC.Tuple
$(deriveGenericAnd1 ''Solo)
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

-- Data.Functor.Compose
$(deriveGeneric1 ''Compose)

-- Data.Functor.Identity
$(deriveGenericAnd1 ''Identity)

-- Control.Category
$(deriveGenericAnd1 ''Kleisli)

-- Data.Ord
$(deriveGenericAnd1 ''Down)
$(deriveGeneric ''Ordering)

-- System.Exit
$(deriveGeneric ''ExitCode)

-- Data.Version
$(deriveGeneric ''Version)

-- GHC.Generics
$(deriveGenericAnd1 ''(GHCG.:+:))
$(deriveGenericAnd1 ''(GHCG.:*:))
$(deriveGenericAnd1 ''GHCG.K1)
$(deriveGenericAnd1 ''GHCG.M1)
$(deriveGenericAnd1 ''GHCG.Par1)
$(deriveGenericAnd1 ''GHCG.U1)
$(deriveGenericAnd1 ''GHCG.V1)
$(deriveGenericAnd1 ''(GHCG.:.:))
$(deriveGenericAnd1 ''GHCG.Rec1)
$(deriveGeneric ''GHCG.Fixity)
$(deriveGeneric ''GHCG.Associativity)
$(deriveGeneric ''GHCG.DecidedStrictness)
$(deriveGeneric ''GHCG.SourceStrictness)
$(deriveGeneric ''GHCG.SourceUnpackedness)

--   These ones use data constructor names
$(deriveGenericAnd1 'GHCG.UAddr)
$(deriveGenericAnd1 'GHCG.UChar)
$(deriveGenericAnd1 'GHCG.UDouble)
$(deriveGenericAnd1 'GHCG.UFloat)
$(deriveGenericAnd1 'GHCG.UInt)
$(deriveGenericAnd1 'GHCG.UWord)

-- Data.Complex
$(deriveGenericAnd1 ''Complex)

-- Data.Proxy
$(deriveGenericAnd1 ''Proxy)

-- Data.Semigroup
$(deriveGeneric ''S.All)
$(deriveGeneric ''S.Any)

$(deriveGenericAnd1 ''S.Min)
$(deriveGenericAnd1 ''S.Max)
$(deriveGenericAnd1 ''S.Arg)
$(deriveGenericAnd1 ''S.Dual)
$(deriveGeneric ''S.Endo)
$(deriveGenericAnd1 ''S.First)
$(deriveGenericAnd1 ''S.Last)
$(deriveGenericAnd1 ''S.Product)
$(deriveGenericAnd1 ''S.Sum)
$(deriveGenericAnd1 ''S.WrappedMonoid)

-- Data.Monoid
$(deriveGenericAnd1 ''M.Alt)
$(deriveGenericAnd1 ''M.Ap)
$(deriveGenericAnd1 ''M.First)
$(deriveGenericAnd1 ''M.Last)

-- Control.Applicative

$(deriveGenericAnd1 ''WrappedArrow)

$(deriveGenericAnd1 ''WrappedMonad)

$(deriveGenericAnd1 ''ZipList)

$(deriveGenericAnd1 ''Const)

-- Prelude
$(deriveGenericAnd1 ''[])
$(deriveGenericAnd1 ''Either)
$(deriveGenericAnd1 ''Maybe)
$(deriveGeneric ''Bool)
$(deriveGeneric ''())
$(deriveGeneric ''Char)
$(deriveGeneric ''Double)
$(deriveGeneric ''Int)
$(deriveGeneric ''Float)
$(deriveGeneric ''Word)

-- Data.List.NonEmpty
$(deriveGenericAnd1 ''NonEmpty)

-- Data.Functor.Sum
$(deriveGenericAnd1 ''FSum.Sum)

-- Data.Functor.Product
$(deriveGenericAnd1 ''FProd.Product)

-- Foreign.Ptr
$(deriveGeneric ''Ptr)
