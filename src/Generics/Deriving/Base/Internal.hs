{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Deriving.Base.Internal
  ( Generic (..)
  , Generic1 (..)
  , (:.:)(..)
  , toLinear
  , module GHCGenerics
  ) where
import GHC.Generics as GHCGenerics hiding (Generic (..), Generic1 (..), (:.:)(..))
import qualified GHC.Generics as G
import Data.Kind (Type)
import Unsafe.Coerce
import GHC.Exts (TYPE, RuntimeRep)
import GHC.Types (Multiplicity (..))

newtype (f :.: g) x = Comp1 { unComp1 :: f (g x) }

toLinear
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2) p.
     (a %p-> b) %1-> (a %1-> b)
toLinear = case unsafeEqualityProof @p @'One of
  UnsafeRefl -> \f -> f

class Generic a where
  type family Rep a :: Type -> Type
  type Rep a = G.Rep a

  -- | Setting @UseGHCGeneric@ to @'True@ activates default definitions
  -- based on @"GHC.Generics".'GHC.Generics.Generic'@. This is disabled
  -- by default because it's not entirely safe. In particular, a hand-written
  -- instance of @"GHC.Generics".'GHC.Generics.Generic'@ is not guaranteed
  -- to be linear.
  type family UseGHCGeneric a :: Bool
  type UseGHCGeneric a = 'False

  to :: Rep a p %1-> a
  default to :: (UseGHCGeneric a ~ 'True, G.Generic a, Rep a ~ G.Rep a)
             => Rep a p %1-> a
  to = toLinear G.to

  from :: a %1-> Rep a p
  default from :: (UseGHCGeneric a ~ 'True, G.Generic a, Rep a ~ G.Rep a)
             => a %1-> Rep a p
  from = toLinear G.from

class Generic1 f where
  type family Rep1 f :: k -> Type
  to1 :: Rep1 f p %1-> f p
  from1 :: f p %1-> Rep1 f p
