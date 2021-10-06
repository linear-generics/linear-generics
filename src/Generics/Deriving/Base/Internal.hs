{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
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
  , GHCGenerically(..)
  , GHCGenerically1(..)
  , module GHCGenerics
  ) where
import GHC.Generics as GHCGenerics hiding (Generic (..), Generic1 (..), (:.:)(..), Rec1)
import qualified GHC.Generics as G
import Data.Kind (Constraint,Type)
import Unsafe.Coerce
import GHC.Exts (TYPE, RuntimeRep, Any)
import Data.Coerce (Coercible, coerce)
import GHC.TypeLits (TypeError, ErrorMessage (..))

newtype (f :.: g) x = Comp1 { unComp1 :: f (g x) }

toLinear
  :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
     (a :: TYPE r1) (b :: TYPE r2) p q.
     (a %p-> b) %1-> (a %q-> b)
toLinear = case unsafeEqualityProof @p @q of
  UnsafeRefl -> \f -> f

-- | @GHCGenerically@ is intended for use /only/ as a `DerivingVia`
-- target. Almost any other use will be wrong. When @a@ is an instance
-- of @"GHC.Generics".'G.Generic'@, @GHCGenerically a@ is an instance
-- of 'Generic'.
--
-- === Warning
--
-- @GHCGenerically1@ is intended for use as a 'DerivingVia' target.
-- Most other uses of its 'Generic1' instance will be quite wrong.
--
-- === Example
--
-- @
-- data Foo a = Bar a (Either Int a) | Baz (Maybe a) Int
--   deriving stock (Show, "GHC.Generics".'G.Generic')
--   deriving 'Generic' via GHCGenerically (Foo a)
-- @
newtype GHCGenerically a = GHCGenerically { unGHCGenerically :: a }

instance G.Generic a => Generic (GHCGenerically a) where
  type Rep (GHCGenerically a) = G.Rep a
  to = toLinear (GHCGenerically #. G.to)
  from = toLinear (G.from .# unGHCGenerically)

class Generic a where
  type family Rep a :: Type -> Type

  to :: Rep a p %1-> a
  from :: a %1-> Rep a p

class Generic1 f where
  type family Rep1 f :: k -> Type
  to1 :: Rep1 f p %1-> f p
  from1 :: f p %1-> Rep1 f p

-- | When @a@ is an instance of @"GHC.Generics".'G.Generic1'@, and its 'G.Rep1'
-- contains no compositions, @GHCGenerically1 a@ is an instance of 'Generic1'.
--
-- === Warning
--
-- @GHCGenerically1@ is intended for use as a 'DerivingVia' target.
-- Most other uses of its 'Generic1' instance will be quite wrong.
--
-- === Example
--
-- @
-- data Foo a = Bar a (Either Int a) | Baz (Maybe a) Int
--   deriving stock (Show, "GHC.Generics".'G.Generic1')
--   deriving 'Generic1' via GHCGenerically1 Foo
-- @
type GHCGenerically1 :: forall k. (k -> Type) -> k -> Type
newtype GHCGenerically1 f a = GHCGenerically1 { unGHCGenerically1 :: f a }

instance (G.Generic1 f, Repairable ('ShowType f) (G.Rep1 f)) => Generic1 (GHCGenerically1 f) where
  type Rep1 (GHCGenerically1 f) = Repair (G.Rep1 f)
  to1 :: forall a. Rep1 (GHCGenerically1 f) a %1-> GHCGenerically1 f a
  to1 = unsafeCoerce (GHCGenerically1 #. G.to1 @_ @f @a)

  from1 :: forall a. GHCGenerically1 f a %1-> Rep1 (GHCGenerically1 f) a
  from1 = unsafeCoerce (G.from1 @_ @f @a .# unGHCGenerically1)

-- | A @"GHC.Generics".'G.Rep1'@ is @Repairable@ if it contains no
-- compositions. The 'ErrorMessage' argument should be 'ShowType'
-- of the type whose representation this is.
type Repairable :: forall k. ErrorMessage -> (k -> Type) -> Constraint
class Repairable tn grep1 where
  -- | Convert a @"GHC.Generics".'G.Rep1'@ representation into a 'Rep1'
  -- representation.
  type Repair grep1 :: k -> Type
instance Repairable tn f => Repairable tn (M1 i c f) where
  type Repair (M1 i c f) = M1 i c (Repair f)
instance Repairable tn (G.Rec1 f) where
  type Repair (G.Rec1 f) = Par1 :.: f
instance Repairable tn (K1 i c) where
  type Repair (K1 i c) = K1 i c
instance Repairable tn Par1 where
  type Repair Par1 = Par1
instance (Repairable tn f, Repairable tn g) => Repairable tn (f :*: g) where
  type Repair (f :*: g) = Repair f :*: Repair g
instance (Repairable tn f, Repairable tn g) => Repairable tn (f :+: g) where
  type Repair (f :+: g) = Repair f :+: Repair g
instance Repairable tn U1 where
  type Repair U1 = U1
instance Repairable tn V1 where
  type Repair V1 = V1
instance Repairable tn (URec r) where
  type Repair (URec r) = URec r
instance
     TypeError ('Text "Could not derive linear Generic1 from GHC Generic1 for type" ':$$:
                tn ':<>: 'Text "." ':$$: 'Text "Its Rep1 instance includes composition.")
  => Repairable tn (f :.: g) where
  type Repair (_ :.: _) = Any

infixr 9 #.
(#.) :: Coercible b c => p b c -> (a -> b) -> a -> c
(#.) _ = coerce

infixl 8 .#
(.#) :: Coercible a b => (b -> c) -> p a b -> a -> c
f .# _ = coerce f
