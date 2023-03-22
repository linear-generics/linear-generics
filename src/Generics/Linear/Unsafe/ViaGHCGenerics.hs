{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Unsafe #-}

-- | @DerivingVia@ targets to instantiate 'Generic' and 'Generic1',
-- both using @"GHC.Generics".'G.Generic'@.
--
-- === Caution

-- It is almost always better to use "Generics.Linear.TH" to derive instances
-- using Template Haskell. The instances derived using this module use unsafe
-- coercions, which tend to block up GHC's optimizer (see
-- <https://gitlab.haskell.org/ghc/ghc/-/wikis/linear-types/multiplicity-evidence this wiki page>
-- for more details on the inner workings of GHC). Use this
-- module only when the Template Haskell is unable to derive the instance
-- (rare) or you absolutely cannot use Template Haskell for some reason.

module Generics.Linear.Unsafe.ViaGHCGenerics
  ( GHCGenerically(..)
  , GHCGenerically1(..)
  ) where
import Data.Coerce (Coercible, coerce)
import Data.Kind (Constraint, Type)
import Data.Type.Bool (type (&&))
import Generics.Linear
import qualified GHC.Generics as G
import Unsafe.Coerce
import GHC.Exts (Any)
import GHC.TypeLits (TypeError, ErrorMessage (..))


-- | When @a@ is an instance
-- of @"GHC.Generics".'G.Generic'@, @GHCGenerically a@ is an instance
-- of 'Generic'.
--
-- === Warnings
--
-- @GHCGenerically@ is intended for use as a @DerivingVia@ target.
-- Most other uses of its 'Generic' instance will be quite wrong.
--
-- @GHCGenerically@ /must not/ be used with datatypes that have
-- nonlinear or linearity-polymorphic fields. Doing so will produce
-- completely bogus results, breaking the linearity rules.
--
-- @GHCGenerically@ is otherwise safe to use with /derived/
-- @"GHC.Generics".'G.Generic'@ instances, which are linear. If
-- you choose to use it with a hand-written instance, you should
-- check that the underlying instance is linear.
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

-- Stolen from linear-base, but without some polymorphism
-- we don't need here.

toLinear
  :: forall a b p q.
     (a %p-> b) %1-> (a %q-> b)
toLinear f = case unsafeEqualityProof @p @q of
  UnsafeRefl -> f

-- Stolen from profunctors

infixr 9 #.
(#.) :: Coercible b c => p b c -> (a -> b) -> a -> c
(#.) _ = coerce

infixl 8 .#
(.#) :: Coercible a b => (b -> c) -> p a b -> a -> c
f .# _ = coerce f

-- --------
-- Generic1
-- --------

-- | When @f a@ is an instance of @"GHC.Generics".'G.Generic'@
-- for all @a@, @GHCGenerically1 f@ is an instance of 'Generic1'.
--
-- === Warning
--
-- @GHCGenerically1@ is intended for use as a @DerivingVia@ target.
-- Most other uses of its 'Generic1' instance will be quite wrong.
--
-- @GHCGenerically1@ /must not/ be used with datatypes that have
-- nonlinear or linearity-polymorphic fields. Doing so will produce
-- completely bogus results, breaking the linearity rules.
--
-- @GHCGenerically1@ is otherwise safe to use with /derived/
-- @"GHC.Generics".'G.Generic'@ instances, which are linear. If
-- you choose to use it with a hand-written instance, you should
-- check that the underlying instance is linear.
--
-- === Example
--
-- @
-- data Foo a = Bar a (Either Int a) | Baz (Maybe a) Int
--   deriving stock (Show, "GHC.Generics".'G.Generic')
--   deriving 'Generic1' via GHCGenerically1 Foo
-- @
type GHCGenerically1 :: forall k. (k -> Type) -> k -> Type
newtype GHCGenerically1 f a = GHCGenerically1 { unGHCGenerically1 :: f a }

instance forall k (f :: k -> Type).
  ( forall (a :: k). G.Generic (f a)
  , CheckValid f
  ) => Generic1 (GHCGenerically1 f) where
  type Rep1 (GHCGenerically1 (f :: k -> Type)) = MakeRep1 @k (MarkedRep f)

  -- Why do we use unsafeCoerce here? While @G.Rep (f a)@ and @Rep1 f a@ are
  -- the same in memory, they're not (generally) Coercible. This largely has to
  -- do with the "modular" way GHC handles Coercible constraints for datatypes
  -- (as opposed to newtypes), including (:+:) and (:*:). Even if
  --
  --   f `Coercible` f' and g `Coercible` g',
  --
  -- we *don't* have
  --
  --   (f :+: g) `Coercible` (f' :+: g').
  --
  -- So we either need to use an unsafe coercion or completely tear down and
  -- rebuild the representation value.  Since we need an unsafe coercion to get
  -- linearity anyway, there's little to be gained by doing the latter.

  to1 :: forall a m. Rep1 (GHCGenerically1 f) a %m-> GHCGenerically1 f a
  to1 = unsafeCoerce (G.to :: G.Rep (f a) Any -> f a)

  from1 :: forall a m. GHCGenerically1 f a %m-> Rep1 (GHCGenerically1 f) a
  from1 = unsafeCoerce (G.from :: f a -> G.Rep (f a) Any)

-- This marking technique is inspired by Csongor Kiss's `GenericN`, part of his
-- generic-lens package family.

data LastMarkT = LastMark
data OtherMarkT = OtherMark
type family LastPar :: LastMarkT -> k
type family OtherPar :: OtherMarkT -> k -> k

-- | Check whether a type is suitable for a `Generic1` instance, using its
-- `GHC.Generics.Generic` representation. We used to just throw `TypeError`s in
-- the `Rep1` calculation. But that meant that type errors occurred only at use
-- sites. Indeed, the situation was even worse than that: a representation
-- could actually be constructed and (partially) explored when some of its
-- contents had `TypeError` types. Yuck.
type CheckValid :: forall k. (k -> Type) -> Constraint
type CheckValid (f :: k -> Type) = CheckValid' f (ValidRep1 (MarkedRep f))

type MarkedRep :: forall k. (k -> Type) -> Type -> Type
type MarkedRep (f :: k -> Type) = G.Rep ((MarkOtherPars f) (LastPar 'LastMark :: k))

type CheckValid' :: forall k. (k -> Type) -> Bool -> Constraint
type family CheckValid' f valid where
  CheckValid' _ 'True = ()
  CheckValid' f 'False = TypeError
    ('Text "Cannot create Generic1 instance for" ':$$:
    'ShowType f ':$$:
    'Text "the last parameter appears in an invalid location.")

type MakeRep1 :: forall k. (Type -> Type) -> k -> Type
type family MakeRep1 (rep :: Type -> Type) :: k -> Type where
  MakeRep1 (M1 i c f) = M1 i c (MakeRep1 f)
  MakeRep1 (x :+: y) = MakeRep1 x :+: MakeRep1 y
  MakeRep1 (x :*: y) = MakeRep1 x :*: MakeRep1 y
  MakeRep1 U1 = U1
  MakeRep1 V1 = V1
  MakeRep1 (Rec0 c) = MakeRep1Field (Rec0 (Unmark c)) Par1 c

-- This follows the structure of MakeRep1
type ValidRep1 :: (Type -> Type) -> Bool
type family ValidRep1 rep where
  ValidRep1 (M1 _ _ f) = ValidRep1 f
  ValidRep1 (x :+: y) = ValidRep1 x && ValidRep1 y
  ValidRep1 (x :*: y) = ValidRep1 x && ValidRep1 y
  ValidRep1 U1 = 'True
  ValidRep1 V1 = 'True
  ValidRep1 (Rec0 c) = ValidRep1Field c

type MarkOtherPars :: forall k. k -> k
type family MarkOtherPars (f :: k) :: k where
  MarkOtherPars ((f :: j -> k) (a :: j)) = MarkOtherPars f (OtherPar 'OtherMark a)
  MarkOtherPars f = f

type Unmark :: forall k. k -> k
type family Unmark f where
  -- We let the erroneous 'LastMark case get stuck; that's handled in the
  -- checker and we don't want redundant (and less informative) error
  -- messages.
  Unmark (_LastPar 'LastMark) = Stuck
  Unmark (_OtherPar 'OtherMark a) = a
  Unmark ((f :: j -> k) (a :: j)) = Unmark f (Unmark a)
  Unmark a = a

type Stuck :: forall k. k
type family Stuck where

-- Verify that the last parameter does not occur.  This follows the structure
-- of Unmark
type NoLast :: forall k. k -> Bool
type family NoLast f where
  NoLast (_LastPar 'LastMark) = 'False
  NoLast (_OtherPar 'OtherMark a) = 'True
  NoLast ((f :: j -> k) (a :: j)) = NoLast f && NoLast a
  NoLast _ = 'True

type MakeRep1Field :: forall j k. (k -> Type) -> (j -> Type) -> j -> k -> Type
type family MakeRep1Field fk acc c where
  -- Watch out! Order matters here. The third clause will match
  -- OtherPar _ as well, and that's no good.
  MakeRep1Field fk (acc :: k -> Type) (_LastPar 'LastMark :: k) = acc
  MakeRep1Field fk (_ :: b -> Type) (_OtherPar 'OtherMark _) = fk
  MakeRep1Field fk (acc :: b -> Type) ((f :: a -> b) (x :: a)) = MakeRep1Field fk (acc :.: Unmark f) x
  MakeRep1Field fk _ _ = fk

-- This follows the structure of MakeRep1Field
type ValidRep1Field :: forall k. k -> Bool
type family ValidRep1Field c where
  ValidRep1Field (_LastPar 'LastMark :: k) = 'True
  ValidRep1Field (_OtherPar 'OtherMark _) = 'True
  ValidRep1Field ((f :: a -> b) (x :: a)) = NoLast f && ValidRep1Field x
  ValidRep1Field _ = 'True
