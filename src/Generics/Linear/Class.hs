{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | The definitions of 'Generic', 'Generic1', and ':.:'.
-- Users should import 'Generics.Linear' instead.
module Generics.Linear.Class
  ( Generic (..)
  , Generic1 (..)
  , (:.:)(..)
  , module GHCGenerics
  ) where
import Control.Applicative
import Data.Functor.Contravariant
import GHC.Generics as GHCGenerics hiding (Generic (..), Generic1 (..), (:.:)(..), Rec1 (..))
import qualified GHC.Generics as G
import Data.Kind (Constraint, Type)
import Data.Data (Data)

-- | @Generic@ is exactly the same as @"GHC.Generics".'Generic'@
-- except that `to` and `from` are multiplicity polymorphic. This
-- means they will work equally well in traditional Haskell code
-- and in linearly typed code.
type Generic :: Type -> Constraint
class Generic a where
  type family Rep a :: Type -> Type

  to :: forall p m. Rep a p %m-> a
  from :: forall p m. a %m-> Rep a p

-- | @Generic1@ is similar to @"GHC.Generics".'Generic1'@, but has a few
-- differences.
--
-- == Multiplicity polymorphism
--
-- As with 'Generic', the @to1@ and @from1@ methods are
-- multiplicity polymorphic.
--
-- == Differences in 'Rep1' representation
--
-- === 'G.Rec1' is not used
--
-- Given a type like
--
-- @
-- newtype Foo a = Foo (Maybe a)
-- @
--
-- where a single type constructor (here @Maybe@) is applied to the
-- parameter, "GHC.Generics" represents the field as @'G.Rec1' Maybe@.
-- We instead represent it using @Par1 :.: Maybe@. It is expected
-- that very few real-life uses of "GHC.Generics" will break as a
-- result, and this simplification means that users don't have to
-- write 'G.Rec1' instances for their generic-deriving classes.
--
-- === Compositions associate in the opposite order
--
-- Given a type like
--
-- @
-- newtype Bar a = Bar (Maybe [Either e a])
-- @
--
-- where multiple type constructors are layered around the parameter,
-- "GHC.Generics@ represents the field as
--
-- @
-- Bar :.: (Maybe :.: Rec1 (Either e))
-- @
--
-- We instead represent it as
--
-- @
-- ((Par1 :.: Bar) :.: Maybe) :.: Either e
-- @
--
-- Doing it this way prevents `to1` and `from1` from having to 'fmap' newtype
-- constructors through the composed types, which can be a considerable
-- performance improvement and enables multiplicity polymorphism.
--
-- In most cases, modifying generic-deriving classes to accommodate this change
-- is simple: just swap which side of the composition is treated as a generic
-- representation and which as a base type. In a few cases, more restructuring
-- will be needed, which will require using different generic-deriving classes
-- than for "GHC.Generics".
--
-- == Difference in specificity
--
-- Users of type application will need to be aware that the kind parameter for
-- 'Generic1' is marked as inferred, whereas for @"GHC.Generics".'Generic1'@ it
-- is marked as specified. So you should use, for example, @to1 \@Maybe@ rather
-- than @to1 \@_ \@Maybe@.

type Generic1 :: forall {k}. (k -> Type) -> Constraint
class Generic1 (f :: k -> Type) where
  type family Rep1 f :: k -> Type

  to1 :: forall p m. Rep1 f p %m-> f p
  from1 :: forall p m. f p %m-> Rep1 f p

type (:.:) :: forall k2 k1. (k2 -> Type) -> (k1 -> k2) -> k1 -> Type
newtype (f :.: g) x = Comp1 { unComp1 :: f (g x) }
  deriving stock ( Eq, Ord, Show, Read
                 , G.Generic, G.Generic1, Data
                 , Functor, Foldable )
  deriving newtype (Semigroup, Monoid)

deriving stock instance (Traversable f, Traversable g) => Traversable (f :.: g)

deriving via forall (f :: Type -> Type) (g :: Type -> Type). f G.:.: g
  instance (Applicative f, Applicative g) => Applicative (f :.: g)

deriving via forall (f :: Type -> Type) (g :: Type -> Type). f G.:.: g
  instance (Alternative f, Applicative g) => Alternative (f :.: g)

deriving via forall (f :: Type -> Type) (g :: Type -> Type). f G.:.: g
  instance (Functor f, Contravariant g) => Contravariant (f :.: g)
