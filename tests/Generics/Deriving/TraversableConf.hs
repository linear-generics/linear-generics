{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# language RankNTypes #-}

-- | A \"confusing\" default implementation of a 'Traversable'-like class that
-- produces code very much like derived instances. It uses the same magic
-- behind @Control.Lens.Traversal.confusing@.
module Generics.Deriving.TraversableConf (
  -- * Generic Traversable class
    GTraversable(..)

  -- * Default method
  , gtraversedefault

  -- * Internal Traversable class
  , GTraversable'(..)

  ) where

import           Control.Applicative (Const, WrappedMonad(..), ZipList)
import qualified Data.Monoid as Monoid (First, Last, Product, Sum)
import           Data.Monoid (Dual)
import           Generics.Linear
import           Generics.Deriving.Foldable
import           Generics.Deriving.Functor
import           Data.Complex (Complex)
import           Data.Ord (Down)
import           Data.Proxy (Proxy)
import           Data.Functor.Identity (Identity)
import qualified Data.Functor.Product as Functor (Product)
import qualified Data.Functor.Sum as Functor (Sum)
import           Data.Functor.Compose (Compose)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Semigroup as Semigroup (First, Last)
import           Data.Semigroup (Arg, Max, Min, WrappedMonoid)

--------------------------------------------------------------------------------
-- Generic traverse
--------------------------------------------------------------------------------

class GTraversable' t where
  gtraverse' :: Applicative f => (a -> f b) -> t a -> CY f (t b)

instance GTraversable' V1 where
  gtraverse' _ x = pure $ case x of

instance GTraversable' U1 where
  gtraverse' _ U1 = pure U1

instance GTraversable' Par1 where
  gtraverse' f (Par1 a) = Par1 <$> liftCY (f a)

instance GTraversable' (K1 i c) where
  gtraverse' _ (K1 a) = pure (K1 a)

instance GTraversable' f => GTraversable' (M1 i c f) where
  gtraverse' f (M1 a) = M1 <$> gtraverse' f a

instance GTraversable' f => GTraversable' (MP1 m f) where
  gtraverse' f (MP1 a) = (\x -> MP1 x) <$> gtraverse' f a

instance (GTraversable' f, GTraversable' g) => GTraversable' (f :+: g) where
  gtraverse' f (L1 a) = L1 <$> gtraverse' f a
  gtraverse' f (R1 a) = R1 <$> gtraverse' f a

instance (GTraversable' f, GTraversable' g) => GTraversable' (f :*: g) where
  gtraverse' f (a :*: b) = (:*:) <$> gtraverse' f a <*> gtraverse' f b

instance (GTraversable' f, GTraversable g) => GTraversable' (f :.: g) where
  gtraverse' f (Comp1 x) = Comp1 <$> gtraverse' (gtraverse f) x

instance GTraversable' UAddr where
  gtraverse' _ (UAddr a) = pure (UAddr a)

instance GTraversable' UChar where
  gtraverse' _ (UChar c) = pure (UChar c)

instance GTraversable' UDouble where
  gtraverse' _ (UDouble d) = pure (UDouble d)

instance GTraversable' UFloat where
  gtraverse' _ (UFloat f) = pure (UFloat f)

instance GTraversable' UInt where
  gtraverse' _ (UInt i) = pure (UInt i)

instance GTraversable' UWord where
  gtraverse' _ (UWord w) = pure (UWord w)

class (GFunctor t, GFoldable t) => GTraversable t where
  gtraverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  default gtraverse :: (Generic1 t, GTraversable' (Rep1 t), Applicative f)
                    => (a -> f b) -> t a -> f (t b)
  gtraverse = gtraversedefault

  gsequenceA :: Applicative f => t (f a) -> f (t a)
  gsequenceA = gtraverse id

  gmapM :: Monad m => (a -> m b) -> t a -> m (t b)
  gmapM f = unwrapMonad . gtraverse (WrapMonad . f)

  gsequence :: Monad m => t (m a) -> m (t a)
  gsequence = gmapM id

gtraversedefault :: (Generic1 t, GTraversable' (Rep1 t), Applicative f)
                 => (a -> f b) -> t a -> f (t b)
gtraversedefault f x = lowerCY $ to1 <$> gtraverse' f (from1 x)
{-# INLINE gtraversedefault #-}

-- Base types instances
instance GTraversable ((,) a)
instance GTraversable ((,,) a b)
instance GTraversable ((,,,) a b c)
instance GTraversable []
instance GTraversable (Arg a)
instance GTraversable Complex
instance GTraversable (Const m)
instance GTraversable Down
instance GTraversable Dual
instance GTraversable (Either a)
instance GTraversable Monoid.First
instance GTraversable (Semigroup.First)
instance GTraversable Identity
instance GTraversable Monoid.Last
instance GTraversable Semigroup.Last
instance GTraversable Max
instance GTraversable Maybe
instance GTraversable Min
instance GTraversable NonEmpty
instance GTraversable Monoid.Product
instance (GTraversable f, GTraversable g) => GTraversable (Functor.Product f g)
instance (GTraversable f, GTraversable g) => GTraversable (Compose f g)
instance GTraversable Proxy
instance GTraversable Monoid.Sum
instance (GTraversable f, GTraversable g) => GTraversable (Functor.Sum f g)
instance GTraversable WrappedMonoid
instance GTraversable ZipList

-- The types below are stolen from kan-extensions, and used in the same way as
-- Control.Lens.Traversal.confusing. Note that this is *not* equivalent to
-- applying `confusing` itself to a plain traversal: the latter seems to make a
-- mess with types like
--
--   data Gramp f a = Gramp Int a (f a)

newtype Curried g h a = Curried (forall r. g (a -> r) -> h r)

instance Functor g => Functor (Curried g h) where
  fmap f (Curried g) = Curried (g . fmap (.f))
  {-# INLINE fmap #-}

instance (Functor g, g ~ h) => Applicative (Curried g h) where
  pure a = Curried (fmap ($ a))
  {-# INLINE pure #-}
  Curried mf <*> Curried ma = Curried (ma . mf . fmap (.))
  {-# INLINE (<*>) #-}

lowerCurried :: Applicative f => Curried f g a -> g a
lowerCurried (Curried f) = f (pure id)
{-# INLINE lowerCurried #-}

newtype Yoneda f a = Yoneda { runYoneda :: forall b. (a -> b) -> f b }

lowerYoneda :: Yoneda f a -> f a
lowerYoneda (Yoneda f) = f id
{-# INLINE lowerYoneda #-}

instance Functor (Yoneda f) where
  fmap f m = Yoneda (\k -> runYoneda m (k . f))
  {-# INLINE fmap #-}

instance Applicative f => Applicative (Yoneda f) where
  pure a = Yoneda (\f -> pure (f a))
  {-# INLINE pure #-}
  Yoneda m <*> Yoneda n = Yoneda (\f -> m (f .) <*> n id)
  {-# INLINE (<*>) #-}

-- Lifted from the implementation of Control.Lens.Traversal.confusing
liftCurriedYoneda :: Applicative f => f a -> Curried (Yoneda f) (Yoneda f) a
liftCurriedYoneda fa = Curried (`yap` fa)
{-# INLINE liftCurriedYoneda #-}

yap :: Applicative f => Yoneda f (a -> b) -> f a -> Yoneda f b
yap (Yoneda k) fa = Yoneda (\ab_r -> k (ab_r .) <*> fa)
{-# INLINE yap #-}

-- This wrapper makes it easy to swap out implementations.
-- See, for example, https://github.com/glguy/generic-traverse,
-- which is essentially the same but uses a custom @Boggle@
-- type. I don't have a good sense of the tradeoffs between
-- the two.
newtype CY f a = CY { unCY :: Curried (Yoneda f) (Yoneda f) a }
  deriving newtype (Functor, Applicative)

liftCY :: Applicative f => f a -> CY f a
liftCY = CY . liftCurriedYoneda

lowerCY :: Applicative f => CY f a -> f a
lowerCY = lowerYoneda . lowerCurried . unCY
