{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE Safe #-}

-- | A generic implementation of a 'Traversable'-like class.
-- See "Generics.Deriving.TraversableConf" for a more efficient,
-- but also more complicated, version.
module Generics.Deriving.Traversable (
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
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Semigroup as Semigroup (First, Last)
import           Data.Semigroup (Arg, Max, Min, WrappedMonoid)


--------------------------------------------------------------------------------
-- Generic traverse
--------------------------------------------------------------------------------

class GTraversable' t where
  gtraverse' :: Applicative f => (a -> f b) -> t a -> f (t b)

instance GTraversable' V1 where
  gtraverse' _ x = pure $ case x of

instance GTraversable' U1 where
  gtraverse' _ U1 = pure U1

instance GTraversable' Par1 where
  gtraverse' f (Par1 a) = Par1 <$> f a

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
gtraversedefault f x = to1 <$> gtraverse' f (from1 x)

-- Base types instances
instance GTraversable ((,) a)
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
instance GTraversable Proxy
instance GTraversable Monoid.Sum
instance (GTraversable f, GTraversable g) => GTraversable (Functor.Sum f g)
instance GTraversable WrappedMonoid
instance GTraversable ZipList
