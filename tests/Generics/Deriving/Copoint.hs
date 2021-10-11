{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DefaultSignatures #-}

{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

module Generics.Deriving.Copoint (
  -- * GCopoint class
    GCopoint(..)

  -- * Default method
  , gcopointdefault

  -- * Internal class
  , GCopoint'(..)

  ) where

import           Control.Applicative (WrappedMonad)

import           Data.Monoid (Dual)
import qualified Data.Monoid as Monoid (Sum)

import           Generics.Linear

import           Data.Ord (Down)

import           Data.Functor.Identity (Identity)
import           Data.Monoid (Alt)

import qualified Data.Functor.Sum as Functor (Sum)
import           Data.Semigroup (Arg, First, Last, Max, Min, WrappedMonoid)

--------------------------------------------------------------------------------
-- Generic copoint
--------------------------------------------------------------------------------

-- General copoint may return 'Nothing'

class GCopoint' t where
    gcopoint' :: (a -> b) -> t a -> Maybe b

instance GCopoint' V1 where
    gcopoint' _ _ = Nothing

instance GCopoint' U1 where
    gcopoint' _ U1 = Nothing

instance GCopoint' Par1 where
    gcopoint' f (Par1 a) = Just (f a)

instance GCopoint' (K1 i c) where
    gcopoint' _ _ = Nothing

instance GCopoint' f => GCopoint' (M1 i c f) where
    gcopoint' f (M1 a) = gcopoint' f a

instance GCopoint' f => GCopoint' (MP1 m f) where
    gcopoint' f (MP1 a) = gcopoint' f a

instance (GCopoint' f, GCopoint' g) => GCopoint' (f :+: g) where
    gcopoint' f (L1 a) = gcopoint' f a
    gcopoint' f (R1 a) = gcopoint' f a

-- Favours left "hole" for copoint
instance (GCopoint' f, GCopoint' g) => GCopoint' (f :*: g) where
    gcopoint' f (a :*: b) = case (gcopoint' f a) of
                             Just x -> Just x
                             Nothing -> gcopoint' f b

instance (GCopoint' f, GCopoint g) => GCopoint' (f :.: g) where
    gcopoint' f (Comp1 x) = gcopoint' (f . gcopoint) x

class GCopoint d where
  gcopoint :: d a -> a
  default gcopoint :: (Generic1 d, GCopoint' (Rep1 d))
                   => (d a -> a)
  gcopoint = gcopointdefault

gcopointdefault :: (Generic1 d, GCopoint' (Rep1 d))
                => d a -> a
gcopointdefault x = case (gcopoint' id . from1 $ x) of
                      Just x' -> x'
                      Nothing -> error "Data type is not copointed"

-- instance (Generic1 d, GCopoint' (Rep1 d)) => GCopoint d

-- Base types instances
instance GCopoint ((,) a)
instance GCopoint ((,,) a b)
instance GCopoint ((,,,) a b c)
instance GCopoint ((,,,,) a b c d)
instance GCopoint ((,,,,,) a b c d e)
instance GCopoint ((,,,,,,) a b c d e f)
instance GCopoint f => GCopoint (Alt f)
instance GCopoint (Arg a)
instance GCopoint Down
instance GCopoint Dual
instance GCopoint First
instance GCopoint Identity
instance GCopoint Last
instance GCopoint Max
instance GCopoint Min
instance (GCopoint f, GCopoint g) => GCopoint (Functor.Sum f g)
instance GCopoint Monoid.Sum
instance GCopoint m => GCopoint (WrappedMonad m)
instance GCopoint WrappedMonoid
