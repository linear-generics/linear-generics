{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The definitions of 'Generic', 'Generic1', 'MP1', 'unMP1', and ':.:'.
-- Users should import 'Generics.Linear' instead.
module Generics.Linear.Class
  ( Generic (..)
  , Generic1 (..)
  , (:.:)(..)
  , MP1 (..)
  , unMP1
  , module GHCGenerics
  ) where
import Control.Applicative
import Data.Foldable (Foldable (..))
import Data.Functor.Classes
import Data.Functor.Contravariant
import GHC.Generics as GHCGenerics hiding (Generic (..), Generic1 (..), (:.:)(..), Rec1 (..))
import qualified GHC.Generics as G
import Control.Monad (MonadPlus (..))
import GHC.Types (Multiplicity (..))
import Data.Kind (Constraint, Type)
import Data.Data (Data)
import qualified Data.Data as D
import Data.Semigroup (Semigroup (..))
import Data.Typeable (Typeable, gcast1)

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

infixl 7 :.:
-- | The composition operator for types.
type (:.:) :: forall k2 k1. (k2 -> Type) -> (k1 -> k2) -> k1 -> Type
-- See Note: kind specificity
newtype (f :.: g) x = Comp1 { unComp1 :: f (g x) }
  deriving stock ( Eq, Ord, Show, Read
                 , G.Generic, G.Generic1, Data
                 , Functor, Foldable )
  deriving newtype (Semigroup, Monoid)

-- Note: kind specificity
--
-- I'd prefer to have the kinds inferred for @(:.:)@ and @MP@ rather than
-- specified. Unfortunately, I think it would be too confusing not to match the
-- types imported from GHC.Generics. See
-- https://gitlab.haskell.org/ghc/ghc/-/issues/20497

deriving stock instance (Traversable f, Traversable g) => Traversable (f :.: g)

deriving via forall (f :: Type -> Type) (g :: Type -> Type). f G.:.: g
  instance (Applicative f, Applicative g) => Applicative (f :.: g)

deriving via forall (f :: Type -> Type) (g :: Type -> Type). f G.:.: g
  instance (Alternative f, Applicative g) => Alternative (f :.: g)

deriving via forall (f :: Type -> Type) (g :: Type -> Type). f G.:.: g
  instance (Functor f, Contravariant g) => Contravariant (f :.: g)

instance (Eq1 f, Eq1 g) => Eq1 (f :.: g) where
  liftEq eq (Comp1 x) (Comp1 y) = liftEq (liftEq eq) x y

instance (Ord1 f, Ord1 g) => Ord1 (f :.: g) where
  liftCompare cmp (Comp1 x) (Comp1 y) = liftCompare (liftCompare cmp) x y

instance (Show1 f, Show1 g) => Show1 (f :.: g) where
  liftShowsPrec sp sl d (Comp1 x) =
      showsUnaryWith (liftShowsPrec sp' sl') "Comp1" d x
    where
      sp' = liftShowsPrec sp sl
      sl' = liftShowList sp sl

instance (Read1 f, Read1 g) => Read1 (f :.: g) where
  liftReadPrec rp rl = readData $
      readUnaryWith (liftReadPrec rp' rl') "Comp1" Comp1
    where
      rp' = liftReadPrec     rp rl
      rl' = liftReadListPrec rp rl

  liftReadListPrec = liftReadListPrecDefault
  liftReadList     = liftReadListDefault

-- | Types with nonlinear or multiplicity-polymorphic fields should use @MP1@
-- under @S1@. Unfortunately, Template Haskell (and GHC Generics) currently
-- lack any support for such types, so their instances must currently be
-- written entirely manually. We may add some functions to ease the pain at
-- some point.
--
-- Generic-deriving classes that do not involve linear types should treat
-- @MP1 m@ much as they treat @M1@: dig through it to get to the meat.
-- Unfortunately, some futzing about may be necessary to convince the
-- type checker that multiplicities work out.
--
-- Generic-deriving classes that use linear types may have to treat @MP1 m@
-- specially. In particular, they may need to constrain @m@ to be
-- ''GHC.Types.One' or ''GHC.Types.Many', or to match some other type
-- variable.
data MP1 :: forall k. Multiplicity -> (k -> Type) -> k -> Type where
-- See Note: kind specificity

-- If anything changes here (e.g., we add a field selector), then
-- the generic instances below will have to change.
  MP1 :: f a %m-> MP1 m f a

unMP1 :: MP1 m f a %n-> f a
-- Making this a field selector seems to break the type of the @MP1@
-- constructor. Whoops!
unMP1 (MP1 fa) = fa

deriving instance G.Generic (MP1 m f a)
deriving instance G.Generic1 (MP1 m f)
-- TODO: Give MP1 Generic and Generic1 instances!

instance Functor f => Functor (MP1 m f) where
  fmap f (MP1 fa) = MP1 (fmap f fa)
  x <$ MP1 fa = MP1 (x <$ fa)

instance Applicative f => Applicative (MP1 m f) where
  -- Why can't we use pure = MP1 Prelude.. pure ?
  pure a = MP1 (pure a)
  liftA2 f (MP1 x) (MP1 y) = MP1 (liftA2 f x y)

instance Monad f => Monad (MP1 m f) where
  MP1 fa >>= f = MP1 (fa >>= unMP1 Prelude.. f)

instance Foldable f => Foldable (MP1 m f) where
  foldMap f (MP1 x) = foldMap f x
  foldMap' f (MP1 x) = foldMap' f x
  fold (MP1 x) = fold x
  foldr c n (MP1 x) = foldr c n x
  foldr' c n (MP1 x) = foldr' c n x
  foldl c n (MP1 x) = foldl c n x
  foldl' c n (MP1 x) = foldl' c n x
  length (MP1 x) = length x
  null (MP1 x) = null x
  elem e (MP1 x) = elem e x
  maximum (MP1 x) = maximum x
  minimum (MP1 x) = minimum x
  sum (MP1 x) = sum x
  product (MP1 x) = product x

instance Traversable f => Traversable (MP1 m f) where
  traverse f (MP1 x) = wrapMP1 <$> traverse f x
  sequenceA (MP1 x) = wrapMP1 <$> sequenceA x
  mapM f (MP1 x) = wrapMP1 <$> mapM f x
  sequence (MP1 x) = wrapMP1 <$> sequence x

instance Contravariant f => Contravariant (MP1 m f) where
  contramap f (MP1 x) = MP1 (contramap f x)
  a >$ MP1 x = MP1 (a >$ x)

instance Alternative f => Alternative (MP1 m f) where
  empty = MP1 empty
  MP1 x <|> MP1 y = MP1 (x <|> y)
  many (MP1 x) = MP1 (many x)
  some (MP1 x) = MP1 (some x)

instance MonadPlus f => MonadPlus (MP1 m f) where
  mzero = MP1 mzero
  mplus (MP1 x) (MP1 y) = MP1 (mplus x y)

deriving instance Eq (f a) => Eq (MP1 m f a)
instance Ord (f a) => Ord (MP1 m f a) where
  compare (MP1 a) (MP1 b) = compare a b
  MP1 a < MP1 b = a < b
  MP1 a <= MP1 b = a <= b
  MP1 a > MP1 b = a > b
  MP1 a >= MP1 b = a >= b
  min (MP1 a) (MP1 b) = MP1 (min a b)
  max (MP1 a) (MP1 b) = MP1 (max a b)

deriving instance Read (f a) => Read (MP1 m f a)
deriving instance Show (f a) => Show (MP1 m f a)

instance Semigroup (f a) => Semigroup (MP1 m f a) where
  MP1 x <> MP1 y = MP1 (x <> y)
  stimes n (MP1 x) = MP1 (stimes n x)
  sconcat x = MP1 (sconcat (fmap unMP1 x))

instance Monoid (f a) => Monoid (MP1 m f a) where
  mempty = MP1 mempty

-- -------------------
-- Lifted instances

instance Eq1 f => Eq1 (MP1 m f) where
  liftEq eq (MP1 x) (MP1 y) = liftEq eq x y

instance Ord1 f => Ord1 (MP1 m f) where
  liftCompare cmp (MP1 x) (MP1 y) = liftCompare cmp x y

instance Show1 f => Show1 (MP1 m f) where
  liftShowsPrec sp sl d (MP1 a) = showsUnaryWith (liftShowsPrec sp sl) "MP1" d a

instance Read1 f => Read1 (MP1 m f) where
  liftReadPrec rp rl = readData $
    readUnaryWith (liftReadPrec rp rl) "MP1" (\x -> MP1 x)
  liftReadListPrec = liftReadListPrecDefault
  liftReadList     = liftReadListDefault

-- -------------------
-- Data instance

-- For some reason, the derived Data instance produces a multiplicity mismatch.
-- *sigh*.
instance (Typeable m, Typeable f, Data a, Data (f a)) => Data (MP1 m f a) where
  gfoldl f g (MP1 fa) = (g wrapMP1 `f` fa)
  gunfold f g _constr = f (g wrapMP1)
  toConstr (MP1 _) = mpConstr
  dataTypeOf _ = mpDataType
  dataCast1 mp = gcast1 mp

mpDataType :: D.DataType
mpDataType = D.mkDataType "MP1" [mpConstr]

mpConstr :: D.Constr
mpConstr = D.mkConstr mpDataType "MP1" [] D.Prefix

-- Why do we need this?
wrapMP1 :: f a -> MP1 m f a
wrapMP1 fa = MP1 fa

-- -----------------
-- Generic instances for MP1 (ugh)

-- | This type is used solely to get the name of this very module and the
-- package it's in, reliably. It must be in the same module as 'MP1'!

data ForInfo deriving G.Generic

-- | @CopyPkgModule f g@ copies the module and package name from
-- representation @f@ to representation @g@. Everything else is left
-- alone.
type family CopyPkgModule (f :: j -> Type) (g :: k -> Type) :: k -> Type where
  CopyPkgModule (D1 ('MetaData _ mod_name pkg_name _) _)
                  (D1 ('MetaData type_name _ _ is_newtype) r)
    = D1 ('MetaData type_name mod_name pkg_name is_newtype) r

instance Generic (MP1 m f a) where
  type Rep (MP1 m f a) = CopyPkgModule (G.Rep ForInfo)
    (D1
       ('MetaData "MP1" "" "" 'False)
       (C1
          ('MetaCons "MP1" 'PrefixI 'False)
          (S1
             ('MetaSel
                'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
             (MP1 m (Rec0 (f a))))))
  from y = from' y
    where
      from' :: MP1 m f a %1 -> Rep (MP1 m f a) x
      from' (MP1 x) = M1 (M1 (M1 (MP1 (K1 x))))
  to y = to' y
    where
      to' :: Rep (MP1 m f a) x %1 -> MP1 m f a
      to' (M1 (M1 (M1 (MP1 (K1 x))))) = MP1 x

instance Generic1 (MP1 m f) where
  type Rep1 (MP1 m f) = CopyPkgModule (G.Rep ForInfo)
    (D1
       ('MetaData "MP1" "" "" 'False)
       (C1
          ('MetaCons "MP1" 'PrefixI 'False)
          (S1
             ('MetaSel
                'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
             (MP1 m (Par1 :.: f)))))
  from1 y = from1' y
    where
      from1' :: MP1 m f a %1 -> Rep1 (MP1 m f) a
      from1' (MP1 x) = M1 (M1 (M1 (MP1 (Comp1 (Par1 x)))))
  to1 y = to1' y
    where
      to1' :: Rep1 (MP1 m f) a %1 -> MP1 m f a
      to1' (M1 (M1 (M1 (MP1 (Comp1 (Par1 x)))))) = MP1 x
