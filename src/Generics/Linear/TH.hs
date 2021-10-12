{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{- |
Module      :  Generics.Linear.TH
Copyright   :  (c) 2008--2009 Universiteit Utrecht
License     :  BSD3

Maintainer  :  David.Feuer@gmail.com
Stability   :  experimental
Portability :  non-portable

This module contains Template Haskell code that can be used to
automatically generate the boilerplate code for the generic deriving
library.

To use these functions, pass the name of a data type as an argument:

@
&#123;-&#35; LANGUAGE TemplateHaskell &#35;-&#125;

data Example a = Example Int Char a
$('deriveGeneric'     ''Example) -- Derives Generic instance
$('deriveGeneric1'     ''Example) -- Derives Generic1 instance
$('deriveGenericAnd1' ''Example) -- Derives Generic and Generic1 instances
@

This code can also be used with data families. To derive
for a data family instance, pass the name of one of the instance's constructors:

@
&#123;-&#35; LANGUAGE FlexibleInstances, TemplateHaskell, TypeFamilies &#35;-&#125;

data family Family a b
newtype instance Family Char x = FamilyChar Char
data    instance Family Bool x = FamilyTrue | FamilyFalse

$('deriveGeneric' 'FamilyChar) -- instance Generic (Family Char b) where ...
$('deriveGeneric1' 'FamilyTrue) -- instance Generic1 (Family Bool) where ...
-- Alternatively, one could type $(deriveGeneric1 'FamilyFalse)
@

=== General usage notes

Template Haskell imposes some fairly harsh limitations on ordering and
visibility within a module. In most cases, classes derived generically will
need to be derived using @StandaloneDeriving@ /after/ the @deriveGeneric*@
invocation. For example, if @Generically@ is a class that uses a 'Generic'
constraint for its instances, then you cannot write

@
data Fish = Fish
  deriving Show via (Generically Fish)

$(deriveGeneric 'Fish)
@

You must instead write

@
data Fish = Fish

$(deriveGeneric 'Fish)

deriving via Generically Fish
  instance Show Fish
@

Furthermore, types defined after a @deriveGeneric*@ invocation are not
visible before that invocation. This may require some careful ordering,
especially in the case of mutually recursive types. For example, the
following will not compile:

@
data Foo = Foo | Bar Baz
$(deriveGeneric 'Foo)

data Baz = Baz Int Foo
$(deriveGeneric 'Baz)
@

Instead, you must write

@
data Foo = Foo | Bar Baz
data Baz = Baz Int Foo

$(deriveGeneric 'Foo)
$(deriveGeneric 'Baz)
@
-}

-- Adapted from Generics.Regular.TH, via
-- Generics.Deriving.TH
module Generics.Linear.TH (
      deriveGeneric
    , deriveGeneric1
    , deriveGenericAnd1
  ) where

import           Control.Monad ((>=>), unless, when)

import           Generics.Linear.TH.Internal
import           Generics.Linear.TH.MetaData
import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH

-- Imports for splices
import           Generics.Linear.Class
  hiding ( uAddr#, uChar#, uDouble#, uFloat#, uInt#, uWord#
         , unM1, unK1, unPar1, unComp1)
import           Generics.Linear.TH.Insertions
  hiding ((.))
import qualified Generics.Linear.TH.Insertions as Ins
import           GHC.Exts (Addr#, Char#, Int#, Word#, Double#, Float#)

-- | Given the name of a type or data family constructor,
-- derive a 'Generic' instance.
deriveGeneric :: Name -> Q [Dec]
deriveGeneric = deriveGenericCommon True False

-- | Given the name of a type or data family constructor,
-- derive a 'Generic1' instance.
deriveGeneric1 :: Name -> Q [Dec]
deriveGeneric1 = deriveGenericCommon False True

-- | Given the name of a type or data family constructor,
-- derive a 'Generic' instance and a 'Generic1' instance.
deriveGenericAnd1 :: Name -> Q [Dec]
deriveGenericAnd1 = deriveGenericCommon True True

deriveGenericCommon :: Bool -> Bool -> Name -> Q [Dec]
deriveGenericCommon generic generic1 n = do
    b <- if generic
            then deriveInst Generic n
            else return []
    c <- if generic1
            then deriveInst Generic1 n
            else return []
    return (b ++ c)

deriveInst :: GenericClass -> Name -> Q [Dec]
deriveInst Generic  = deriveInstCommon ''Generic  ''Rep  Generic  'from  'to
deriveInst Generic1 = deriveInstCommon ''Generic1 ''Rep1 Generic1 'from1 'to1

deriveInstCommon :: Name
                 -> Name
                 -> GenericClass
                 -> Name
                 -> Name
                 -> Name
                 -> Q [Dec]
deriveInstCommon genericName repName gClass fromName toName n = do
  i <- reifyDataInfo n
  let (name, instTys, cons, dv) = either error id i
      gt = mkGenericTvbs gClass instTys
  (origTy, origKind) <- buildTypeInstance gClass name instTys
  tyInsRHS <- repType gt dv name cons

  let origSigTy = SigT origTy origKind
  tyIns <- tySynInstDCompat repName Nothing [return origSigTy] (return tyInsRHS)
  let
    mkBody maker = [clause [] (normalB $ lamCaseE [maker gt cons]) []]

    fcs = mkBody mkFrom
    tcs = mkBody mkTo

  fmap (:[]) $
    instanceD (cxt []) (conT genericName `appT` return origSigTy)
                         [return tyIns, funD fromName fcs, funD toName tcs]

repType :: GenericTvbs
        -> DatatypeVariant_
        -> Name
        -> [ConstructorInfo]
        -> Q Type
repType gt dv dt cs =
    conT ''D1 `appT` mkMetaDataType dv dt `appT`
      foldBal sum' (conT ''V1) (map (repCon gt dv dt) cs)
  where
    sum' :: Q Type -> Q Type -> Q Type
    sum' a b = conT ''(:+:) `appT` a `appT` b

repCon :: GenericTvbs
       -> DatatypeVariant_
       -> Name
       -> ConstructorInfo
       -> Q Type
repCon gt dv dt
  (ConstructorInfo { constructorName       = n
                   , constructorVars       = vars
                   , constructorContext    = ctxt
                   , constructorStrictness = bangs
                   , constructorFields     = ts
                   , constructorVariant    = cv
                   }) = do
  checkExistentialContext n vars ctxt
  let mbSelNames = case cv of
                     NormalConstructor          -> Nothing
                     InfixConstructor           -> Nothing
                     RecordConstructor selNames -> Just selNames
      isRecord   = case cv of
                     NormalConstructor   -> False
                     InfixConstructor    -> False
                     RecordConstructor _ -> True
      isInfix    = case cv of
                     NormalConstructor   -> False
                     InfixConstructor    -> True
                     RecordConstructor _ -> False
  ssis <- reifySelStrictInfo n bangs
  repConWith gt dv dt n mbSelNames ssis ts isRecord isInfix

repConWith :: GenericTvbs
           -> DatatypeVariant_
           -> Name
           -> Name
           -> Maybe [Name]
           -> [SelStrictInfo]
           -> [Type]
           -> Bool
           -> Bool
           -> Q Type
repConWith gt dv dt n mbSelNames ssis ts isRecord isInfix = do
    let structureType :: Q Type
        structureType = foldBal prodT (conT ''U1) f

        f :: [Q Type]
        f = case mbSelNames of
                 Just selNames -> zipWith3 (repField gt dv dt n . Just)
                                           selNames ssis ts
                 Nothing       -> zipWith  (repField gt dv dt n Nothing)
                                           ssis ts

    conT ''C1
      `appT` mkMetaConsType dv dt n isRecord isInfix
      `appT` structureType

prodT :: Q Type -> Q Type -> Q Type
prodT a b = conT ''(:*:) `appT` a `appT` b

repField :: GenericTvbs
         -> DatatypeVariant_
         -> Name
         -> Name
         -> Maybe Name
         -> SelStrictInfo
         -> Type
         -> Q Type
repField gt dv dt ns mbF ssi t =
           conT ''S1
    `appT` mkMetaSelType dv dt ns mbF ssi
    `appT` (repFieldArg gt =<< resolveTypeSynonyms t)

repFieldArg :: GenericTvbs -> Type -> Q Type
repFieldArg Gen0{} (dustOff -> t0) = boxT t0
repFieldArg (Gen1{gen1LastTvbName = name}) (dustOff -> t0) = go (conT ''Par1) t0
  where
    -- | Returns NoPar if the parameter doesn't appear.
    -- Expects its argument to have been dusted.
    go :: Q Type -> Type -> Q Type
    go _ ForallT{} = rankNError
    go _ ForallVisT{} = rankNError
    go macc (VarT t) | t == name = macc
    go macc (AppT f x) = do
      when (not (f `ground` name)) outOfPlaceTyVarError
      let
        macc' = do
          itf <- isUnsaturatedType f
          when itf typeFamilyApplicationError
          infixT macc ''(:.:) (pure f)
      go macc' (dustOff x)
    go _ _ = boxT t0

boxT :: Type -> Q Type
boxT ty = case unboxedRepNames ty of
    Just (boxTyName, _, _) -> conT boxTyName
    Nothing                -> conT ''Rec0 `appT` return ty

mkFrom :: GenericTvbs -> [ConstructorInfo] -> Q Match
mkFrom gt cs = do
    y <- newName "y"
    match (varP y)
          (normalB $ conE 'M1 `appE` tweakedCaseE (varE y) cases)
          []
  where
    cases = zipWith (fromCon gt id (length cs)) [1..] cs

mkTo :: GenericTvbs -> [ConstructorInfo] -> Q Match
mkTo gt cs = do
    y <- newName "y"
    match (conP 'M1 [varP y])
          (normalB $ tweakedCaseE (varE y) cases)
          []
  where
    cases = zipWith (toCon gt id (length cs)) [1..] cs

tweakedCaseE :: Quote m => m Exp -> [m Match] -> m Exp
#if __GLASGOW_HASKELL__ >= 901
tweakedCaseE = caseE
#else
-- In GHC 9.0.1, there was a bug in multiplicity checking of case expressions,
-- so we can't use those. Fortunately, lambda case was fine, so we just express
--
--   case scrut of
--     branches
--
-- as
--
--   (\case branches) scrut
tweakedCaseE scrut branches = lamCaseE branches `appE` scrut
#endif

fromCon :: GenericTvbs -> (Q Exp -> Q Exp) -> Int -> Int
        -> ConstructorInfo -> Q Match
fromCon gt wrap m i
  (ConstructorInfo { constructorName    = cn
                   , constructorVars    = vars
                   , constructorContext = ctxt
                   , constructorFields  = ts
                   }) = do
  checkExistentialContext cn vars ctxt
  fNames <- newNameList "f" $ length ts
  match (conP cn (map varP fNames))
        (normalB $ wrap $ lrE i m $ conE 'M1 `appE`
          foldBal prodE (conE 'U1) (zipWith (fromField gt) fNames ts)) []

prodE :: Q Exp -> Q Exp -> Q Exp
prodE x y = conE '(:*:) `appE` x `appE` y

fromField :: GenericTvbs -> Name -> Type -> Q Exp
fromField gt nr t = conE 'M1 `appE` (fromFieldWrap gt nr =<< resolveTypeSynonyms t)

fromFieldWrap :: GenericTvbs -> Name -> Type -> Q Exp
fromFieldWrap _                              _  ForallT{}  = rankNError
fromFieldWrap gt                             nr (SigT t _) = fromFieldWrap gt nr t
fromFieldWrap Gen0{}                         nr t          = conE (boxRepName t) `appE` varE nr
fromFieldWrap (Gen1{gen1LastTvbName = name}) nr t          = wC t name           `appE` varE nr

wC :: Type -> Name -> Q Exp
wC (dustOff -> t0) name = go (ConE 'Par1) t0
  where
    go :: Exp -> Type -> Q Exp
    go !_ ForallT{} = rankNError
    go _ ForallVisT{} = rankNError
    go acc (VarT t) | t == name = pure acc
    go acc (AppT _f x) =
      -- We needn't check f `ground` name here; that was checked in
      -- repFieldArg.
      let
        acc' =
          -- We needn't check for f being unsaturated; that was checked
          -- in repFieldArg.
          InfixE (Just (ConE 'Comp1)) (VarE '(Ins..)) (Just acc)
      in go acc' (dustOff x)
    go _ _ = conE (boxRepName t0)

boxRepName :: Type -> Name
boxRepName = maybe 'K1 snd3 . unboxedRepNames

toCon :: GenericTvbs -> (Q Pat -> Q Pat) -> Int -> Int
      -> ConstructorInfo -> Q Match
toCon gt wrap m i
  (ConstructorInfo { constructorName    = cn
                   , constructorVars    = vars
                   , constructorContext = ctxt
                   , constructorFields  = ts
                   }) = do
  checkExistentialContext cn vars ctxt
  fNames <- newNameList "f" $ length ts
  match (wrap $ lrP i m $ conP 'M1
          [foldBal prod (conP 'U1 []) (zipWith (toField gt) fNames ts)])
        (normalB $ foldl appE (conE cn)
                         (zipWith (\nr -> resolveTypeSynonyms >=> toConUnwC gt nr)
                           fNames ts)) []
  where prod x y = conP '(:*:) [x,y]

toConUnwC :: GenericTvbs -> Name -> Type -> Q Exp
toConUnwC Gen0{}                         nr _ = varE nr
toConUnwC (Gen1{gen1LastTvbName = name}) nr t = unwC t name `appE` varE nr

toField :: GenericTvbs -> Name -> Type -> Q Pat
toField gt nr t = conP 'M1 [toFieldWrap gt nr t]

toFieldWrap :: GenericTvbs -> Name -> Type -> Q Pat
toFieldWrap Gen0{} nr t = conP (boxRepName t) [varP nr]
toFieldWrap Gen1{} nr _ = varP nr

unwC :: Type -> Name -> Q Exp
unwC (dustOff -> t0) name = go (VarE 'unPar1) t0
  where
    go :: Exp -> Type -> Q Exp
    go !_ ForallT{} = rankNError
    go _ ForallVisT{} = rankNError
    go acc (VarT t) | t == name = pure acc
    go acc (AppT _f x) =
      -- We needn't check f `ground` name here; that was checked in
      -- repFieldArg.
      let
        acc' =
          -- We needn't check for f being unsaturated; that was checked
          -- in repFieldArg.
          InfixE (Just acc)
                   (VarE '(Ins..))
                   (Just (VarE 'unComp1))
      in
        go acc' (dustOff x)
    go _ _ = varE (unboxRepName t0)

unboxRepName :: Type -> Name
unboxRepName = maybe 'unK1 trd3 . unboxedRepNames

lrP :: Int -> Int -> (Q Pat -> Q Pat)
lrP i n p
  | n == 0       = fail "lrP: impossible"
  | n == 1       = p
  | i <= div n 2 = conP 'L1 [lrP i     (div n 2) p]
  | otherwise    = conP 'R1 [lrP (i-m) (n-m)     p]
                     where m = div n 2

lrE :: Int -> Int -> (Q Exp -> Q Exp)
lrE i n e
  | n == 0       = fail "lrE: impossible"
  | n == 1       = e
  | i <= div n 2 = conE 'L1 `appE` lrE i     (div n 2) e
  | otherwise    = conE 'R1 `appE` lrE (i-m) (n-m)     e
                     where m = div n 2

unboxedRepNames :: Type -> Maybe (Name, Name, Name)
unboxedRepNames ty
  | ty == ConT ''Addr#   = Just (''UAddr,   'UAddr,   'uAddr#)
  | ty == ConT ''Char#   = Just (''UChar,   'UChar,   'uChar#)
  | ty == ConT ''Double# = Just (''UDouble, 'UDouble, 'uDouble#)
  | ty == ConT ''Float#  = Just (''UFloat,  'UFloat,  'uFloat#)
  | ty == ConT ''Int#    = Just (''UInt,    'UInt,    'uInt#)
  | ty == ConT ''Word#   = Just (''UWord,   'UWord,   'uWord#)
  | otherwise            = Nothing

-- For the given Types, deduces the instance type (and kind) to use for a
-- Generic(1) instance. Coming up with the instance type isn't as simple as
-- dropping the last types, as you need to be wary of kinds being instantiated
-- with *.
-- See Note [Type inference in derived instances]
buildTypeInstance :: GenericClass
                  -- ^ Generic or Generic1
                  -> Name
                  -- ^ The type constructor or data family name
                  -> [Type]
                  -- ^ The types to instantiate the instance with
                  -> Q (Type, Kind)
buildTypeInstance gClass tyConName varTysOrig = do
    -- Make sure to expand through type/kind synonyms! Otherwise, the
    -- eta-reduction check might get tripped up over type variables in a
    -- synonym that are actually dropped.
    -- (See GHC Trac #11416 for a scenario where this actually happened.)
    varTysExp <- mapM resolveTypeSynonyms varTysOrig

    let remainingLength :: Int
        remainingLength = length varTysOrig - fromEnum gClass

    -- Check there are enough types to drop. If not, throw an error.
    when (remainingLength < 0) $ derivingKindError tyConName

        -- Substitute kind * for any dropped kind variables
    let varTysExpSubst :: [Type]
        varTysExpSubst = varTysExp

    let remainingTysExpSubst, droppedTysExpSubst :: [Type]
        (remainingTysExpSubst, droppedTysExpSubst) =
          splitAt remainingLength varTysExpSubst

        -- We now substitute all of the specialized-to-* kind variable names
        -- with *, but in the original types, not the synonym-expanded types. The reason
        -- we do this is a superficial one: we want the derived instance to resemble
        -- the datatype written in source code as closely as possible. For example,
        -- for the following data family instance:
        --
        --   data family Fam a
        --   newtype instance Fam String = Fam String
        --
        -- We'd want to generate the instance:
        --
        --   instance C (Fam String)
        --
        -- Not:
        --
        --   instance C (Fam [Char])
    let
        remainingTysOrigSubst, droppedTysOrigSubst :: [Type]
        (remainingTysOrigSubst, droppedTysOrigSubst) =
            splitAt remainingLength varTysOrig

        instanceType :: Type
        instanceType = applyTyToTys (ConT tyConName) remainingTysOrigSubst

        -- See Note [Kind signatures in derived instances]
        instanceKind :: Kind
        instanceKind = makeFunKind (map typeKind droppedTysOrigSubst) starK

    -- Ensure the dropped types can be safely eta-reduced. Otherwise,
    -- throw an error.
    unless (canEtaReduce remainingTysExpSubst droppedTysExpSubst) $
      etaReductionError instanceType
    return (instanceType, instanceKind)

{-
Note [Kind signatures in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We include explicit type signatures in derived instances. One reason for
doing so is that in the case of certain data family instances, not including kind
signatures can result in ambiguity. For example, consider the following two data
family instances that are distinguished by their kinds:

  data family Fam (a :: k)
  data instance Fam (a :: * -> *)
  data instance Fam (a :: *)

If we dropped the kind signature for a in a derived instance for Fam a, then GHC
would have no way of knowing which instance we are talking about. The
DataFamilyKindsSpec test case checks that this behaves as intended.

In addition to using explicit kind signatures in the instance head, we also put
explicit kinds in the associated Rep(1) instance. For example, this data type:

  data S (a :: k) = S k

Will have the following Generic1 instance generated for it:

  instance Generic1 (S :: k -> *) where
    type Rep1 (S :: k -> *) = ... (Rec0 k)

Why do we do this? Imagine what the instance would be without the explicit kind
annotation in the Rep1 instance:

  instance Generic1 S where
    type Rep1 S = ... (Rec0 k)

This is an error, since the variable k is now out-of-scope! The TypeInTypeSpec
test case checks that this behaves as intended.
-}
