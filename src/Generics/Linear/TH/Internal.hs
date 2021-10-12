{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      :  Generics.Linear.TH.Internal
Copyright   :  (c) 2008--2009 Universiteit Utrecht
License     :  BSD3

Maintainer  :  generics@haskell.org
Stability   :  experimental
Portability :  non-portable

Template Haskell-related utilities.
-}

module Generics.Linear.TH.Internal where

import           Control.Monad (unless)

import           Data.Foldable (foldr')
import qualified Data.List as List
import qualified Data.Set as Set
import           Data.Set (Set)

import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.Datatype.TyVarBndr
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Ppr (pprint)
import           Language.Haskell.TH.Syntax

-------------------------------------------------------------------------------
-- Assorted utilities
-------------------------------------------------------------------------------

-- Note: There are quite a few other utilities in
--
-- generic-deriving: Generics.Deriving.TH.Internal
--
-- Most of the ones that aren't used here have been stripped out.

-- | If a Type is a SigT, returns its kind signature. Otherwise, return *.
typeKind :: Type -> Kind
typeKind (SigT _ k) = k
typeKind _          = starK

-- | Turns
--
-- @
-- [a, b] c
-- @
--
-- into
--
-- @
-- a -> b -> c
-- @
makeFunType :: [Type] -> Type -> Type
makeFunType argTys resTy = foldr' (AppT . AppT ArrowT) resTy argTys

-- | Turns
--
-- @
-- [k1, k2] k3
-- @
--
-- into
--
-- @
-- k1 -> k2 -> k3
-- @
makeFunKind :: [Kind] -> Kind -> Kind
makeFunKind = makeFunType

-- | Remove any outer `SigT` and `ParensT` constructors, and turn
-- an outermost `InfixT` constructor into plain applications.
dustOff :: Type -> Type
dustOff (SigT ty _) = dustOff ty
dustOff (ParensT ty) = dustOff ty
dustOff (InfixT ty1 n ty2) = ConT n `AppT` ty1 `AppT` ty2
dustOff ty = ty

-- | Checks whether a type is an unsaturated type family
-- application.
isUnsaturatedType :: Type -> Q Bool
isUnsaturatedType = go 0 . dustOff
  where
    -- Expects its argument to be dusted
    go :: Int -> Type -> Q Bool
    go d t = case t of
      ConT tcName -> check d tcName
      AppT f _ -> go (d + 1) (dustOff f)
      _ -> return False

    check :: Int -> Name -> Q Bool
    check d tcName = do
      mbinders <- getTypeFamilyBinders tcName
      return $ case mbinders of
        Just bndrs -> length bndrs > d
        Nothing -> False

-- | Given a name, check if that name is a type family. If
-- so, return a list of its binders.
getTypeFamilyBinders :: Name -> Q (Maybe [TyVarBndr_ ()])
getTypeFamilyBinders tcName = do
      info <- reify tcName
      return $ case info of
        FamilyI (OpenTypeFamilyD (TypeFamilyHead _ bndrs _ _)) _
          -> Just bndrs

        FamilyI (ClosedTypeFamilyD (TypeFamilyHead _ bndrs _ _) _) _
          -> Just bndrs
        _ -> Nothing

-- | True if the type does not mention the Name
ground :: Type -> Name -> Bool
ground ty name = name `notElem` freeVariables ty

-- | Construct a type via curried application.
applyTyToTys :: Type -> [Type] -> Type
applyTyToTys = List.foldl' AppT

-- | Generate a list of fresh names with a common prefix, and numbered suffixes.
newNameList :: String -> Int -> Q [Name]
newNameList prefix n = mapM (newName . (prefix ++) . show) [1..n]

-- | Checks to see if the last types in a data family instance can be safely eta-
-- reduced (i.e., dropped), given the other types. This checks for three conditions:
--
-- (1) All of the dropped types are type variables
-- (2) All of the dropped types are distinct
-- (3) None of the remaining types mention any of the dropped types
canEtaReduce :: [Type] -> [Type] -> Bool
canEtaReduce remaining dropped =
       all isTyVar dropped
       -- Make sure not to pass something of type [Type], since Type
       -- didn't have an Ord instance until template-haskell-2.10.0.0
    && allDistinct droppedNames
    && not (any (`mentionsName` droppedNames) remaining)
  where
    droppedNames :: [Name]
    droppedNames = map varTToName dropped

-- | Extract the Name from a type variable. If the argument Type is not a
-- type variable, throw an error.
varTToName :: Type -> Name
varTToName (VarT n)   = n
varTToName (SigT t _) = varTToName t
varTToName _          = error "Not a type variable!"

-- | Is the given type a variable?
isTyVar :: Type -> Bool
isTyVar VarT{}     = True
isTyVar (SigT t _) = isTyVar t
isTyVar _          = False

-- | Is the given kind a variable?
isKindVar :: Kind -> Bool
isKindVar = isTyVar

-- | Does the given type mention any of the Names in the list?
mentionsName :: Type -> [Name] -> Bool
mentionsName = go
  where
    go :: Type -> [Name] -> Bool
    go (AppT t1 t2) names = go t1 names || go t2 names
    go (SigT t k)  names = go t names || go k names
    go (VarT n)     names = n `elem` names
    go _            _     = False

-- | Are all of the items in a list (which have an ordering) distinct?
--
-- This uses Set (as opposed to nub) for better asymptotic time complexity.
allDistinct :: Ord a => [a] -> Bool
allDistinct = allDistinct' Set.empty
  where
    allDistinct' :: Ord a => Set a -> [a] -> Bool
    allDistinct' uniqs (x:xs)
        | x `Set.member` uniqs = False
        | otherwise            = allDistinct' (Set.insert x uniqs) xs
    allDistinct' _ _           = True

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

foldBal :: (a -> a -> a) -> a -> [a] -> a
{-# INLINE foldBal #-} -- inlined to produce specialised code for each op
foldBal op0 x0 xs0 = fold_bal op0 x0 (length xs0) xs0
  where
    fold_bal op x !n xs = case xs of
      []  -> x
      [a] -> a
      _   -> let !nl = n `div` 2
                 !nr = n - nl
                 (l,r) = splitAt nl xs
             in fold_bal op x nl l
                `op` fold_bal op x nr r

isNewtypeVariant :: DatatypeVariant_ -> Bool
isNewtypeVariant Datatype_             = False
isNewtypeVariant Newtype_              = True
isNewtypeVariant (DataInstance_ {})    = False
isNewtypeVariant (NewtypeInstance_ {}) = True

-- | Indicates whether Generic or Generic1 is being derived.
data GenericClass = Generic | Generic1 deriving Enum

-- | Records information about the type variables of a data type with a
-- 'Generic' or 'Generic1' instance.
data GenericTvbs
    -- | Information about a data type with a 'Generic' instance.
  = Gen0
      { gen0Tvbs :: [TyVarBndrUnit]
        -- ^ All of the type variable arguments to the data type.
      }
    -- | Information about a data type with a 'Generic1' instance.
  | Gen1
      { gen1InitTvbs :: [TyVarBndrUnit]
        -- ^ All of the type variable arguments to the data type except the
        --   last one. In a @'Generic1' (T a_1 ... a_(n-1))@ instance, the
        --   'gen1InitTvbs' would be @[a_1, ..., a_(n-1)]@.
      , gen1LastTvbName :: Name
        -- ^ The name of the last type variable argument to the data type.
        --   In a @'Generic1' (T a_1 ... a_(n-1))@ instance, the
        --   'gen1LastTvbName' name would be @a_n@.
      }

-- | Compute 'GenericTvbs' from a 'GenericClass' and the type variable
-- arguments to a data type.
mkGenericTvbs :: GenericClass -> [Type] -> GenericTvbs
mkGenericTvbs gClass tySynVars =
  case gClass of
    Generic  -> Gen0{gen0Tvbs = freeVariablesWellScoped tySynVars}
    Generic1 -> Gen1{ gen1InitTvbs    = freeVariablesWellScoped initArgs
                    , gen1LastTvbName = varTToName lastArg
                    }
  where
    -- Everything below is only used for Generic1.
    initArgs :: [Type]
    initArgs = init tySynVars

    lastArg :: Type
    lastArg = last tySynVars

-- | Return the type variable arguments to a data type that appear in a
-- 'Generic' or 'Generic1' instance. For a 'Generic' instance, this consists of
-- all the type variable arguments. For a 'Generic1' instance, this consists of
-- all the type variable arguments except for the last one.
genericInitTvbs :: GenericTvbs -> [TyVarBndrUnit]
genericInitTvbs (Gen0{gen0Tvbs = tvbs})     = tvbs
genericInitTvbs (Gen1{gen1InitTvbs = tvbs}) = tvbs

-- | A version of 'DatatypeVariant' in which the data family instance
-- constructors come equipped with the 'ConstructorInfo' of the first
-- constructor in the family instance (for 'Name' generation purposes).
data DatatypeVariant_
  = Datatype_
  | Newtype_
  | DataInstance_    ConstructorInfo
  | NewtypeInstance_ ConstructorInfo

-- | One of the last type variables cannot be eta-reduced (see the canEtaReduce
-- function for the criteria it would have to meet).
etaReductionError :: Type -> Q a
etaReductionError instanceType = fail $
  "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
  ++ pprint instanceType

-- | Either the given data type doesn't have enough type variables, or one of
-- the type variables to be eta-reduced cannot realize kind *.
derivingKindError :: Name -> Q a
derivingKindError tyConName = fail
  . showString "Cannot derive well-kinded instance of form ‘Generic1 "
  . showParen True
    ( showString (nameBase tyConName)
    . showString " ..."
    )
  . showString "‘\n\tClass Generic1 expects an argument of kind k -> Type"
  $ ""

-- | The data type mentions the last type variable in a place other
-- than the last position of a data type in a constructor's field.
outOfPlaceTyVarError :: Q a
outOfPlaceTyVarError = fail
  . showString "Constructor must only use its last type variable as"
  . showString " the last argument of a data type"
  $ ""

-- | The data type mentions the last type variable in a type family
-- application.
typeFamilyApplicationError :: Q a
typeFamilyApplicationError = fail
  . showString "Constructor must not apply its last type variable"
  . showString " to an unsaturated type family"
  $ ""

-- | Cannot have a constructor argument of form (forall a1 ... an. <type>)
-- when deriving Generic(1)
rankNError :: Q a
rankNError = fail "Cannot have polymorphic arguments"

-- | Boilerplate for top level splices.
--
-- The given Name must meet one of two criteria:
--
-- 1. It must be the name of a type constructor of a plain data type or newtype.
-- 2. It must be the name of a data family instance or newtype instance constructor.
--
-- Any other value will result in an exception.
reifyDataInfo :: Name
              -> Q (Name, [Type], [ConstructorInfo], DatatypeVariant_)
reifyDataInfo name = do
  do
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = tys
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } <-
                     fail (ns ++ " Could not reify " ++ nameBase name)
                     `recover`
                     reifyDatatype name
    let variant_ = case variant of
                     Datatype        -> Datatype_
                     Newtype         -> Newtype_
                     -- This isn't total, but the API requires that the data
                     -- family instance have at least one constructor anyways,
                     -- so this will always succeed.
                     DataInstance    -> DataInstance_    $ head cons
                     NewtypeInstance -> NewtypeInstance_ $ head cons
    checkDataContext parentName ctxt
    pure (parentName, tys, cons, variant_)
  where
    ns :: String
    ns = "Generics.Linear.TH.reifyDataInfo: "

-- | One cannot derive Generic(1) instance for anything that uses DatatypeContexts,
-- so check to make sure the Cxt field of a datatype is null.
checkDataContext :: Name -> Cxt -> Q ()
checkDataContext _        [] = pure ()
checkDataContext dataName _ = fail $
  nameBase dataName ++ " must not have a datatype context"

-- | Deriving Generic(1) doesn't work with ExistentialQuantification or GADTs.
checkExistentialContext :: Name -> [TyVarBndrUnit] -> Cxt -> Q ()
checkExistentialContext conName vars ctxt =
  unless (null vars && null ctxt) $ fail $
    nameBase conName ++ " must be a vanilla data constructor"
