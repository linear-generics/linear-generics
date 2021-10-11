{- |
Module      :  Generics.Linear.TH.MetaData
Copyright   :  (c) 2008--2009 Universiteit Utrecht
License     :  BSD3

Maintainer  :  generics@haskell.org
Stability   :  experimental
Portability :  non-portable

Template Haskell machinery for the type-literal-based variant of GHC
generics introduced in @base-4.9@.
-}

{-# LANGUAGE TemplateHaskellQuotes #-}

module Generics.Linear.TH.MetaData (
      mkMetaDataType
    , mkMetaConsType
    , mkMetaSelType
    , SelStrictInfo(..)
    , reifySelStrictInfo
  ) where

import Data.Maybe (fromMaybe)

import Generics.Linear.TH.Internal

import Language.Haskell.TH.Datatype as THAbs
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

-- For splices

import qualified Generics.Linear.Class as G

mkMetaDataType :: DatatypeVariant_ -> Name -> Q Type
mkMetaDataType dv n =
           promotedT 'G.MetaData
    `appT` litT (strTyLit (nameBase n))
    `appT` litT (strTyLit m)
    `appT` litT (strTyLit pkg)
    `appT` promoteBool (isNewtypeVariant dv)
  where
    m, pkg :: String
    m   = fromMaybe (error "Cannot fetch module name!")  (nameModule n)
    pkg = fromMaybe (error "Cannot fetch package name!") (namePackage n)

mkMetaConsType :: DatatypeVariant_ -> Name -> Name -> Bool -> Bool -> Q Type
mkMetaConsType _ _ n conIsRecord conIsInfix = do
    mbFi <- reifyFixity n
    promotedT 'G.MetaCons
      `appT` litT (strTyLit (nameBase n))
      `appT` fixityIPromotedType mbFi conIsInfix
      `appT` promoteBool conIsRecord

promoteBool :: Bool -> Q Type
promoteBool True  = promotedT 'True
promoteBool False = promotedT 'False

fixityIPromotedType :: Maybe Fixity -> Bool -> Q Type
fixityIPromotedType mbFi True =
           promotedT 'G.InfixI
    `appT` promoteAssociativity a
    `appT` litT (numTyLit (toInteger n))
  where
    Fixity n a = fromMaybe defaultFixity mbFi
fixityIPromotedType _ False = promotedT 'G.PrefixI

promoteAssociativity :: FixityDirection -> Q Type
promoteAssociativity InfixL = promotedT 'G.LeftAssociative
promoteAssociativity InfixR = promotedT 'G.RightAssociative
promoteAssociativity InfixN = promotedT 'G.NotAssociative

mkMetaSelType :: DatatypeVariant_ -> Name -> Name -> Maybe Name
              -> SelStrictInfo -> Q Type
mkMetaSelType _ _ _ mbF (SelStrictInfo su ss ds) =
    let mbSelNameT = case mbF of
            Just f  -> promotedT 'Just `appT` litT (strTyLit (nameBase f))
            Nothing -> promotedT 'Nothing
    in promotedT 'G.MetaSel
        `appT` mbSelNameT
        `appT` promoteUnpackedness su
        `appT` promoteStrictness ss
        `appT` promoteDecidedStrictness ds

data SelStrictInfo = SelStrictInfo Unpackedness Strictness DecidedStrictness

promoteUnpackedness :: Unpackedness -> Q Type
promoteUnpackedness UnspecifiedUnpackedness = promotedT 'G.NoSourceUnpackedness
promoteUnpackedness NoUnpack                = promotedT 'G.SourceNoUnpack
promoteUnpackedness Unpack                  = promotedT 'G.SourceUnpack

promoteStrictness :: Strictness -> Q Type
promoteStrictness UnspecifiedStrictness = promotedT 'G.NoSourceStrictness
promoteStrictness Lazy                  = promotedT 'G.SourceLazy
promoteStrictness THAbs.Strict          = promotedT 'G.SourceStrict

promoteDecidedStrictness :: DecidedStrictness -> Q Type
promoteDecidedStrictness DecidedLazy   = promotedT 'G.DecidedLazy
promoteDecidedStrictness DecidedStrict = promotedT 'G.DecidedStrict
promoteDecidedStrictness DecidedUnpack = promotedT 'G.DecidedUnpack

reifySelStrictInfo :: Name -> [FieldStrictness] -> Q [SelStrictInfo]
reifySelStrictInfo conName fs = do
    dcdStrs <- reifyConStrictness conName
    let srcUnpks = map fieldUnpackedness fs
        srcStrs  = map fieldStrictness   fs
    return $ zipWith3 SelStrictInfo srcUnpks srcStrs dcdStrs
