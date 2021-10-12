{-# language PolyKinds #-}
{-# language TemplateHaskell #-}

{-# options_ghc -Wno-orphans #-}

module Generics.Linear.Instances.Template_haskell (
  -- Instances only
  ) where
import Generics.Linear.TH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

deriveGeneric ''Loc
deriveGeneric ''Info
deriveGeneric ''ModuleInfo
deriveGeneric ''Extension
deriveGeneric ''AnnLookup
deriveGenericAnd1 ''Code
deriveGeneric ''Name
deriveGeneric ''NameSpace
deriveGeneric ''Dec
deriveGeneric ''Con
deriveGeneric ''Clause
deriveGeneric ''SourceUnpackedness
deriveGeneric ''SourceStrictness
deriveGeneric ''DecidedStrictness
deriveGeneric ''Bang
deriveGeneric ''Foreign
deriveGeneric ''Callconv
deriveGeneric ''Safety
deriveGeneric ''Pragma
deriveGeneric ''Inline
deriveGeneric ''RuleMatch
deriveGeneric ''Phases
deriveGeneric ''RuleBndr
deriveGeneric ''AnnTarget
deriveGeneric ''FunDep
deriveGeneric ''TySynEqn
deriveGeneric ''TypeFamilyHead
deriveGeneric ''Fixity
deriveGeneric ''FixityDirection
deriveGeneric ''PatSynDir
deriveGeneric ''PatSynArgs
deriveGeneric ''Exp
deriveGeneric ''Match
deriveGeneric ''Body
deriveGeneric ''Guard
deriveGeneric ''Stmt
deriveGeneric ''Range
deriveGeneric ''Lit
deriveGeneric ''Pat
deriveGeneric ''Type
deriveGenericAnd1 ''TyVarBndr
deriveGeneric ''TyLit
deriveGeneric ''Role
deriveGeneric ''Specificity
deriveGeneric ''FamilyResultSig
deriveGeneric ''InjectivityAnn
deriveGeneric ''OccName
deriveGeneric ''NameFlavour
deriveGeneric ''Module
deriveGeneric ''PkgName
-- No instance for TExp because it really wants that abstraction barrier.
deriveGeneric ''ForeignSrcLang
