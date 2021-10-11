{-# language TemplateHaskell #-}

{-# options_ghc -Wno-orphans #-}

module Generics.Linear.Instances.Containers where
import Generics.Linear.TH

import Data.Tree (Tree)
import Data.Sequence (ViewL, ViewR)
import Data.Sequence.Internal (FingerTree, Node, Digit, Elem)

$(deriveGenericAnd1 ''Tree)
$(deriveGenericAnd1 ''ViewL)
$(deriveGenericAnd1 ''ViewR)
$(deriveGenericAnd1 ''FingerTree)
$(deriveGenericAnd1 ''Node)
$(deriveGenericAnd1 ''Digit)
$(deriveGenericAnd1 ''Elem)
