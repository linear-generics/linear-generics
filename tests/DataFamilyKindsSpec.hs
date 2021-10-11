{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | A test case which ensures that generated Generic instances are able to
-- distinguish data family instances which only differ in their kinds. See
-- https://github.com/linear-generics/linear-generics/issues/8#issuecomment-940512978
module DataFamilyKindsSpec (main, spec) where

import Data.Kind (Type)
import Generics.Linear.TH (deriveGeneric)
import Test.Hspec (Spec, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = pure ()

type Fam :: k -> Type
data family Fam a

data instance Fam (a :: Type -> Type) = Fam1
data instance Fam (a :: Type)         = Fam2

$(deriveGeneric 'Fam1)
$(deriveGeneric 'Fam2)
