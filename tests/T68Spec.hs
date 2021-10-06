{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
#endif

module T68Spec (main, spec) where

import Generics.Linear.TH
import Test.Hspec
import Data.Kind (Type)

main :: IO ()
main = hspec spec

spec :: Spec
spec = return ()

type family F68 :: Type -> Type
type instance F68 = Maybe
data T68 a = MkT68 (F68 a)
$(deriveGeneric1 ''T68)
