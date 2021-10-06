{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
#endif

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE EmptyCase #-}
#endif

module EmptyCaseSpec (main, spec) where

import Generics.Linear.TH
import Test.Hspec

data Empty a
$(deriveGenericAnd1 ''Empty)

main :: IO ()
main = hspec spec

spec :: Spec
spec = return ()
