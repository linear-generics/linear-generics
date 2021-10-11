-- {-# LANGUAGE EmptyCase #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# language TemplateHaskell #-}
{-# language DataKinds #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Generics.Linear.Instances.Linear_generics (
  -- Instances only
  ) where

import Generics.Linear.Class
import Generics.Linear.TH

$(deriveGenericAnd1 ''(:.:))
