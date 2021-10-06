{-# LANGUAGE Safe #-}

-- | Multiplicity polymorphic versions of @"GHC.Generics".'G.Generic'@ and
-- @"GHC.Generics".'G.Generic1'. 'Generic' is otherwise identical to the
-- standard version. 'Generic1' is similar, but with modifications that
-- make it more efficient, as well as supporting linearity.
--
-- This module re-exports everything in "GHC.Generics" except 'G.Generic',
-- 'G.Generic1', 'G.Rec1', and 'G.:.:'. The 'Generic1' representations here
-- don't need 'G.Rec1'. We expose our own, identical, ':.:'. This allows
-- users to instantiate their 'Generic1'-based generic-deriving classes so
-- they can be used with /both/ "GHC.Generics" and this package.

module Generics.Linear (module Generics.Linear.Class) where

import Generics.Linear.Class
import Generics.Linear.Instances ()
