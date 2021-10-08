{-# language LinearTypes #-}
{-# language MagicHash #-}
{-# language NoImplicitPrelude #-}
{-# language PolyKinds #-}
{-# language TypeOperators #-}
{-# options_haddock hide #-}

-- | Functions intended to be emitted by Template Haskell splices.  Do /not/
-- import this module outside the @linear-generics@ package—its contents will
-- depend on the GHC version and may change at absolutely any time.
module Generics.Linear.TH.Insertions where
import qualified Generics.Linear.Class as G
import qualified GHC.Exts as E

infixr 9 .
(.) :: (b %m-> c) -> (a %m-> b) -> a %m-> c
f . g = \x -> f (g x)
{-# INLINE (.) #-}

-- As of ghc-9.2.0.20210821, field accessors (even for newtypes) aren't
-- multiplicity polymorphic, so we have to make our own.

uAddr# :: G.UAddr a %m-> E.Addr#
uAddr# (G.UAddr a) = a
{-# INLINE uAddr# #-}

uChar# :: G.UChar a %m-> E.Char#
uChar# (G.UChar a) = a
{-# INLINE uChar# #-}

uDouble# :: G.UDouble a %m-> E.Double#
uDouble# (G.UDouble a) = a
{-# INLINE uDouble# #-}

uInt# :: G.UInt a %m-> E.Int#
uInt# (G.UInt a) = a
{-# INLINE uInt# #-}

uFloat# :: G.UFloat a %m-> E.Float#
uFloat# (G.UFloat a) = a
{-# INLINE uFloat# #-}

uWord# :: G.UWord a %m-> E.Word#
uWord# (G.UWord a) = a
{-# INLINE uWord# #-}

unComp1 :: (f G.:.: g) a %m-> f (g a)
unComp1 (G.Comp1 a) = a
{-# INLINE unComp1 #-}

unK1 :: G.K1 i c a %m-> c
unK1 (G.K1 c) = c
{-# INLINE unK1 #-}

unPar1 :: G.Par1 a %m-> a
unPar1 (G.Par1 a) = a
{-# INLINE unPar1 #-}
