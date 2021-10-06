{-# language LinearTypes #-}
{-# language MagicHash #-}
{-# language NoImplicitPrelude #-}
{-# language PolyKinds #-}
{-# language TypeOperators #-}
{-# options_haddock hide #-}
module Generics.Linear.TH.Insertions where
import qualified Generics.Linear.Class as G
import qualified GHC.Exts as E

infixr 9 .
(.) :: (b %m-> c) -> (a %m-> b) -> a %m-> c
f . g = \x -> f (g x)
{-# INLINE (.) #-}

uAddr# :: G.UAddr a %m-> E.Addr#
uAddr# (G.UAddr a) = a

uChar# :: G.UChar a %m-> E.Char#
uChar# (G.UChar a) = a

uDouble# :: G.UDouble a %m-> E.Double#
uDouble# (G.UDouble a) = a

uInt# :: G.UInt a %m-> E.Int#
uInt# (G.UInt a) = a

uFloat# :: G.UFloat a %m-> E.Float#
uFloat# (G.UFloat a) = a

uWord# :: G.UWord a %m-> E.Word#
uWord# (G.UWord a) = a

unComp1 :: (f G.:.: g) a %m-> f (g a)
unComp1 (G.Comp1 a) = a

unK1 :: G.K1 i c a %m-> c
unK1 (G.K1 c) = c

unPar1 :: G.Par1 a %m-> a
unPar1 (G.Par1 a) = a
