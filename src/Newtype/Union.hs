{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
module Newtype.Union(
  IsUnion(..),
  withUnion,
  withUnion',
  
  unique, symmetry, greater, idemp, assocUnion
) where

import Prelude hiding (id, (.))
import Control.Category
import Data.Type.Coercion.Sub (equiv)
import Data.Type.Coercion.Sub.Internal
import Data.Type.Coercion ( Coercion(Coercion), sym )
import Data.Coerce

data IsUnion x y z = IsUnion
  {
    inl :: !(Sub x z),
    inr :: !(Sub y z),
    elim :: forall r. Sub x r -> Sub y r -> Sub z r
  }

withUnion :: Sub x u -> Sub y u -> (forall xy. IsUnion x y xy -> r) -> r
withUnion xu yu@(Sub Coercion) body =
    body IsUnion{
        inl = xu,
        inr = yu,
        elim = \ !_ yr -> coerce yr
    }

withUnion' :: Sub u x -> Sub u y -> (forall xy. IsUnion x y xy -> r) -> r
withUnion' (Sub ux@Coercion) (Sub uy@Coercion) body =
    body IsUnion{
        inl = Sub (sym ux),
        inr = Sub (sym uy),
        elim = \ !_ yr -> coerce yr
    }

unique :: IsUnion x y z -> IsUnion x y z' -> Coercion z z'
unique xy xy' = equiv (elim xy (inl xy') (inr xy')) (elim xy' (inl xy) (inr xy))

greater :: Sub x y -> IsUnion x y y
greater l = IsUnion{ inl = l, inr = id, elim=seq }

idemp :: IsUnion x x x
idemp = greater id

symmetry :: IsUnion x y z -> IsUnion y x z
symmetry xyz = IsUnion{ inl = inr xyz, inr = inl xyz, elim = flip (elim xyz)}

assocUnion :: IsUnion x y xy -> IsUnion xy z xy'z -> IsUnion y z yz -> IsUnion x yz x'yz -> Coercion xy'z x'yz
assocUnion xy xy'z yz x'yz =
    equiv (elim xy'z (elim xy (inl x'yz) (inr x'yz . inl yz)) (inr x'yz . inr yz))
          (elim x'yz (inl xy'z . inl xy) (elim yz (inl xy'z . inr xy) (inr xy'z)))