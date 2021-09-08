{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
module Newtype.Intersection(
  IsIntersection(..),
  withIntersection,
  withIntersection',
  
  unique, symmetry, lesser, idemp, assocIntersection
) where

import Prelude hiding (id, (.))
import Control.Category
import Data.Type.Coercion.Sub (equiv)
import Data.Type.Coercion.Sub.Internal
import Data.Type.Coercion ( Coercion(Coercion), sym )
import Data.Coerce

data IsIntersection x y z = IsIntersection
  {
    proj1 :: !(Sub z x),
    proj2 :: !(Sub z y),
    conjunct :: forall s. Sub s x -> Sub s y -> Sub s z
  }

withIntersection :: Sub u x -> Sub u y -> (forall xy. IsIntersection x y xy -> r) -> r
withIntersection xu yu@(Sub Coercion) body =
    body IsIntersection{
        proj1 = xu,
        proj2 = yu,
        conjunct = \ !_ yr -> coerce yr
    }

withIntersection' :: Sub x u -> Sub y u -> (forall xy. IsIntersection x y xy -> r) -> r
withIntersection' (Sub xu@Coercion) (Sub yu@Coercion) body =
    body IsIntersection{
        proj1 = Sub (sym xu),
        proj2 = Sub (sym yu),
        conjunct = \ !_ sy -> coerce sy
    }

unique :: IsIntersection x y z -> IsIntersection x y z' -> Coercion z z'
unique xy xy' = equiv (conjunct xy' (proj1 xy) (proj2 xy)) (conjunct xy (proj1 xy') (proj2 xy'))

symmetry :: IsIntersection x y z -> IsIntersection y x z
symmetry xyz = IsIntersection{ proj1 = proj2 xyz, proj2 = proj1 xyz, conjunct = flip (conjunct xyz)}

lesser :: Sub x y -> IsIntersection x y x
lesser l = IsIntersection{ proj1=id, proj2=l, conjunct= \sx !_ -> sx }

idemp :: IsIntersection x x x
idemp = lesser id

assocIntersection :: IsIntersection x y xy -> IsIntersection xy z xy'z -> IsIntersection y z yz -> IsIntersection x yz x'yz -> Coercion xy'z x'yz
assocIntersection xy xy'z yz x'yz =
    equiv (conjunct x'yz (proj1 xy . proj1 xy'z) (conjunct yz (proj2 xy . proj1 xy'z) (proj2 xy'z)))
          (conjunct xy'z (conjunct xy (proj1 x'yz) (proj1 yz . proj2 x'yz)) (proj2 yz . proj2 x'yz))