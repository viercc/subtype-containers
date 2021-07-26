{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Newtype.Utils where

import Data.Coerce
import Data.Type.Coercion.Sub
import Data.Type.Coercion.Sub.Internal
import Data.Reflection
import Data.Type.Coercion (Coercion(Coercion))

ungiven :: forall a b. Given (Sub a b) => Sub b a
ungiven = case given :: Sub a b of Sub Coercion -> sub

(/->/) :: Sub a a' -> Sub b b' -> Sub (a' -> b) (a -> b')
(/->/) = dimapR

infixr 3 /->/

coerce1 :: Coercible f g => f a -> g a
coerce1 = coerce

rep1 :: Coercible f g => Sub (f a) (g a)
rep1 = sub
