{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Newtype.Utils where

import Data.Coerce
import Data.Type.Coercion.Sub

coerce1 :: Coercible f g => f a -> g a
coerce1 = coerce

rep1 :: Coercible f g => Sub (f a) (g a)
rep1 = sub
