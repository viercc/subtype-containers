{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Newtype.Set.Internal(
    Set(..), toList
) where

import qualified Data.Set as Data

import Data.Reflection
import Data.Type.Coercion.Sub
import Newtype.Utils

newtype Set u k = Mk { toRawSet :: Data.Set u }
  deriving (Eq, Ord)
type role Set nominal representational

toList :: (Given (Sub k u)) => Set u k -> [k]
toList = upcastWith (sub /->/ mapR ungiven) Data.toList

instance (Given (Sub k u), Show k) => Show (Set u k) where
  showsPrec p s = showParen (p > 10) $ ("fromList " ++) . showList (toList s)
