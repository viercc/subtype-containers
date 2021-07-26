{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RoleAnnotations #-}
module Newtype.Map.Internal(
    Map(..)
) where

import qualified Data.Map.Lazy as Data

newtype Map u k a = Mk { toRawMap :: Data.Map u a }
  deriving (Eq, Ord, Functor, Foldable, Traversable)
type role Map nominal representational representational
