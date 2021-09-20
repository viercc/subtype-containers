{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Newtype.Map.Internal(
    Map(..),
    fromRawMap, extractSub,
    toAscList, relaxKey
) where

import Prelude hiding (id, (.))
import Control.Category ( Category((.)) )

import Data.Coerce
import Data.Type.Coercion.Sub
import Data.Type.Coercion.Sub.Internal ( Sub(Sub) )
import Data.Type.Coercion ( Coercion(Coercion) )

import qualified Data.Map.Lazy as Data

data Map u k a where
  Mk :: Coercible k u => { toRawMap :: Data.Map u a } -> Map u k a
type role Map nominal representational representational

fromRawMap :: Data.Map u a -> Map u u a
fromRawMap = Mk
{-# INLINABLE fromRawMap #-}

extractSub :: Map u k a -> Sub k u
extractSub (Mk _) = sub
{-# INLINABLE extractSub #-}

deriving instance (Eq u, Eq a) => Eq (Map u k a)
deriving instance (Ord u, Ord a) => Ord (Map u k a)
deriving instance Foldable (Map u k)
deriving instance Functor (Map u k)
deriving instance Traversable (Map u k)

instance (Show k, Show a) => Show (Map u k a) where
  showsPrec p s = showParen (p > 10) $ ("fromList " ++) . showList (toAscList s)

toAscList :: Map u k a -> [(k,a)]
toAscList (Mk ma) = coerce (Data.toAscList ma)
{-# INLINABLE toAscList #-}

relaxKey :: Sub k k' -> Sub (Map u k a) (Map u k' a)
relaxKey (Sub Coercion) = sub
{-# INLINABLE relaxKey #-}
