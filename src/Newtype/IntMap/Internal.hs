{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Newtype.IntMap.Internal where

import Prelude hiding (id, (.))
import Control.Category ( Category((.)) )

import Data.Coerce
import Data.Type.Coercion.Sub
import Data.Type.Coercion.Sub.Internal ( Sub(Sub) )
import Data.Type.Coercion

import qualified Data.IntMap.Lazy as Data

data IntMap k a where
  Mk :: Coercible k Int => { toRawIntMap :: Data.IntMap a } -> IntMap k a
type role IntMap representational representational

fromRawIntMap :: Data.IntMap a -> IntMap Int a
fromRawIntMap = Mk
{-# INLINABLE fromRawIntMap #-}

extractSub :: IntMap k a -> Sub k Int
extractSub (Mk _) = sub
{-# INLINABLE extractSub #-}

deriving instance (Eq a) => Eq (IntMap k a)
deriving instance (Ord a) => Ord (IntMap k a)
deriving instance Foldable (IntMap k)
deriving instance Functor (IntMap k)
deriving instance Traversable (IntMap k)

instance (Show k, Show a) => Show (IntMap k a) where
  showsPrec p s = showParen (p > 10) $ ("fromList " ++) . showList (toAscList s)

toAscList :: IntMap k a -> [(k,a)]
toAscList (Mk ma) = coerce (Data.toAscList ma)
{-# INLINABLE toAscList #-}

relaxKey :: Sub k k' -> Sub (IntMap k a) (IntMap k' a)
relaxKey (Sub Coercion) = sub
{-# INLINABLE relaxKey #-}
