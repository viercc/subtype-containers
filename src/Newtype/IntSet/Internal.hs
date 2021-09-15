{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Newtype.IntSet.Internal where

import Prelude hiding (id, (.))
import Control.Category ( Category((.)) )

import Data.Coerce
import Data.Type.Coercion.Sub
import Data.Type.Coercion.Sub.Internal ( Sub(Sub) )
import Data.Type.Coercion

import qualified Data.IntSet as Data
import Data.Foldable

data IntSet k where
  Mk :: Coercible k Int => { toRawIntSet :: Data.IntSet } -> IntSet k
type role IntSet representational

fromRawIntSet :: Data.IntSet -> IntSet Int
fromRawIntSet = Mk

extractSub :: IntSet k -> Sub k Int
extractSub (Mk _) = sub

deriving instance Eq (IntSet k)
deriving instance Ord (IntSet k)
instance Foldable IntSet where
  foldr f z (Mk s) = Data.foldr (coerce f) z s
  foldl f z (Mk s) = Data.foldl (coerce f) z s
  foldr' f z (Mk s) = Data.foldr' (coerce f) z s
  foldl' f z (Mk s) = Data.foldl' (coerce f) z s

  null = Data.null . toRawIntSet
  length = Data.size . toRawIntSet

instance (Show k) => Show (IntSet k) where
  showsPrec p s = showParen (p > 10) $ ("fromList " ++) . showList (toAscList s)

toAscList :: IntSet k -> [k]
toAscList (Mk us) = coerce Data.toList us

relax :: Sub k k' -> Sub (IntSet k) (IntSet k')
relax (Sub Coercion) = sub
