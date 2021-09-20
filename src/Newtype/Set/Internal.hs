{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Newtype.Set.Internal(
    Set(..), fromRawSet, extractSub, toAscList, relax
) where

import Prelude hiding (id, (.))
import Control.Category ( Category((.)) )
import qualified Data.Set as Data

import Data.Coerce
import Data.Type.Coercion.Sub
import Data.Type.Coercion.Sub.Internal ( Sub(Sub) )
import Data.Type.Coercion
import Data.Foldable

data Set u k where
  Mk :: Coercible k u => { toRawSet :: !(Data.Set u) } -> Set u k

type role Set nominal representational

fromRawSet :: Data.Set u -> Set u u
fromRawSet = Mk
{-# INLINABLE fromRawSet #-}

extractSub :: Set u k -> Sub k u
extractSub (Mk _) = sub
{-# INLINABLE extractSub #-}

deriving instance Eq u => Eq (Set u k)
deriving instance Ord u => Ord (Set u k)

instance Foldable (Set u) where
  foldMap f (Mk us) = foldMap (f . coerce) us
  foldr f z (Mk s) = Data.foldr (coerce f) z s
  foldl f z (Mk s) = Data.foldl (coerce f) z s
  foldr' f z (Mk s) = Data.foldr' (coerce f) z s
  foldl' f z (Mk s) = Data.foldl' (coerce f) z s

  null = Data.null . toRawSet
  length = Data.size . toRawSet

instance Show k => Show (Set u k) where
  showsPrec p s = showParen (p > 10) $ ("fromList " ++) . showList (toAscList s)

toAscList :: Set u k -> [k]
toAscList (Mk us) = coerce Data.toAscList us
{-# INLINABLE toAscList #-}

relax :: Sub k k' -> Sub (Set u k) (Set u k')
relax (Sub Coercion) = sub
{-# INLINABLE relax #-}