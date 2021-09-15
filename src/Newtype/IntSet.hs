{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
module Newtype.IntSet(
    -- * Core
    IntSet(), toRawIntSet, extractSub, relax,

    -- * Construction
    empty, singleton, fromRawIntSet,

    fromList, 
    fromAscList,
    fromDistinctAscList,

    -- * Single item manipulation
    insert, delete, alterF,

    -- * Query
    member, notMember, membership,

    ulookupLT, ulookupGT, ulookupLE, ulookupGE,
    lookupLT, lookupGT, lookupLE, lookupGE,

    null, size,

    -- * Conversion
    elems, toList, toAscList, toDescList,

    -- * Map, Filter
    filter, map, mapMaybe,

    -- * Combination
    union,
    UnionIntSet(..),
    tightUnion,

    intersection,
    IntersectionIntSet(..),
    tightIntersection,

    difference, (\\)

) where

import Prelude hiding (id, (.), null, lookup, filter, map, take, drop)

import Data.Coerce
import qualified Data.Maybe (mapMaybe)

import Control.Category
import Data.Type.Coercion.Sub
import Data.Type.Coercion.Sub.Internal
import Data.Type.Coercion

import qualified Data.IntSet as Data
import Newtype.IntSet.Internal
import Newtype.Union ( IsUnion, greater )
import Newtype.Intersection ( IsIntersection, lesser )
import qualified Data.Foldable


-- * Primitive construction

empty :: Sub k Int -> IntSet k
empty (Sub Coercion) = Mk Data.empty

singleton :: Sub k Int -> k -> IntSet k
singleton (Sub Coercion) k = Mk (coerce Data.singleton k)

-- * Construction from lists

fromList :: Sub k Int -> [k] -> IntSet k
fromList (Sub Coercion) ks = Mk (coerce Data.fromList ks)

fromAscList :: Sub k Int -> [k] -> IntSet k
fromAscList (Sub Coercion) ks = Mk (coerce Data.fromAscList ks)

fromDistinctAscList :: Sub k Int -> [k] -> IntSet k
fromDistinctAscList (Sub Coercion) ks = Mk (coerce Data.fromDistinctAscList ks)

-- * Insertion/Deletion

insert :: k -> IntSet k -> IntSet k
insert k (Mk us) = Mk (coerce Data.insert k us)

delete :: k -> IntSet k -> IntSet k
delete k (Mk us) = Mk (coerce Data.delete k us)

alterF :: (Functor f) => (Bool -> f Bool) -> k -> IntSet k -> f (IntSet k)
alterF f k (Mk us) = Mk <$> Data.alterF f (coerce k) us

-- * Query

member, notMember :: k -> IntSet k -> Bool
member k (Mk us) = coerce Data.member k us
notMember k = not . member k

membership :: Int -> IntSet k -> Maybe k
membership u (Mk us) = coerce $ if Data.member u us then Just u else Nothing

ulookupLT, ulookupGT, ulookupLE, ulookupGE
  :: Int -> IntSet k -> Maybe k
ulookupLT u (Mk us) = coerce Data.lookupLT u us
ulookupLE u (Mk us) = coerce Data.lookupLE u us
ulookupGT u (Mk us) = coerce Data.lookupGT u us
ulookupGE u (Mk us) = coerce Data.lookupGE u us

lookupLT, lookupGT, lookupLE, lookupGE
  :: k -> IntSet k -> Maybe k
lookupLT k (Mk us) = coerce Data.lookupLT k us
lookupLE k (Mk us) = coerce Data.lookupLE k us
lookupGT k (Mk us) = coerce Data.lookupGT k us
lookupGE k (Mk us) = coerce Data.lookupGE k us

-- * Size
null :: IntSet k -> Bool
null = Data.Foldable.null

size :: IntSet k -> Int
size = Data.Foldable.length

-- * Converting to
elems, toList, toDescList :: IntSet k -> [k]
elems = toAscList
toList = toAscList
toDescList (Mk us) = coerce $ Data.toDescList us

-- * Filter

filter :: (k -> Bool) -> IntSet k -> IntSet k
filter f (Mk us) = Mk (Data.filter (coerce f) us)

map :: Sub k' Int -> (k -> k') -> IntSet k -> IntSet k'
map (Sub Coercion) f (Mk us) = Mk (Data.map (coerce f) us)

mapMaybe :: Sub k' Int -> (k -> Maybe k') -> IntSet k -> IntSet k'
mapMaybe k'u' f = fromList k'u' . Data.Maybe.mapMaybe f . toList

-- * Combine
union :: IntSet k -> IntSet k -> IntSet k
union (Mk xs) (Mk ys) = Mk (Data.union xs ys)

data UnionIntSet x y where
    UnionIntSet :: IsUnion x y z -> IntSet z -> UnionIntSet k k'

tightUnion :: forall x y. IntSet x -> IntSet y -> UnionIntSet x y
tightUnion (Mk xs) (Mk ys) =
  let zs = Mk (Data.union xs ys) :: IntSet y
  in UnionIntSet (greater sub :: IsUnion x y y) zs

intersection :: IntSet k -> IntSet k' -> IntSet k
intersection (Mk xs) (Mk ys) = Mk (Data.intersection xs ys)

data IntersectionIntSet x y where
    IntersectionIntSet :: IsIntersection x y z -> IntSet z -> IntersectionIntSet x y

tightIntersection :: forall x y.
     IntSet x -> IntSet y -> IntersectionIntSet x y
tightIntersection (Mk xs) (Mk ys) =
  let zs = Mk (Data.intersection xs ys) :: IntSet x
  in IntersectionIntSet (lesser sub :: IsIntersection x y x) zs

difference :: IntSet k -> IntSet k' -> IntSet k
difference (Mk xs) (Mk ys) = Mk (xs Data.\\ ys)

(\\) :: IntSet k -> IntSet k -> IntSet k
(\\) = difference

infixl 9 \\
