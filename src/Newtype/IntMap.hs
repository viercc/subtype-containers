{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
module Newtype.IntMap(
    -- * Core
    IntMap(), toRawIntMap, extractSub, fromRawIntMap, relaxKey,
    
    -- * Construction
    empty, singleton,

    fromList, fromListWith, fromListWithKey,
    fromAscList,
    fromDistinctAscList,
    fromSet,

    -- * Single Item Manipulation
    insert, insertWith, insertWithKey,
    insertLookupWithKey,

    delete, adjust, update, updateLookupWithKey,
    alter, alterF,
   
    -- * Query

    -- ** Lookup
    ulookup, ulookupAndKey,
    lookup, (!?), (!),
    findWithDefault,
    membership, member, notMember,

    ulookupLT, ulookupGT, ulookupLE, ulookupGE,
    lookupLT, lookupGT, lookupLE, lookupGE,

    -- ** Size
    null, size,

    -- * Conversion
    elems, keys, assocs,
    keysSet,
    toList, toAscList, toDescList,

    -- * Map/Filter
    map, mapWithKey, traverseWithKey,
    mapKeys, mapKeysWith,

    filter, filterWithKey,
    mapMaybe, mapMaybeWithKey,


    -- * Combination

    -- ** Union
    union, unionWith,
    UnionIntMap(..),
    tightUnionWith,
    
    -- ** Intersection
    intersection,
    intersectionWith,
    IntersectionIntMap(..),
    tightIntersectionWith,
    restrictKeysBy, restrictKeysTo,
    tightRestrictKeys,

    -- ** Difference
    difference, (\\), differenceWith,
    withoutKeys,

    -- ** Composition
    compose
) where

import Prelude hiding (id, (.), null, map, filter, lookup)

import Data.Coerce

import Control.Category
import Data.Type.Coercion.Sub
import Data.Type.Coercion.Sub.Internal ( Sub(Sub) )
import Data.Type.Coercion ( Coercion(Coercion) )
import Data.Maybe (fromMaybe)
import qualified Data.Foldable as F

import Newtype.IntSet (IntSet)
import qualified Newtype.IntSet.Internal as IntSet

import qualified Data.IntMap.Lazy as Data
import Newtype.IntMap.Internal
import Newtype.Union ( IsUnion, greater )
import Newtype.Intersection ( IsIntersection, lesser )

-- * Primitive construction

empty :: Sub k Int -> IntMap k a
empty (Sub Coercion) = Mk Data.empty

singleton :: Sub k Int -> k -> a -> IntMap k a
singleton (Sub Coercion) k a = Mk (Data.singleton (coerce k) a)

fromSet :: (k -> a) -> IntSet k -> IntMap k a
fromSet f (IntSet.Mk us) = Mk (Data.fromSet (coerce f) us)

-- * Construction from lists

fromList :: Sub k Int -> [(k,a)] -> IntMap k a
fromList (Sub Coercion) kas = Mk (Data.fromList (coerce kas))

fromListWith :: Sub k Int -> (a -> a -> a) -> [(k,a)] -> IntMap k a
fromListWith (Sub Coercion) op kas = Mk (Data.fromListWith op (coerce kas))

fromListWithKey :: Sub k Int -> (k -> a -> a -> a) -> [(k,a)] -> IntMap k a
fromListWithKey (Sub Coercion) op kas = Mk (Data.fromListWithKey (coerce op) (coerce kas))

fromAscList :: Sub k Int -> [(k,a)] -> IntMap k a
fromAscList (Sub Coercion) kas = Mk (Data.fromAscList (coerce kas))

fromDistinctAscList :: Sub k Int -> [(k,a)] -> IntMap k a
fromDistinctAscList (Sub Coercion) kas = Mk (Data.fromDistinctAscList (coerce kas))

-- * Insertion

insert :: k -> a -> IntMap k a -> IntMap k a
insert k a (Mk ma) = Mk (Data.insert (coerce k) a ma)

insertWith :: (a -> a -> a) -> k -> a -> IntMap k a -> IntMap k a
insertWith op k a (Mk ma) = Mk (Data.insertWith op (coerce k) a ma)

insertWithKey :: (k -> a -> a -> a) -> k -> a -> IntMap k a -> IntMap k a
insertWithKey op k a (Mk ma) = Mk (Data.insertWithKey (coerce op) (coerce k) a ma)

insertLookupWithKey :: (k -> a -> a -> a) -> k -> a -> IntMap k a -> (Maybe a, IntMap k a)
insertLookupWithKey op k a (Mk ma) = case Data.insertLookupWithKey (coerce op) (coerce k) a ma of
  ~(resp, ma') -> (resp, Mk ma')

-- * Deletion/Update

delete :: k -> IntMap k a -> IntMap k a
delete k (Mk ma) = Mk (Data.delete (coerce k) ma)

adjust :: (a -> a) -> k -> IntMap k a -> IntMap k a
adjust f k (Mk ma) = Mk (Data.adjust f (coerce k) ma)

update :: (a -> Maybe a) -> k -> IntMap k a -> IntMap k a
update f k (Mk ma) = Mk (Data.update f (coerce k) ma)

updateLookupWithKey :: (k -> a -> Maybe a) -> k -> IntMap k a -> (Maybe a, IntMap k a)
updateLookupWithKey f k (Mk ma) = case Data.updateLookupWithKey (coerce f) (coerce k) ma of
  ~(resp, ma') -> (resp, Mk ma')

alter :: (Maybe a -> Maybe a) -> k -> IntMap k a -> IntMap k a
alter f k (Mk ma) = Mk (Data.alter f (coerce k) ma)

alterF :: (Functor f)
  => (Maybe a -> f (Maybe a)) -> k -> IntMap k a -> f (IntMap k a)
alterF f k (Mk ma) = Mk <$> Data.alterF f (coerce k) ma

-- * Query

lookup :: k -> IntMap k a -> Maybe a
lookup k (Mk ma) = Data.lookup (coerce k) ma

ulookup :: Int -> IntMap k a -> Maybe a
ulookup u (Mk ma) = Data.lookup u ma

ulookupAndKey :: Int -> IntMap k a -> Maybe (k, a)
ulookupAndKey u (Mk ma) = (coerce u, ) <$> Data.lookup u ma

(!?) :: IntMap k a -> k -> Maybe a
(!?) = flip lookup

(!) :: IntMap k a -> k -> a
(!) m k = fromMaybe (error "element not in the Map") $ lookup k m

findWithDefault :: a -> k -> IntMap k a -> a
findWithDefault a k m = fromMaybe a $ lookup k m

membership :: Int -> IntMap k a -> Maybe k
membership u (Mk ma) = coerce $ if Data.member u ma then Just u else Nothing

member, notMember :: k -> IntMap k a -> Bool
member k (Mk ma) = Data.member (coerce k) ma
notMember k = not . member k

ulookupLT, ulookupGT, ulookupLE, ulookupGE
  :: Int -> IntMap k a -> Maybe (k,a)
ulookupLT u (Mk ma) = coerce $ Data.lookupLT u ma
ulookupGT u (Mk ma) = coerce $ Data.lookupGT u ma
ulookupLE u (Mk ma) = coerce $ Data.lookupLE u ma
ulookupGE u (Mk ma) = coerce $ Data.lookupGE u ma

lookupLT, lookupGT, lookupLE, lookupGE
  :: k -> IntMap k a -> Maybe (k,a)
lookupLT k (Mk ma) = coerce $ Data.lookupLT (coerce k) ma
lookupGT k (Mk ma) = coerce $ Data.lookupGT (coerce k) ma
lookupLE k (Mk ma) = coerce $ Data.lookupLE (coerce k) ma
lookupGE k (Mk ma) = coerce $ Data.lookupGE (coerce k) ma

-- * Size
null :: IntMap k a -> Bool
null = Data.null . toRawIntMap

size :: IntMap k a -> Int
size = Data.size . toRawIntMap

-- * Conversion

elems :: IntMap k a -> [a]
elems = F.toList

keys :: IntMap k a -> [k]
keys (Mk ma) = coerce $ Data.keys ma

assocs :: IntMap k a -> [(k,a)]
assocs = toAscList

keysSet :: IntMap k a -> IntSet k
keysSet (Mk ma) = IntSet.Mk (Data.keysSet ma)

toList :: IntMap k a -> [(k,a)]
toList = toAscList

toDescList :: IntMap k a -> [(k,a)]
toDescList (Mk ma) = coerce $ Data.toDescList ma

-- * Traversal/Map
map :: (a -> b) -> IntMap k a -> IntMap k b
map = fmap

mapWithKey :: (k -> a -> b) -> IntMap k a -> IntMap k b
mapWithKey f (Mk ma) = Mk (Data.mapWithKey (coerce f) ma)

traverseWithKey :: (Applicative f) => (k -> a -> f b) -> IntMap k a -> f (IntMap k b)
traverseWithKey f (Mk ma) = Mk <$> Data.traverseWithKey (coerce f) ma

mapKeys :: Sub k' Int -> (k -> k') -> IntMap k a -> IntMap k' a
mapKeys (Sub Coercion) f (Mk ma) = Mk (Data.mapKeys (coerce f) ma)

mapKeysWith :: Sub k' Int -> (a -> a -> a) -> (k -> k') -> IntMap k a -> IntMap k' a
mapKeysWith (Sub Coercion) op f (Mk ma) = Mk (Data.mapKeysWith op (coerce f) ma)

-- * Filtering

filter :: (a -> Bool) -> IntMap k a -> IntMap k a
filter f (Mk ma) = Mk (Data.filter f ma)

filterWithKey ::  (k -> a -> Bool) -> IntMap k a -> IntMap k a
filterWithKey f (Mk ma) = Mk (Data.filterWithKey (coerce f) ma)

mapMaybe :: (a -> Maybe b) -> IntMap k a -> IntMap k b
mapMaybe f (Mk ma) = Mk (Data.mapMaybe f ma)

mapMaybeWithKey ::  (k -> a -> Maybe b) -> IntMap k a -> IntMap k b
mapMaybeWithKey f (Mk ma) = Mk (Data.mapMaybeWithKey (coerce f) ma)

restrictKeysBy ::  IntMap k a -> IntSet k' -> IntMap k a
restrictKeysBy (Mk ma) (IntSet.Mk us) = Mk (Data.restrictKeys ma us)

restrictKeysTo ::  IntMap k a -> IntSet k' -> IntMap k' a
restrictKeysTo (Mk ma) (IntSet.Mk us) = Mk (Data.restrictKeys ma us)

tightRestrictKeys :: forall x y a.
  IntMap x a -> IntSet y -> IntersectionIntMap x y a
tightRestrictKeys (Mk ma) (IntSet.Mk us) =
    let ma' = Mk (Data.restrictKeys ma us) :: IntMap x a
    in IntersectionIntMap (lesser sub :: IsIntersection x y x) ma'

withoutKeys ::  IntMap k a -> IntSet k' -> IntMap k a
withoutKeys (Mk ma) (IntSet.Mk us) = Mk (Data.withoutKeys ma us)

-- * Combine
union ::  IntMap k a -> IntMap k a -> IntMap k a
union (Mk m1) (Mk m2) = Mk (Data.union m1 m2)

unionWith ::  (a -> a -> a) -> IntMap k a -> IntMap k a -> IntMap k a
unionWith op (Mk m1) (Mk m2) = Mk (Data.unionWith op m1 m2)

data UnionIntMap x y a where
    UnionIntMap :: IsUnion x y z -> IntMap z a -> UnionIntMap x y a

tightUnionWith :: forall x y a.
  (a -> a -> a) -> IntMap x a -> IntMap y a -> UnionIntMap x y a
tightUnionWith op (Mk mx) (Mk my) =
  let mz = Mk (Data.unionWith op mx my) :: IntMap y a
  in UnionIntMap (greater sub :: IsUnion x y y) mz

intersection ::  IntMap k a -> IntMap k' b -> IntMap k a
intersection (Mk m1) (Mk m2) = Mk (Data.intersection m1 m2)

intersectionWith ::  (a -> b -> c) -> IntMap k a -> IntMap k' b -> IntMap k c
intersectionWith op (Mk m1) (Mk m2) = Mk (Data.intersectionWith op m1 m2)

data IntersectionIntMap x y a where
    IntersectionIntMap :: IsIntersection x y z -> IntMap z a -> IntersectionIntMap x y a

tightIntersectionWith :: forall x y a b c.
     (a -> b -> c) -> IntMap x a -> IntMap y b -> IntersectionIntMap x y c
tightIntersectionWith op (Mk mx) (Mk my) =
  let mz = Mk (Data.intersectionWith op mx my) :: IntMap x c
  in IntersectionIntMap (lesser sub :: IsIntersection x y x) mz

difference ::  IntMap k a -> IntMap k' b -> IntMap k a
difference (Mk m1) (Mk m2) = Mk (Data.difference m1 m2)

(\\) ::  IntMap k a -> IntMap k b -> IntMap k a
(\\) = difference

differenceWith ::  (a -> b -> Maybe a) -> IntMap k a -> IntMap k' b -> IntMap k a
differenceWith f (Mk m1) (Mk m2) = Mk (Data.differenceWith f m1 m2)

infixl 9 \\

-- * Composition

compose :: IntMap b c -> IntMap a b -> IntMap a c
compose (Mk m1) (Mk m2) = Mk (Data.compose m1 (coerce m2))
