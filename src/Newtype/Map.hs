{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
module Newtype.Map(
    Map(), toRawMap, extractSub, fromRawMap, relaxKey,

    empty, singleton, fromSet,

    fromList, fromListWith, fromListWithKey,
    fromAscList,
    fromDistinctAscList,

    insert, insertWith, insertWithKey,
    insertLookupWithKey,

    delete, adjust, update, updateLookupWithKey,
    alter, alterF,

    ulookup, ulookupAndKey,
    lookup, (!?), (!),
    findWithDefault,
    membership, member, notMember,

    ulookupLT, ulookupGT, ulookupLE, ulookupGE,
    lookupLT, lookupGT, lookupLE, lookupGE,

    null, size,

    elems, keys, assocs,
    keysSet,
    toList, toAscList, toDescList,

    map, mapWithKey, traverseWithKey,
    mapKeys, mapKeysWith,

    filter, filterWithKey,
    mapMaybe, mapMaybeWithKey,
    restrictKeysBy, restrictKeysTo, tightRestrictKeys, withoutKeys,

    union, unionWith,
    UnionMap(..),
    tightUnionWith,
    
    intersection,
    intersectionWith,
    IntersectionMap(..),
    tightIntersectionWith,
    difference, (\\), differenceWith,

    compose
) where

import Prelude hiding (id, (.), null, map, filter, lookup)

import Data.Coerce
import qualified Data.Map.Lazy as Data

import Control.Category
import Data.Type.Coercion.Sub
import Data.Type.Coercion.Sub.Internal ( Sub(Sub) )
import Data.Type.Coercion ( Coercion(Coercion) )
import Data.Maybe (fromMaybe)
import qualified Data.Foldable as F

import Newtype.Set (Set)
import qualified Newtype.Set.Internal as Set

import Newtype.Map.Internal
import Newtype.Union ( IsUnion, greater )
import Newtype.Intersection ( IsIntersection, lesser )

-- * Primitive construction

empty :: Sub k u -> Map u k a
empty (Sub Coercion) = Mk Data.empty

singleton :: Sub k u -> k -> a -> Map u k a
singleton (Sub Coercion) k a = Mk (Data.singleton (coerce k) a)

fromSet :: (k -> a) -> Set u k -> Map u k a
fromSet f (Set.Mk us) = Mk (Data.fromSet (coerce f) us)

-- * Construction from lists

fromList :: (Ord u) => Sub k u -> [(k,a)] -> Map u k a
fromList (Sub Coercion) kas = Mk (Data.fromList (coerce kas))

fromListWith :: (Ord u) => Sub k u -> (a -> a -> a) -> [(k,a)] -> Map u k a
fromListWith (Sub Coercion) op kas = Mk (Data.fromListWith op (coerce kas))

fromListWithKey :: (Ord u)
  => Sub k u -> (k -> a -> a -> a) -> [(k,a)] -> Map u k a
fromListWithKey (Sub Coercion) op kas = Mk (Data.fromListWithKey (coerce op) (coerce kas))

fromAscList :: (Eq u) => Sub k u -> [(k,a)] -> Map u k a
fromAscList (Sub Coercion) kas = Mk (Data.fromAscList (coerce kas))

fromDistinctAscList :: Sub k u -> [(k,a)] -> Map u k a
fromDistinctAscList (Sub Coercion) kas = Mk (Data.fromDistinctAscList (coerce kas))

-- * Insertion

insert :: (Ord u) => k -> a -> Map u k a -> Map u k a
insert k a (Mk ma) = Mk (Data.insert (coerce k) a ma)

insertWith :: (Ord u)
  => (a -> a -> a) -> k -> a -> Map u k a -> Map u k a
insertWith op k a (Mk ma) = Mk (Data.insertWith op (coerce k) a ma)

insertWithKey :: (Ord u)
  => (k -> a -> a -> a) -> k -> a -> Map u k a -> Map u k a
insertWithKey op k a (Mk ma) = Mk (Data.insertWithKey (coerce op) (coerce k) a ma)

insertLookupWithKey :: (Ord u)
  => (k -> a -> a -> a) -> k -> a -> Map u k a -> (Maybe a, Map u k a)
insertLookupWithKey op k a (Mk ma) = case Data.insertLookupWithKey (coerce op) (coerce k) a ma of
  ~(resp, ma') -> (resp, Mk ma')

-- * Deletion/Update

delete :: (Ord u) => k -> Map u k a -> Map u k a
delete k (Mk ma) = Mk (Data.delete (coerce k) ma)

adjust :: (Ord u) => (a -> a) -> k -> Map u k a -> Map u k a
adjust f k (Mk ma) = Mk (Data.adjust f (coerce k) ma)

update :: (Ord u) => (a -> Maybe a) -> k -> Map u k a -> Map u k a
update f k (Mk ma) = Mk (Data.update f (coerce k) ma)

updateLookupWithKey :: (Ord u)
 => (k -> a -> Maybe a) -> k -> Map u k a -> (Maybe a, Map u k a)
updateLookupWithKey f k (Mk ma) = case Data.updateLookupWithKey (coerce f) (coerce k) ma of
  ~(resp, ma') -> (resp, Mk ma')

alter :: (Ord u) => (Maybe a -> Maybe a) -> k -> Map u k a -> Map u k a
alter f k (Mk ma) = Mk (Data.alter f (coerce k) ma)

alterF :: (Ord u, Functor f)
  => (Maybe a -> f (Maybe a)) -> k -> Map u k a -> f (Map u k a)
alterF f k (Mk ma) = Mk <$> Data.alterF f (coerce k) ma

-- * Query

lookup :: (Ord u) => k -> Map u k a -> Maybe a
lookup k (Mk ma) = Data.lookup (coerce k) ma

ulookup :: (Ord u) => u -> Map u k a -> Maybe a
ulookup u (Mk ma) = Data.lookup u ma

ulookupAndKey :: (Ord u) => u -> Map u k a -> Maybe (k, a)
ulookupAndKey u (Mk ma) = (coerce u, ) <$> Data.lookup u ma

(!?) :: (Ord u) => Map u k a -> k -> Maybe a
(!?) = flip lookup

(!) :: (Ord u) => Map u k a -> k -> a
(!) m k = fromMaybe (error "element not in the Map") $ lookup k m

findWithDefault :: (Ord u) => a -> k -> Map u k a -> a
findWithDefault a k m = fromMaybe a $ lookup k m

membership :: (Ord u) => u -> Map u k a -> Maybe k
membership u (Mk ma) = coerce $ if Data.member u ma then Just u else Nothing

member, notMember :: (Ord u) => k -> Map u k a -> Bool
member k (Mk ma) = Data.member (coerce k) ma
notMember k = not . member k

ulookupLT, ulookupGT, ulookupLE, ulookupGE
  :: (Ord u) => u -> Map u k a -> Maybe (k,a)
ulookupLT u (Mk ma) = coerce $ Data.lookupLT u ma
ulookupGT u (Mk ma) = coerce $ Data.lookupGT u ma
ulookupLE u (Mk ma) = coerce $ Data.lookupLE u ma
ulookupGE u (Mk ma) = coerce $ Data.lookupGE u ma

lookupLT, lookupGT, lookupLE, lookupGE
  :: (Ord u) => k -> Map u k a -> Maybe (k,a)
lookupLT k (Mk ma) = coerce $ Data.lookupLT (coerce k) ma
lookupGT k (Mk ma) = coerce $ Data.lookupGT (coerce k) ma
lookupLE k (Mk ma) = coerce $ Data.lookupLE (coerce k) ma
lookupGE k (Mk ma) = coerce $ Data.lookupGE (coerce k) ma

-- * Size
null :: Map u k a -> Bool
null = Data.null . toRawMap

size :: Map u k a -> Int
size = Data.size . toRawMap

-- * Conversion

elems :: Map u k a -> [a]
elems = F.toList

keys :: Map u k a -> [k]
keys (Mk ma) = coerce $ Data.keys ma

assocs :: Map u k a -> [(k,a)]
assocs = toAscList

keysSet :: Map u k a -> Set u k
keysSet (Mk ma) = Set.Mk (Data.keysSet ma)

toList :: Map u k a -> [(k,a)]
toList = toAscList

toDescList :: Map u k a -> [(k,a)]
toDescList (Mk ma) = coerce $ Data.toDescList ma

-- * Traversal/Map
map :: (a -> b) -> Map u k a -> Map u k b
map = fmap

mapWithKey :: (k -> a -> b) -> Map u k a -> Map u k b
mapWithKey f (Mk ma) = Mk (Data.mapWithKey (coerce f) ma)

traverseWithKey :: (Applicative f) => (k -> a -> f b) -> Map u k a -> f (Map u k b)
traverseWithKey f (Mk ma) = Mk <$> Data.traverseWithKey (coerce f) ma

mapKeys :: (Ord u') => Sub u' k' -> (k -> k') -> Map u k a -> Map u' k' a
mapKeys (Sub Coercion) f (Mk ma) = Mk (Data.mapKeys (coerce f) ma)

mapKeysWith :: (Ord u') => Sub u' k' -> (a -> a -> a) -> (k -> k') -> Map u k a -> Map u' k' a
mapKeysWith (Sub Coercion) op f (Mk ma) = Mk (Data.mapKeysWith op (coerce f) ma)

-- * Filtering

filter :: Ord u => (a -> Bool) -> Map u k a -> Map u k a
filter f (Mk ma) = Mk (Data.filter f ma)

filterWithKey :: (Ord u) => (k -> a -> Bool) -> Map u k a -> Map u k a
filterWithKey f (Mk ma) = Mk (Data.filterWithKey (coerce f) ma)

mapMaybe :: Ord u => (a -> Maybe b) -> Map u k a -> Map u k b
mapMaybe f (Mk ma) = Mk (Data.mapMaybe f ma)

mapMaybeWithKey :: (Ord u) => (k -> a -> Maybe b) -> Map u k a -> Map u k b
mapMaybeWithKey f (Mk ma) = Mk (Data.mapMaybeWithKey (coerce f) ma)

restrictKeysBy :: (Ord u) => Map u k a -> Set u k' -> Map u k a
restrictKeysBy (Mk ma) (Set.Mk us) = Mk (Data.restrictKeys ma us)

restrictKeysTo :: (Ord u) => Map u k a -> Set u k' -> Map u k' a
restrictKeysTo (Mk ma) (Set.Mk us) = Mk (Data.restrictKeys ma us)

tightRestrictKeys :: forall x y u a.
  (Ord u)
  => Map u x a -> Set u y -> IntersectionMap u x y a
tightRestrictKeys (Mk ma) (Set.Mk us) =
    let ma' = Mk (Data.restrictKeys ma us) :: Map u x a
    in IntersectionMap (lesser sub :: IsIntersection x y x) ma'

withoutKeys :: (Ord u) => Map u k a -> Set u k' -> Map u k a
withoutKeys (Mk ma) (Set.Mk us) = Mk (Data.withoutKeys ma us)

-- * Combine
union :: (Ord u) => Map u k a -> Map u k a -> Map u k a
union (Mk m1) (Mk m2) = Mk (Data.union m1 m2)

unionWith :: (Ord u) => (a -> a -> a) -> Map u k a -> Map u k a -> Map u k a
unionWith op (Mk m1) (Mk m2) = Mk (Data.unionWith op m1 m2)

data UnionMap u x y a where
    UnionMap :: IsUnion x y z -> Map u z a -> UnionMap u x y a

tightUnionWith :: forall u x y a. (Ord u)
  => (a -> a -> a) -> Map u x a -> Map u y a -> UnionMap u x y a
tightUnionWith op (Mk mx) (Mk my) =
  let mz = Mk (Data.unionWith op mx my) :: Map u y a
  in UnionMap (greater sub :: IsUnion x y y) mz

intersection :: (Ord u) => Map u k a -> Map u k' b -> Map u k a
intersection (Mk m1) (Mk m2) = Mk (Data.intersection m1 m2)

intersectionWith :: (Ord u) => (a -> b -> c) -> Map u k a -> Map u k' b -> Map u k c
intersectionWith op (Mk m1) (Mk m2) = Mk (Data.intersectionWith op m1 m2)

data IntersectionMap u x y a where
    IntersectionMap :: IsIntersection x y z -> Map u z a -> IntersectionMap u x y a

tightIntersectionWith :: forall x y u a b c.
     (Ord u)
  => (a -> b -> c) -> Map u x a -> Map u y b -> IntersectionMap u x y c
tightIntersectionWith op (Mk mx) (Mk my) =
  let mz = Mk (Data.intersectionWith op mx my) :: Map u x c
  in IntersectionMap (lesser sub :: IsIntersection x y x) mz

difference :: (Ord u) => Map u k a -> Map u k' b -> Map u k a
difference (Mk m1) (Mk m2) = Mk (Data.difference m1 m2)

(\\) :: (Ord u) => Map u k a -> Map u k b -> Map u k a
(\\) = difference

differenceWith :: (Ord u) => (a -> b -> Maybe a) -> Map u k a -> Map u k' b -> Map u k a
differenceWith f (Mk m1) (Mk m2) = Mk (Data.differenceWith f m1 m2)

infixl 9 \\

-- * Composition

compose :: (Ord v) => Map v b c -> Map u a b -> Map u a c
compose (Mk m1) (Mk m2) = Mk (Data.compose m1 (coerce m2))
