{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
module Newtype.Map(
    Map(), toRawMap, fromRawMap, raw, relaxKey,

    empty, singleton, fromSet,

    fromList, fromListWith, fromListWithKey,
    fromListWithKey_safe,
    fromAscList,
    fromDistinctAscList,

    insert, insertWith, insertWithKey,
    insertWithKey_safe,
    insertLookupWithKey,

    delete, adjust, update, updateLookupWithKey,
    alter, alterF,

    ulookup, ulookupAndKey,
    lookup, (!?), (!),
    findWithDefault,
    membership, umember, unotMember, member, notMember,

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
import Data.Reflection (Given(..), give)
import Data.Maybe (fromMaybe)
import qualified Data.Foldable as F

import Newtype.Set (Set)
import qualified Newtype.Set.Internal as Set

import Newtype.Map.Internal
import Newtype.Utils

fromRawMap :: Data.Map u a -> Map u u a
fromRawMap = Mk

relaxKey :: Given (Sub k' u) => Sub k k' -> Sub (Map u k a) (Map u k' a)
relaxKey !_ = sub

raw :: Sub (Map u k a) (Data.Map u a)
raw = sub

-- * Primitive construction

empty :: Given (Sub k u) => Map u k a
empty = coerce1 Data.empty

singleton :: Given (Sub k u) => k -> a -> Map u k a
singleton = upcastWith (given /->/ id /->/ rep1) Data.singleton

fromSet :: Given (Sub k u) => (k -> a) -> Set u k -> Map u k a
fromSet = upcastWith ((ungiven /->/ id) /->/ sub /->/ rep1) Data.fromSet

-- * Construction from lists

repFromList :: Given (Sub k u) => Sub ([(u,a)] -> Data.Map u a) ([(k,a)] -> Map u k a)
repFromList = mapR (bimapR given id) /->/ sub

fromList :: (Given (Sub k u), Ord u) => [(k,a)] -> Map u k a
fromList = upcastWith repFromList Data.fromList

fromListWith :: (Given (Sub k u), Ord u) => (a -> a -> a) -> [(k,a)] -> Map u k a
fromListWith = upcastWith (mapR repFromList) Data.fromListWith

fromListWithKey :: (Given (Sub k u), Ord u)
  => (k -> a -> a -> a) -> [(k,a)] -> Map u k a
fromListWithKey = upcastWith ((ungiven /->/ id) /->/ repFromList) Data.fromListWithKey

-- | The usage of unsafe 'ungiven' in 'fromListWithKey' is OK
fromListWithKey_safe :: (Given (Sub k u), Ord u)
  => (k -> a -> a -> a) -> [(k,a)] -> Map u k a
fromListWithKey_safe f kas = fst <$> fromListWith g (listened <$> kas)
  where
    g (x,op) (y,_) = (op x y, op)
    listened (k,a) = (k, (a,f k))

fromAscList :: (Given (Sub k u), Eq u) => [(k,a)] -> Map u k a
fromAscList = upcastWith repFromList Data.fromAscList

fromDistinctAscList :: (Given (Sub k u)) => [(k,a)] -> Map u k a
fromDistinctAscList = upcastWith repFromList Data.fromDistinctAscList

-- * Insertion
repInsert :: Given (Sub k u)
  => Sub (u -> a -> Data.Map u a -> Data.Map u a)
         (k -> a -> Map u k a -> Map u k a)
repInsert = given /->/ sub

insert :: (Given (Sub k u), Ord u) => k -> a -> Map u k a -> Map u k a
insert = upcastWith repInsert Data.insert

insertWith :: (Given (Sub k u), Ord u)
  => (a -> a -> a) -> k -> a -> Map u k a -> Map u k a
insertWith = upcastWith (id /->/ repInsert) Data.insertWith

insertWithKey :: (Given (Sub k u), Ord u)
  => (k -> a -> a -> a) -> k -> a -> Map u k a -> Map u k a
insertWithKey = upcastWith ((ungiven /->/ id) /->/ repInsert) Data.insertWithKey

-- | The usage of unsafe 'ungiven' in 'fromListWithKey' is OK
insertWithKey_safe :: (Given (Sub k u), Ord u)
  => (k -> a -> a -> a) -> k -> a -> Map u k a -> Map u k a
insertWithKey_safe f k = insertWith (f k) k

insertLookupWithKey :: (Given (Sub k u), Ord u)
  => (k -> a -> a -> a) -> k -> a -> Map u k a -> (Maybe a, Map u k a)
insertLookupWithKey = upcastWith rep Data.insertLookupWithKey
  where
    rep = (ungiven /->/ id) /->/ given /->/ sub

-- * Deletion/Update

repDelete :: (Given (Sub k u), Ord u)
  => Sub (u -> Data.Map u a -> Data.Map u a)
         (k -> Map u k a -> Map u k a)
repDelete = given /->/ sub

delete :: (Given (Sub k u), Ord u) => k -> Map u k a -> Map u k a
delete = upcastWith repDelete Data.delete

adjust :: (Given (Sub k u), Ord u) => (a -> a) -> k -> Map u k a -> Map u k a
adjust = upcastWith (id /->/ repDelete) Data.adjust

update :: (Given (Sub k u), Ord u) => (a -> Maybe a) -> k -> Map u k a -> Map u k a
update = upcastWith (id /->/ repDelete) Data.update

updateLookupWithKey :: (Given (Sub k u), Ord u)
 => (k -> a -> Maybe a) -> k -> Map u k a -> (Maybe a, Map u k a)
updateLookupWithKey = upcastWith rep Data.updateLookupWithKey
  where
    rep = (ungiven /->/ id) /->/ given /->/ sub

alter :: (Given (Sub k u), Ord u) => (Maybe a -> Maybe a) -> k -> Map u k a -> Map u k a
alter = upcastWith (id /->/ repDelete) Data.alter

alterF :: (Given (Sub k u), Ord u, Functor f)
  => (Maybe a -> f (Maybe a)) -> k -> Map u k a -> f (Map u k a)
alterF = (fmap . fmap . fmap . fmap) coerce1 . upcastWith rep $ Data.alterF
  where
    rep = id /->/ given /->/ rep1 /->/ id

-- * Query

lookup :: (Given (Sub k u), Ord u) => k -> Map u k a -> Maybe a
lookup = upcastWith (given /->/ id) ulookup

ulookup :: (Ord u) => u -> Map u k a -> Maybe a
ulookup = upcastWith (id /->/ rep1 /->/ id) Data.lookup

ulookupAndKey :: (Given (Sub k u), Ord u) => u -> Map u k a -> Maybe (k, a)
ulookupAndKey u ma = upcastWith (mapR (bimapR ungiven id)) $ (,) u <$> Data.lookup u (coerce1 ma)

(!?) :: (Given (Sub k u), Ord u) => Map u k a -> k -> Maybe a
(!?) = flip lookup

(!) :: (Given (Sub k u), Ord u) => Map u k a -> k -> a
(!) m k = fromMaybe (error "element not in the Map") $ lookup k m

findWithDefault :: (Given (Sub k u), Ord u) => a -> k -> Map u k a -> a
findWithDefault a k m = fromMaybe a $ lookup k m

membership :: (Given (Sub k u), Ord u) => u -> Map u k a -> Maybe k
membership u ma = upcastWith (mapR ungiven) $
  if Data.member u (coerce1 ma) then Just u else Nothing

umember, unotMember :: (Ord u) => u -> Map u k a -> Bool
umember = upcastWith (id /->/ rep1 /->/ id) Data.member
unotMember = upcastWith (id /->/ rep1 /->/ id) Data.member

member, notMember :: (Given (Sub k u), Ord u) => k -> Map u k a -> Bool
member = upcastWith (given /->/ id) umember
notMember = upcastWith (given /->/ id) unotMember

repLookupXX :: (Given (Sub k u))
  => Sub (u -> Data.Map u a -> Maybe (u,a))
         (u -> Map u k a -> Maybe (k,a))
repLookupXX = mapR (rep1 /->/ mapR (bimapR ungiven id))

ulookupLT, ulookupGT, ulookupLE, ulookupGE
  :: (Given (Sub k u), Ord u) => u -> Map u k a -> Maybe (k,a)
ulookupLT = upcastWith repLookupXX Data.lookupLT
ulookupGT = upcastWith repLookupXX Data.lookupGT
ulookupLE = upcastWith repLookupXX Data.lookupLE
ulookupGE = upcastWith repLookupXX Data.lookupGE

lookupLT, lookupGT, lookupLE, lookupGE
  :: (Given (Sub k u), Ord u) => k -> Map u k a -> Maybe (k,a)
lookupLT = upcastWith (given /->/ id) ulookupLT
lookupGT = upcastWith (given /->/ id) ulookupGT
lookupLE = upcastWith (given /->/ id) ulookupLE
lookupGE = upcastWith (given /->/ id) ulookupGE

-- * Size
null :: Map u k a -> Bool
null = upcastWith (rep1 /->/ id) Data.null

size :: Map u k a -> Int
size = upcastWith (rep1 /->/ id) Data.size

-- * Conversion

elems :: Map u k a -> [a]
elems = F.toList

keys :: (Given (Sub k u)) => Map u k a -> [k]
keys = upcastWith (rep1 /->/ mapR ungiven) Data.keys

assocsRep :: Given (Sub k u)
  => Sub (Data.Map u a -> [(u,a)])
         (Map u k a -> [(k,a)])
assocsRep = rep1 /->/ mapR (bimapR ungiven id)

assocs :: Given (Sub k u) => Map u k a -> [(k,a)]
assocs = upcastWith assocsRep Data.assocs

keysSet :: Given (Sub k u) => Map u k a -> Set u k
keysSet = upcastWith (rep1 /->/ sub) Data.keysSet

toList :: Given (Sub k u) => Map u k a -> [(k,a)]
toList = assocs

toAscList :: Given (Sub k u) => Map u k a -> [(k,a)]
toAscList = upcastWith assocsRep Data.toAscList

toDescList :: Given (Sub k u) => Map u k a -> [(k,a)]
toDescList = upcastWith assocsRep Data.toDescList

-- * Traversal/Map
map :: (a -> b) -> Map u k a -> Map u k b
map = fmap

mapWithKey :: (Given (Sub k u)) => (k -> a -> b) -> Map u k a -> Map u k b
mapWithKey = upcastWith ((ungiven /->/ id) /->/ rep1 /->/ rep1) Data.mapWithKey

traverseWithKey :: (Given (Sub k u), Applicative f) => (k -> a -> f b) -> Map u k a -> f (Map u k b)
traverseWithKey = (fmap . fmap . fmap) coerce1 . upcastWith ((ungiven /->/ id) /->/ rep1 /->/ id) $
  Data.traverseWithKey

repMapKeys :: (Given (Sub k u), Given (Sub k' u))
  => Sub ((u -> u) -> Data.Map u a -> Data.Map u a)
         ((k -> k') -> Map u k a -> Map u k' a)
repMapKeys = (ungiven /->/ given) /->/ rep1 /->/ rep1

mapKeys :: (Given (Sub k u), Given (Sub k' u), Ord u) => (k -> k') -> Map u k a -> Map u k' a
mapKeys = upcastWith repMapKeys Data.mapKeys

mapKeysWith :: (Given (Sub k u), Given (Sub k' u), Ord u) => (a -> a -> a) -> (k -> k') -> Map u k a -> Map u k' a
mapKeysWith = upcastWith (id /->/ repMapKeys) Data.mapKeysWith

-- * Filtering

filter :: Ord u => (a -> Bool) -> Map u k a -> Map u k a
filter = upcastWith (id /->/ rep1 /->/ rep1) Data.filter

filterWithKey :: (Given (Sub k u), Ord u) => (k -> a -> Bool) -> Map u k a -> Map u k a
filterWithKey = upcastWith ((ungiven /->/ id) /->/ rep1 /->/ rep1) Data.filterWithKey

mapMaybe :: Ord u => (a -> Maybe b) -> Map u k a -> Map u k b
mapMaybe = upcastWith (id /->/ rep1 /->/ rep1) Data.mapMaybe

mapMaybeWithKey :: (Given (Sub k u), Ord u) => (k -> a -> Maybe b) -> Map u k a -> Map u k b
mapMaybeWithKey = upcastWith ((ungiven /->/ id) /->/ rep1 /->/ rep1) Data.mapMaybeWithKey

restrictKeysBy :: (Given (Sub k u), Ord u) => Map u k a -> Set u k' -> Map u k a
restrictKeysBy = upcastWith (rep1 /->/ sub /->/ rep1) Data.restrictKeys

restrictKeysTo :: (Given (Sub k' u), Ord u) => Map u k a -> Set u k' -> Map u k' a
restrictKeysTo = upcastWith (rep1 /->/ sub /->/ rep1) Data.restrictKeys

tightRestrictKeys :: forall k k' u a.
  (Given (Sub k u), Given (Sub k' u), Ord u)
  => Map u k a -> Set u k' -> IntersectionMap u k k' a
tightRestrictKeys m s =
    let m' = Mk (Data.restrictKeys (coerce1 m) (coerce s)) :: Map u u a
    in give (id :: Sub u u) (IntersectionMap (ungiven :: Sub u k) (ungiven :: Sub u k') m')

withoutKeys :: (Given (Sub k u), Ord u) => Map u k a -> Set u k' -> Map u k a
withoutKeys = upcastWith (rep1 /->/ sub /->/ rep1) Data.withoutKeys

-- * Combine
union :: (Given (Sub k u), Ord u) => Map u k a -> Map u k a -> Map u k a
union = upcastWith (rep1 /->/ rep1 /->/ rep1) Data.union

unionWith :: (Given (Sub k u), Ord u) => (a -> a -> a) -> Map u k a -> Map u k a -> Map u k a
unionWith = upcastWith (id /->/ rep1 /->/ rep1 /->/ rep1) Data.unionWith

intersection :: (Given (Sub k u), Ord u) => Map u k a -> Map u k' b -> Map u k a
intersection = upcastWith (rep1 /->/ rep1 /->/ rep1) Data.intersection

intersectionWith :: (Given (Sub k u), Ord u) => (a -> b -> c) -> Map u k a -> Map u k' b -> Map u k c
intersectionWith = upcastWith (id /->/ rep1 /->/ rep1 /->/ rep1) Data.intersectionWith

data IntersectionMap u k k' a where
    IntersectionMap :: Given (Sub k'' u) => Sub k'' k -> Sub k'' k' -> Map u k'' a -> IntersectionMap u k k' a

tightIntersectionWith :: forall k k' u a b c.
     (Given (Sub k u), Given (Sub k' u), Ord u)
  => (a -> b -> c) -> Map u k a -> Map u k' b -> IntersectionMap u k k' c
tightIntersectionWith op ma mb =
  let mc = Mk (Data.intersectionWith op (coerce1 ma) (coerce1 mb)) :: Map u u c
  in give (id :: Sub u u) (IntersectionMap (ungiven :: Sub u k) (ungiven :: Sub u k') mc)

difference :: (Given (Sub k u), Ord u) => Map u k a -> Map u k' b -> Map u k a
difference = upcastWith (rep1 /->/ rep1 /->/ rep1) Data.difference

(\\) :: (Given (Sub k u), Ord u) => Map u k a -> Map u k b -> Map u k a
(\\) = difference

differenceWith :: (Given (Sub k u), Ord u) => (a -> b -> Maybe a) -> Map u k a -> Map u k' b -> Map u k a
differenceWith = upcastWith (id /->/ rep1 /->/ rep1 /->/ rep1) Data.differenceWith

infixl 9 \\

-- * Composition

compose :: (Given (Sub a u), Given (Sub b v), Ord v) => Map v b c -> Map u a b -> Map u a c
compose = upcastWith (rep1 /->/ mapR given . rep1 /->/ rep1) Data.compose
