{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
module Newtype.Set(
    Set(), toRawSet, fromRawSet, raw, relax,

    empty, singleton,

    fromList, 
    fromAscList,
    fromDistinctAscList,

    insert, delete, alterF,

    umember, unotMember, member, notMember, membership,

    ulookupLT, ulookupGT, ulookupLE, ulookupGE,
    lookupLT, lookupGT, lookupLE, lookupGE,

    null, size,
    elems, toList, toAscList, toDescList,

    filter, map, mapMaybe,

    lookupIndex, elemAt, deleteAt,
    take, drop,

    union, 
    intersection,
    IntersectionSet(..),
    tightIntersection,
    difference, (\\),
    cartesianProduct,
    disjointUnion,

) where

import Prelude hiding (id, (.), null, lookup, filter, map, take, drop)

import Data.Coerce
import qualified Data.Set as Data
import qualified Data.Maybe (mapMaybe)

import Control.Category
import Data.Type.Coercion.Sub
import Data.Reflection (Given(..), give)

import Newtype.Set.Internal

import Newtype.Utils

fromRawSet :: Data.Set u -> Set u u
fromRawSet = Mk

relax :: Given (Sub k' u) => Sub k k' -> Sub (Set u k) (Set u k')
relax !_ = sub

raw :: Sub (Set u k) (Data.Set u)
raw = sub

-- * Primitive construction

empty :: Given (Sub k u) => Set u k
empty = coerce Data.empty

singleton :: Given (Sub k u) => k -> Set u k
singleton = upcastWith (given /->/ sub) Data.singleton

-- fromSet :: Given (Sub k u) => Newtype.Set u k -> Set u k

-- * Construction from lists

repFromList :: Given (Sub k u) => Sub ([u] -> Data.Set u) ([k] -> Set u k)
repFromList = mapR given /->/ sub

fromList :: (Given (Sub k u), Ord u) => [k] -> Set u k
fromList = upcastWith repFromList Data.fromList

fromAscList :: (Given (Sub k u), Eq u) => [k] -> Set u k
fromAscList = upcastWith repFromList Data.fromAscList

fromDistinctAscList :: (Given (Sub k u)) => [k] -> Set u k
fromDistinctAscList = upcastWith repFromList Data.fromDistinctAscList

-- * Insertion/Deletion

insert :: (Given (Sub k u), Ord u) => k -> Set u k -> Set u k
insert = upcastWith (given /->/ sub) Data.insert

delete :: (Given (Sub k u), Ord u) => k -> Set u k -> Set u k
delete = upcastWith (given /->/ sub) Data.delete

alterF :: (Given (Sub k u), Ord u, Functor f) => (Bool -> f Bool) -> k -> Set u k -> f (Set u k)
alterF = (fmap . fmap . fmap . fmap) coerce . upcastWith rep $ Data.alterF
  where
    rep = id /->/ given /->/ sub /->/ id

-- * Query

umember, unotMember :: (Ord u) => u -> Set u k -> Bool
umember = coerce Data.member
unotMember = coerce Data.member

member, notMember :: (Given (Sub k u), Ord u) => k -> Set u k -> Bool
member = upcastWith (given /->/ id) umember
notMember = upcastWith (given /->/ id) unotMember

membership :: (Given (Sub k u), Ord u) => u -> Set u k -> Maybe k
membership u s = upcastWith (mapR ungiven) $
  if Data.member u (coerce s) then Just u else Nothing

repLookupXX :: (Given (Sub k u))
  => Sub (u -> Data.Set u -> Maybe u)
         (u -> Set u k -> Maybe k)
repLookupXX = id /->/ sub /->/ mapR ungiven

ulookupLT, ulookupGT, ulookupLE, ulookupGE
  :: (Given (Sub k u), Ord u) => u -> Set u k -> Maybe k
ulookupLT = upcastWith repLookupXX Data.lookupLT
ulookupGT = upcastWith repLookupXX Data.lookupGT
ulookupLE = upcastWith repLookupXX Data.lookupLE
ulookupGE = upcastWith repLookupXX Data.lookupGE

lookupLT, lookupGT, lookupLE, lookupGE
  :: (Given (Sub k u), Ord u) => k -> Set u k -> Maybe k
lookupLT = upcastWith (given /->/ id) ulookupLT
lookupGT = upcastWith (given /->/ id) ulookupGT
lookupLE = upcastWith (given /->/ id) ulookupLE
lookupGE = upcastWith (given /->/ id) ulookupGE

-- * Size
null :: Set u k -> Bool
null = coerce Data.null

size :: Set u k -> Int
size = coerce Data.size

-- * Converting to
elems, toAscList, toDescList :: Given (Sub k u) => Set u k -> [k]
elems = upcastWith (sub /->/ mapR ungiven) Data.elems
toAscList = upcastWith (sub /->/ mapR ungiven) Data.toAscList
toDescList = upcastWith (sub /->/ mapR ungiven) Data.toDescList

-- * Filter

filter :: (Given (Sub k u), Ord u) => (k -> Bool) -> Set u k -> Set u k
filter = upcastWith ((ungiven /->/ id) /->/ sub) Data.filter

map :: (Given (Sub k u), Given (Sub k' u), Ord u) => (k -> k') -> Set u k -> Set u k'
map = upcastWith ((ungiven /->/ given) /->/ sub) Data.map

mapMaybe :: (Given (Sub k u), Given (Sub k' u), Ord u) => (k -> Maybe k') -> Set u k -> Set u k'
mapMaybe f s = fromList $ Data.Maybe.mapMaybe f (toList s)

-- * Indexed

lookupIndex :: (Given (Sub k u), Ord u) => k -> Set u k -> Maybe Int
lookupIndex = upcastWith (given /->/ sub) Data.lookupIndex

elemAt :: (Given (Sub k u)) => Int -> Set u k -> k
elemAt = upcastWith (id /->/ sub /->/ ungiven) Data.elemAt

deleteAt :: (Given (Sub k u), Ord u) => Int -> Set u k -> Set u k
deleteAt = coerce Data.deleteAt

take, drop :: (Given (Sub k u), Ord u) => Int -> Set u k -> Set u k
take = coerce Data.take
drop = coerce Data.drop

-- * Combine
union :: (Given (Sub k u), Ord u) => Set u k -> Set u k -> Set u k
union = coerce Data.union

intersection :: (Given (Sub k u), Ord u) => Set u k -> Set u k' -> Set u k
intersection = coerce Data.intersection

data IntersectionSet u k k' where
    IntersectionSet :: Given (Sub k'' u) => Sub k'' k -> Sub k'' k' -> Set u k'' -> IntersectionSet u k k'

tightIntersection :: forall k k' u.
     (Given (Sub k u), Given (Sub k' u), Ord u)
  => Set u k -> Set u k' -> IntersectionSet u k k'
tightIntersection ma mb =
  let mc = Mk (Data.intersection (coerce ma) (coerce mb)) :: Set u u
  in give (id :: Sub u u) (IntersectionSet (ungiven :: Sub u k) (ungiven :: Sub u k') mc)

difference :: (Given (Sub k u), Ord u) => Set u k -> Set u k' -> Set u k
difference = coerce Data.difference

(\\) :: (Given (Sub k u), Ord u) => Set u k -> Set u k -> Set u k
(\\) = difference

infixl 9 \\

cartesianProduct :: Set u a -> Set v b -> Set (u,v) (a,b)
cartesianProduct = coerce Data.cartesianProduct

disjointUnion :: Set u a -> Set v b -> Set (Either u v) (Either a b)
disjointUnion = coerce Data.disjointUnion
