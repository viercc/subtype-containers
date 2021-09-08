{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
module Newtype.Set(
    Set(), toRawSet, extractSub, relax,

    empty, singleton, fromRawSet,

    fromList, 
    fromAscList,
    fromDistinctAscList,

    insert, delete, alterF,

    member, notMember, membership,

    ulookupLT, ulookupGT, ulookupLE, ulookupGE,
    lookupLT, lookupGT, lookupLE, lookupGE,

    null, size,
    elems, toList, toAscList, toDescList,

    filter, map, mapMaybe,

    lookupIndex, elemAt, deleteAt,
    take, drop,

    union,
    UnionSet(..),
    tightUnion,

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
import Data.Type.Coercion.Sub.Internal
import Data.Type.Coercion

import Newtype.Set.Internal
import Newtype.Union ( IsUnion, greater )
import Newtype.Intersection ( IsIntersection, lesser )

-- * Primitive construction

empty :: Sub k u -> Set u k
empty (Sub Coercion) = Mk Data.empty

singleton :: Sub k u -> k -> Set u k
singleton (Sub Coercion) k = Mk (coerce Data.singleton k)

-- * Construction from lists

fromList :: (Ord u) => Sub k u -> [k] -> Set u k
fromList (Sub Coercion) ks = Mk (coerce Data.fromList ks)

fromAscList :: (Eq u) => Sub k u -> [k] -> Set u k
fromAscList (Sub Coercion) ks = Mk (coerce Data.fromAscList ks)

fromDistinctAscList :: Sub k u -> [k] -> Set u k
fromDistinctAscList (Sub Coercion) ks = Mk (coerce Data.fromDistinctAscList ks)

-- * Insertion/Deletion

insert :: (Ord u) => k -> Set u k -> Set u k
insert k (Mk us) = Mk (coerce Data.insert k us)

delete :: (Ord u) => k -> Set u k -> Set u k
delete k (Mk us) = Mk (coerce Data.delete k us)

alterF :: (Ord u, Functor f) => (Bool -> f Bool) -> k -> Set u k -> f (Set u k)
alterF f k (Mk us) = Mk <$> Data.alterF f (coerce k) us

-- * Query

member, notMember :: (Ord u) => k -> Set u k -> Bool
member k (Mk us) = coerce Data.member k us
notMember k = not . member k

membership :: (Ord u) => u -> Set u k -> Maybe k
membership u (Mk us) = coerce $ if Data.member u us then Just u else Nothing

ulookupLT, ulookupGT, ulookupLE, ulookupGE
  :: (Ord u) => u -> Set u k -> Maybe k
ulookupLT u (Mk us) = coerce Data.lookupLT u us
ulookupLE u (Mk us) = coerce Data.lookupLE u us
ulookupGT u (Mk us) = coerce Data.lookupGT u us
ulookupGE u (Mk us) = coerce Data.lookupGE u us

lookupLT, lookupGT, lookupLE, lookupGE
  :: (Ord u) => k -> Set u k -> Maybe k
lookupLT k (Mk us) = coerce Data.lookupLT k us
lookupLE k (Mk us) = coerce Data.lookupLE k us
lookupGT k (Mk us) = coerce Data.lookupGT k us
lookupGE k (Mk us) = coerce Data.lookupGE k us

-- * Size
null :: Set u k -> Bool
null = Data.null . toRawSet

size :: Set u k -> Int
size = Data.size . toRawSet

-- * Converting to
elems, toList, toDescList :: Set u k -> [k]
elems = toAscList
toList = toAscList
toDescList (Mk us) = coerce $ Data.toDescList us

-- * Filter

filter :: (Ord u) => (k -> Bool) -> Set u k -> Set u k
filter f (Mk us) = Mk (Data.filter (coerce f) us)

map :: (Ord u') => Sub k' u' -> (k -> k') -> Set u k -> Set u' k'
map (Sub Coercion) f (Mk us) = Mk (Data.map (coerce f) us)

mapMaybe :: (Ord u') => Sub k' u' -> (k -> Maybe k') -> Set u k -> Set u' k'
mapMaybe k'u' f = fromList k'u' . Data.Maybe.mapMaybe f . toList

-- * Indexed

lookupIndex :: (Ord u) => u -> Set u k -> Maybe Int
lookupIndex u (Mk us) = Data.lookupIndex u us

elemAt :: Int -> Set u k -> k
elemAt i (Mk us) = coerce $ Data.elemAt i us

deleteAt :: (Ord u) => Int -> Set u k -> Set u k
deleteAt i (Mk us) = Mk (Data.deleteAt i us)

take, drop :: (Ord u) => Int -> Set u k -> Set u k
take i (Mk us) = Mk (Data.take i us)
drop i (Mk us) = Mk (Data.drop i us)

-- * Combine
union :: (Ord u) => Set u k -> Set u k -> Set u k
union (Mk xs) (Mk ys) = Mk (Data.union xs ys)

data UnionSet u x y where
    UnionSet :: IsUnion x y z -> Set u z -> UnionSet u k k'

tightUnion :: forall u x y. (Ord u) => Set u x -> Set u y -> UnionSet u x y
tightUnion (Mk xs) (Mk ys) =
  let zs = Mk (Data.union xs ys) :: Set u y
  in UnionSet (greater sub :: IsUnion x y y) zs

intersection :: (Ord u) => Set u k -> Set u k' -> Set u k
intersection (Mk xs) (Mk ys) = Mk (Data.intersection xs ys)

data IntersectionSet u x y where
    IntersectionSet :: IsIntersection x y z -> Set u z -> IntersectionSet u x y

tightIntersection :: forall x y u.
     (Ord u) => Set u x -> Set u y -> IntersectionSet u x y
tightIntersection (Mk xs) (Mk ys) =
  let zs = Mk (Data.intersection xs ys) :: Set u x
  in IntersectionSet (lesser sub :: IsIntersection x y x) zs

difference :: (Ord u) => Set u k -> Set u k' -> Set u k
difference (Mk xs) (Mk ys) = Mk (xs Data.\\ ys)

(\\) :: (Ord u) => Set u k -> Set u k -> Set u k
(\\) = difference

infixl 9 \\

cartesianProduct :: Set u a -> Set v b -> Set (u,v) (a,b)
cartesianProduct (Mk us) (Mk vs) = Mk (Data.cartesianProduct us vs)

disjointUnion :: Set u a -> Set v b -> Set (Either u v) (Either a b)
disjointUnion (Mk us) (Mk vs) = Mk (Data.disjointUnion us vs)
