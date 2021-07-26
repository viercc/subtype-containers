module Main where

import Prelude hiding (id, (.))
import Control.Category
import Data.Reflection
import Data.Type.Coercion.Sub

import Newtype.Set (Set)
import qualified Newtype.Set as Set

import Digits

data1, data2, data3 :: [String]
data1 = [ "000" ]
data2 = [ "000", "123" ]
data3 = [ "000", "456", "abc" ]

main :: IO ()
main = give (id :: Sub String String) $ give unDigits $
  do let set1 = Set.fromList data1 :: Set String String
         set1D = Set.mapMaybe parseDigits set1
     print set1
     print set1D
     return ()
