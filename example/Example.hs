module Main where

import Prelude hiding (id, (.))
import Control.Category

import Newtype.Set (Set)
import qualified Newtype.Set as Set

import Digits


data1, data2, data3 :: Set String String
data1 = Set.fromList id [ "00", "12" ]
data2 = Set.fromList id [ "00", "34" ]
data3 = Set.fromList id [ "12", "56", "abc" ]

-- Convert to Set of Digits. Note that Digits doesn't
-- have @Ord Digits@ instance and @Ord String@ instance
-- is used to construct a Set of Digits.
onlyDigits :: Set String String -> Set String Digits
onlyDigits = Set.mapMaybe unDigits parseDigits

data1', data2' :: Set String Digits
data1' = onlyDigits data1
data2' = onlyDigits data2

main :: IO ()
main =
  do print data1
     print data1'
     print (Set.union data1' data2')
     -- print (Set.union data1' data3) -- type error!
     print (Set.intersection data1' data3)
     return ()
