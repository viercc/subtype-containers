module Digits(
    Digits(), unDigits, parseDigits
) where

import Data.Char (isDigit)

import Data.Type.Coercion.Sub

-- Digits is a String where all characters are digits
newtype Digits = Digits String
  deriving (Show)

-- A Digits is a String, but not the other way around!
unDigits :: Sub Digits String
unDigits = sub

parseDigits :: String -> Maybe Digits
parseDigits str
  | all isDigit str = Just (Digits str)
  | otherwise       = Nothing
