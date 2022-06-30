module Cipher where

import Data.Char

caesar :: Int -> Char -> Char
caesar n c
  | isLetter c = chr $ (ord c + n - a) `mod` 26 + a
  | otherwise  = c
  where a = if isLower c then ord 'a' else ord 'A'

uncaesar :: Int -> Char -> Char
uncaesar = caesar . negate
