module Cipher where

import Data.Char

caesar :: Int -> Char -> Char
caesar n c
  | isLetter c = chr $ (ord c + n - a) `mod` 26 + a
  | otherwise  = c
  where a = if isLower c then ord 'a' else ord 'A'

uncaesar :: Int -> Char -> Char
uncaesar = caesar . negate

vigenere :: String -> String -> String
vigenere keyword message = map (uncurry caesar) pairs'
  where
    pairs' = map (\(k, m) -> ((ord . toLower) k - 97, m)) pairs

    pairs  = zip' (cycle keyword) message

    zip' _        []     = []
    zip' k@(x:xs) (y:ys) = if y == ' '
                           then (' ', ' ') : zip' k  ys
                           else ( x ,  y ) : zip' xs ys

caesarIO :: IO Char
caesarIO = do
  n   <- read <$> getLine
  [c] <- getLine
  return $ caesar n c

vigenereIO :: IO String
vigenereIO = do
  keyword <- getLine
  message <- getLine
  return $ vigenere keyword message
