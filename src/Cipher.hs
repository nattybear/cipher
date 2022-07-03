module Cipher where

import Data.Char

caesar :: Int -> Char -> Char
caesar n c = if    'a' <=  c
                &&  c  <= 'z'
                || 'A' <=  c
                &&  c  <= 'Z'
             then chr $ (ord c + n - a) `mod` 26 + a
             else c
  where
    a = if isLower c
        then ord 'a'
        else ord 'A'

uncaesar :: Int -> Char -> Char
uncaesar = caesar . negate

vigenere' :: (Int -> Char -> Char) -> String -> String -> String
vigenere' f keyword message = map (uncurry f) pairs'
  where
    pairs' = map (\(k, m) -> ((ord . toLower) k - 97, m)) pairs

    pairs  = zip' (cycle keyword) message

    zip' _        []     = []
    zip' k@(x:xs) (y:ys) = if y == ' '
                           then (' ', ' ') : zip' k  ys
                           else ( x ,  y ) : zip' xs ys

vigenere :: String -> String -> String
vigenere = vigenere' caesar

unvigenere :: String -> String -> String
unvigenere = vigenere' uncaesar

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
