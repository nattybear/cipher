module Main where

import Cipher
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "ciphers return\
           \ the same data after\
           \ encoding and decoding\
           \ a string" $ do
    it "Caesar" $ do
      property $ \x -> x == uncaesar 3 (caesar 3 x)
    it "Vigenere" $ do
      property $ \x -> x == unvigenere "ALLY" (vigenere "ALLY" x)
