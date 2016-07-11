module CipherTests where

import Test.QuickCheck
import Cipher

---------------------
-- Validating Ciphers
---------------------

testCaesarIdentity :: IO ()
testCaesarIdentity = quickCheck (\x -> x == (caesar 5 . unCaesar 5) x)

keyword = "Hello"

sanitize = unwords . words

-- The first version of vigenere is based on `words` and `unwords`
-- which end up collapsing white spaces in a `words . unwords` roundtrip.
-- For this reason I `sanitize x` before comparing it to the result.

testVigenereIdentity :: IO ()
testVigenereIdentity = quickCheck (\x -> sanitize x == (vigenere keyword . unVigenere keyword) x )

testVigenereIdentity' :: IO ()
testVigenereIdentity' = quickCheck (\x -> x == (vigenere' keyword . unVigenere' keyword) x )
