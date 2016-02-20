{-|
Module      : Cipher
Copyright   : (c) Eric Bailey, 2016
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Chapter 9: Cipher
-}

module Cipher
       ( caesar
       , unCaesar
       , encrypt
       , decrypt
       ) where

import           Control.Arrow (Arrow (..), arr, second, (&&&), (***), (>>>))
import           Data.Char     (chr, isAsciiLower, isAsciiUpper, ord)
import           Data.Function (on)

caesar :: Int -> String -> String
caesar = map . encrypt

unCaesar :: Int -> String -> String
unCaesar = map . encrypt . negate

encrypt :: Int -> Char -> Char
encrypt n c = maybe c f (ord' c)
-- encrypt n = flip maybe f `ap` ord'
  where f :: (Char, Int) -> Char
        f = second (n +) >>> arr chr'

decrypt :: Int -> Char -> Char
decrypt = encrypt . negate

ord' :: Char -> Maybe (Char, Int)
ord' c | isAsciiUpper c = Just $ go 'A'
       | isAsciiLower c = Just $ go 'a'
       | otherwise      = Nothing
  where go :: Char -> (Char, Int)
        go = id &&& (c `charMinus`)

chr' :: (Char, Int) -> Char
chr' = ord *** (`mod` 26) >>> addChr
  where addChr :: Arrow a => a (Int, Int) Char
        addChr = arr (chr . uncurry (+))
-- chr' (c, n) | isLetter' c = chr $ ord c + (n `mod` 26)
--             | otherwise   = undefined
--
-- isLetter' :: Char -> Bool
-- isLetter' = isAsciiUpper &&& isAsciiLower >>> arr (uncurry (||))

charMinus :: Char -> Char -> Int
charMinus = (-) `on` ord
