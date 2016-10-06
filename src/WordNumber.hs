{-|
Module      : WordNumber
Copyright   : (c) Eric Bailey, 2016
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Chapter 8: WordNumber
-}

module WordNumber
       ( digitToWord
       , digits
       , wordNumber
       ) where

import           Control.Arrow (arr, (***), (>>>))
import           Data.List     (intercalate)

-- | Given a digit, return its corresponding English word.
digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "Invalid digit"

-- | Given an integer, return a list of its digits.
digits :: Int -> [Int]
digits n | n >= 10    = go $ n `divMod` 10
         | otherwise = [n]
  where go :: (Int, Int) -> [Int]
        go = digits *** (:[]) >>> arr uncurry (++)

-- | Given an integer, return a string composed of the name of each digit,
-- separated by hyphens.
wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits
