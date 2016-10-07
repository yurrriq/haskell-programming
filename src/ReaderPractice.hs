{-|
Module      : ReaderPractice
Copyright   : (c) Eric Bailey, 2016
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Chapter 22: Reader Practice
-}

module ReaderPractice where

import Control.Applicative
import Data.Maybe

x, y, z :: [Integer]
x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

zs :: Maybe Integer
zs = lookup 4 $ zip x y

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  let s' = summed <$> ((,) <$> xs <*> ys)
  print $ s'
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  print $ foldr1 (&&) (sequA 4)
  print $ sequA (fromMaybe 0 s')
  print $ bolt (fromMaybe 0 ys)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]
