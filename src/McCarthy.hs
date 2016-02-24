{-|
Module      : McCarthy
Copyright   : (c) Eric Bailey, 2016
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Chapter 8: McCarthy 91
-}

module McCarthy
       ( mc91
       ) where

-- | The McCarthy 91 function yields @n - 10@ when @n > 100@ and @91@ otherwise.
-- The function is recursive.
-- <<resources/mc91.png The McCarthy 91 function>>
mc91 :: (Num a, Ord a) => a -> a
mc91 n | n > 100   = n - 10
       | otherwise = mc91 . mc91 $ n + 11
