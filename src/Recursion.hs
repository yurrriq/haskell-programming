{-|
Module      : Recursion
Copyright   : (c) Eric Bailey, 2016
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Chapter 8: Recursion
-}

module Recursion
       ( mc91
       ) where

-- | The McCarthy 91 function yields @n - 10@ when @n > 100@ and @91@ otherwise.
-- The function is recursive.
-- <<resources/m91.png The McCarthy 91 function>>
--
-- @
-- MC(n) = \\begin{cases}
--           n - 10         & \text{if}\\ n > 100   \\\\
--           MC(MC(n + 11)) & \text{if}\\ n \\le 100 \\\\
--         \\end{cases}
-- @
mc91 :: (Num a, Ord a) => a -> a
mc91 = undefined
