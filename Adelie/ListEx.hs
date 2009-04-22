-- ListEx.hs
--
-- Extra list functions.

module Adelie.ListEx (
  break2,
  concatMapM,
  digitsToInt,
  dropTail,
  pad
) where

import Char   (digitToInt)
import Monad  (liftM)

----------------------------------------------------------------

break2 :: (a -> Bool) -> [a] -> ([a], [a])
break2 f l = (h, drop 1 t)
  where (h, t) = break f l

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat $ mapM f xs

digitsToInt :: String -> Int
digitsToInt = foldl (\ a b -> a*10 + digitToInt b) 0

dropTail :: Int -> [a] -> [a]
dropTail n s = take (length s-n) s

pad :: Int -> a -> [a] -> [a]
pad n a str = str ++ (replicate (n-length str) a)
