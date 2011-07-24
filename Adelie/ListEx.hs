-- ListEx.hs
--
-- Extra list functions.

module Adelie.ListEx (
  break2,
  concatMapM,
  digitsToInt,
  dropTail,
  dropUntilAfter,
  foldMUntil,
  pad
) where

import Data.Char (digitToInt)
import Data.List (foldl')
import Control.Monad (liftM)

----------------------------------------------------------------

break2 :: (a -> Bool) -> [a] -> ([a], [a])
break2 f l = (h, drop 1 t)
  where (h, t) = break f l

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat $ mapM f xs

digitsToInt :: String -> Int
digitsToInt = foldl' (\ a b -> a*10 + digitToInt b) 0

dropTail :: Int -> [a] -> [a]
dropTail n s = take (length s-n) s

dropUntilAfter :: (a -> Bool) -> [a] -> [a]
dropUntilAfter f = dropWhile f . dropWhile (not.f)

foldMUntil :: Monad m => (a -> b -> m a) -> (a -> Bool) -> a -> [b] -> m a
foldMUntil _ _ a [] = return a
foldMUntil f g a (x:xs) = do
  a' <- f a x
  case g a' of
    True  -> return a'
    False -> foldMUntil f g a' xs

pad :: Int -> a -> [a] -> [a]
pad n a str = take n (str ++ repeat a)
