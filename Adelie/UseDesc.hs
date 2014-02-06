-- UseDesc.hs
--
-- Module for parsing portageProfile/use.desc and use.local.desc files.

module Adelie.UseDesc (
  UseDescriptions,
  readUseDesc,
  readUseDescPackage
) where

import Data.Char (isSpace)
import qualified Data.HashTable.IO as HT
import Control.Monad (when)

import Adelie.ListEx
import Adelie.Portage

type UseDescriptions = HT.BasicHashTable String String

----------------------------------------------------------------

readUseDesc :: IO UseDescriptions
readUseDesc = do
  table <- HT.new
  ls <- readFile useDesc
  mapM_ (useParser table) (lines ls)
  return table

useParser :: UseDescriptions -> String -> IO ()
useParser _ ('#':_) = return ()
useParser table line = HT.insert table use desc
  where (use, desc) = myBreak line

myBreak :: String -> (String, String)
myBreak [] = ("", "")
myBreak (' ':'-':' ':xs) = ("", xs)
myBreak (x:xs) = (x:ys, zs)
  where (ys, zs) = myBreak xs

----------------------------------------------------------------

readUseDescPackage :: String -> String -> IO UseDescriptions
readUseDescPackage start end = do
  table <- HT.new
  ls <- readFile useDescPackage
  mapMUntil_ (useParser2 table start end) (lines ls)
  return table

mapMUntil_ :: Monad m => (a -> m Bool) -> [a] -> m ()
mapMUntil_ _ [] = return ()
mapMUntil_ f (x:xs) = do
  r <- f x
  when r (mapMUntil_ f xs)

useParser2 :: UseDescriptions -> String -> String -> String -> IO Bool
useParser2 _ _ _ [] = return True
useParser2 _ _ _ ('#':_) = return True
useParser2 table start end str = do
  case mid start catname end of
      LT -> return True
      EQ -> HT.insert table use desc >> return True
      GT -> return False
  where str' = reverse $ dropWhile isSpace $ reverse str
        (catname, rest) = break2 (':' ==) str'
        (use, desc) = myBreak rest
  
----------------------------------------------------------------

-- In Haskell, vim-core > vim
-- In sort,    vim-core < vim
-- Work around it.
myCompare :: String -> String -> Ordering
myCompare [] [] = EQ
myCompare  _ [] = LT
myCompare []  _ = GT
myCompare (a:as) (b:bs) =
  if r == EQ
    then myCompare as bs 
    else r
  where r = compare a b

mid :: String -> String -> String -> Ordering
mid l m r
  | myCompare l m == GT = LT
  | myCompare m r == GT = GT
  | otherwise = EQ
