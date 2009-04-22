-- Contents.hs
--
-- Module for parsing CONTENTS files, located in portageDB/category/package/.

module Adelie.Contents (
  Contents(..),

  contentsFromCatName,
  putContents,
  putContentsLn,
  readContents
) where

import Char (isDigit, isHexDigit, isSpace)
import IO

import Adelie.Colour
import Adelie.ListEx
import Adelie.Portage

data Contents
  = Dir String
  | Obj String String Int
  | Sym String String Int
  deriving Show

----------------------------------------------------------------

contentsFromCatName :: (String, String) -> String
contentsFromCatName (cat, name) = concatPath [portageDB,cat,name,"CONTENTS"]

----------------------------------------------------------------

readContents :: (Contents -> a -> IO (Bool, a)) -> FilePath -> a -> IO a
readContents f fn a = do
  r <- try read
  case r of
    Left  _  -> return a
    Right a' -> return a'
  where
    read = (bracket (openFile fn ReadMode)
                    hClose
                    (readContents' f a))

readContents' :: (Contents -> a -> IO (Bool, a)) -> a -> Handle -> IO a
readContents' f a fp = do
  eof <- hIsEOF fp
  if eof
    then return a
    else do
      ln <- hGetLine fp
      (done, a') <- f (contentsParser ln) a
      if done
        then return a'
        else readContents' f a' fp

----------------------------------------------------------------

putContents, putContentsLn :: Contents -> IO ()
putContents   = putContents' off
putContentsLn = putContents' off2

putContents' :: IO () -> Contents -> IO ()
putContents' f (Dir d)     = blue  >> putStr d >> f
putContents' f (Obj o _ _) = white >> putStr o >> f
putContents' f (Sym l t _) = cyan  >> putStr (l ++ " -> " ++ t) >> f

----------------------------------------------------------------

contentsParser :: String -> Contents
contentsParser ('d':'i':'r':' ':dir) = (Dir dir)

contentsParser ('o':'b':'j':' ':ln0) = (Obj obj md5 time)
  where ln1 = dropWhile isSpace $ reverse ln0
        (time', ln2) = break2 (not.isDigit) ln1
        (md5', obj') = break2 (not.isHexDigit) ln2
        obj  = reverse obj'
        md5  = reverse md5'
        time = digitsToInt (reverse time')

contentsParser ('s':'y':'m':' ':ln0) = (Sym link target time)
  where (link, ln1) = breakLink ln0
        ln2 = reverse ln1
        (time', target') = break2 (not.isDigit) ln2
        target = reverse target'
        time = digitsToInt (reverse time')

breakLink (' ':'-':'>':' ':xs) = ([], xs)
breakLink (x:xs) = (x:as, bs)
  where (as, bs) = breakLink xs
