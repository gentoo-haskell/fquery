-- QOwn.hs
--
-- Module to query who owns a particular file.

module Adelie.QOwn (
  qOwn,
  qOwnRegex
) where

import Data.List (delete)
import Control.Monad (when)
import Text.Regex (Regex, matchRegex, mkRegex)

import Adelie.Contents
import Adelie.ListEx
import Adelie.Portage
import Adelie.Pretty

----------------------------------------------------------------

qOwn :: [String] -> IO ()
qOwn [] = return ()
qOwn args = do
  foldMUntil qOwn' null args =<< allInstalledPackages
  putChar '\n'

qOwn' :: [String] -> (String, String) -> IO [String]
qOwn' files catname = readContents (puts catname) contents files
  where contents = contentsFromCatName catname

----------------------------------------------------------------

puts :: (String, String) -> Contents -> [String] -> IO (Bool, [String])
puts catname c fs =
  if c `contentsElem` fs
    then do
      let fs' = deleteContent c fs
      putCatName catname >> putStr " (" >> putContents c >> putStrLn ")"
      return (null fs', fs')
    else
      return (False, fs)

contentsElem :: Contents -> [String] -> Bool
(Dir d)     `contentsElem` fs = d `elem` fs
(Obj o _ _) `contentsElem` fs = o `elem` fs
(Sym l _ _) `contentsElem` fs = l `elem` fs

-- Only remove objects from the list.
deleteContent :: Contents -> [String] -> [String]
deleteContent (Obj o _ _) fs = delete o fs
deleteContent _ fs = fs

----------------------------------------------------------------

qOwnRegex :: [String] -> IO ()
qOwnRegex args = mapM_ (qOwnRegex' pats) =<< allInstalledPackages
  where pats = map mkRegex args

qOwnRegex' :: [Regex] -> (String, String) -> IO ()
qOwnRegex' pats catname = readContents (putsRegex catname pats) contents ()
  where contents = contentsFromCatName catname

putsRegex :: (String, String) -> [Regex] -> Contents -> () -> IO (Bool, ())
putsRegex catname pats c _ = do
  when (c `regexElem` pats) match
  return (False, ())
  where match = putCatName catname >> putStr " (" >> putContents c>>putStrLn ")"

regexElem :: Contents -> [Regex] -> Bool
(Dir d)     `regexElem` pats = mapMaybeOnce (flip matchRegex d) pats
(Obj o _ _) `regexElem` pats = mapMaybeOnce (flip matchRegex o) pats
(Sym l _ _) `regexElem` pats = mapMaybeOnce (flip matchRegex l) pats

mapMaybeOnce :: (a -> Maybe b) -> [a] -> Bool
mapMaybeOnce _ [] = False
mapMaybeOnce f (x:xs) =
  case f x of 
    Just _a  -> True
    Nothing -> mapMaybeOnce f xs
