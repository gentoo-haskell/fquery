-- Use.hs
--
-- Module for parsing USE and IUSE, files, located in
-- portageDB/category/package/.

module Adelie.Use (
  useFromCatName,
  iUseFromCatName,
  readUse,
  readIUse
) where

import List   (nub, sort)
import Monad  (liftM)

import Adelie.Portage

----------------------------------------------------------------

useFromCatName :: (String, String) -> String
useFromCatName (cat, name) = concatPath [portageDB,cat,name,"USE"]

iUseFromCatName :: (String, String) -> String
iUseFromCatName (cat, name) = concatPath [portageDB,cat,name,"IUSE"]

----------------------------------------------------------------

readUse :: FilePath -> IO [String]
readUse fn = (liftM words $ readFile fn) `catch` (\ _ -> return [])

-- IUSE files sometimes have duplicate USE flags.  I am not sure if it is the
-- intended behaviour, but I filter them out.
readIUse :: FilePath -> IO [String]
readIUse fn = (liftM (nub.sort.words) $ readFile fn) `catch` (\ _ -> return [])
