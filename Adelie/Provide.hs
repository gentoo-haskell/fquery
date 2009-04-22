-- Provide.hs
--
-- Module for parsing PROVIDE files, located in portageDB/category/package/.

module Adelie.Provide (
  provideFromCatName,
  readProvide
) where

import Monad (liftM)

import Adelie.Portage

----------------------------------------------------------------

provideFromCatName :: (String, String) -> String
provideFromCatName (cat,name) = concatPath [portageDB,cat,name,"PROVIDE"]

----------------------------------------------------------------

readProvide :: FilePath -> IO [String]
readProvide fn = (liftM words (readFile fn)) `catch` (\ _ -> return [])
