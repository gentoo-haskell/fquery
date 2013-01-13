-- Provide.hs
--
-- Module for parsing PROVIDE files, located in portageDB/category/package/.

module Adelie.Provide (
  provideFromCatName,
  readProvide
) where

import Control.Monad (liftM)

import Adelie.Portage
import qualified Adelie.Util as E

----------------------------------------------------------------

provideFromCatName :: (String, String) -> String
provideFromCatName (cat,name) = concatPath [portageDB,cat,name,"PROVIDE"]

----------------------------------------------------------------

readProvide :: FilePath -> IO [String]
readProvide fn = (liftM words (readFile fn)) `E.catchIOE` (\ _ -> return [])
