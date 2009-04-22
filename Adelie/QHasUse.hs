-- QHasUse.hs
--
-- Module to list all installed packages with a particular use flag.

module Adelie.QHasUse (qHasUse) where

import Monad (when)

import Adelie.Colour
import Adelie.Portage
import Adelie.Pretty
import Adelie.Use

----------------------------------------------------------------

qHasUse :: [String] -> IO ()
qHasUse [] = return ()
qHasUse args = qHasUse' args =<< allInstalledPackages 

qHasUse' :: [String] -> [(String, String)] -> IO ()
qHasUse' uses catnames = mapM_ (hasUse catnames) uses

hasUse :: [(String, String)] -> String -> IO ()
hasUse catnames use = do
  putStr "Packages installed with " >> putUse use >> putStrLn " USE flag"
  mapM_ (flip hasUse' use) catnames
  putChar '\n'

hasUse' :: (String, String) -> String -> IO ()
hasUse' catname use = do
  iUse <- readIUse fnIUse
  when (use `elem` iUse) (putCatNameLn catname)
  where fnIUse = iUseFromCatName catname

----------------------------------------------------------------

putUse :: String -> IO ()
putUse u = blue >> putStr u >> off
