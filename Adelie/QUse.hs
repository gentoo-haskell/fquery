-- QUse.hs
--
-- Module to describe the use flags of an installed package.

module Adelie.QUse (qUse) where

import qualified Data.HashTable.IO as HT
import qualified Data.List as L
import Control.Monad (unless)

import Adelie.Colour
import Adelie.ListEx
import Adelie.Portage
import Adelie.Pretty
import Adelie.Use
import Adelie.UseDesc

----------------------------------------------------------------

qUse :: [String] -> IO ()
qUse args = qUse' =<< findInstalledPackages args

qUse' :: [(String, String)] -> IO ()
qUse' [] = return ()
qUse' catnames = do
  useDesc' <- readUseDesc
  useDescPackage' <- readUseDescPackage min' max'
  mapM_ (use useDesc' useDescPackage') catnames
  where min' = dropVersion $ fullnameFromCatName $ minimum catnames
        max' = dropVersion $ fullnameFromCatName $ maximum catnames

use :: UseDescriptions -> UseDescriptions -> (String, String) -> IO ()
use useDesc' useDescPackage' catname = do
  iUse <- readIUse fnIUse
  pUse <- readUse  fnPUse
  let iUse' = fmap (\x -> case L.stripPrefix "+" x of
                      Just xs' -> xs'
                      Nothing  -> x) iUse
      len   = maximum $ map length iUse'
  use' catname len useDesc' useDescPackage' iUse' pUse
  where fnIUse = iUseFromCatName catname
        fnPUse = useFromCatName catname

use' :: (String, String) -> Int -> UseDescriptions -> UseDescriptions ->
        [String] -> [String] -> IO ()

use' catname _ _ _ [] _ = putStr "No USE flags for " >> putCatNameLn catname
use' catname len useDesc' useDescPackage' iUse pUse = do
  putStr "USE flags for " >> putCatNameLn catname
  mapM_ (format len useDesc' useDescPackage' pUse) iUse
  putChar '\n'

----------------------------------------------------------------

format :: Int -> UseDescriptions -> UseDescriptions ->
          [String] -> String -> IO ()

format len useDesc' useDescPackage' pUse iUse =
  inst >> putStr (pad len ' ' iUse) >> off >> putStr " : " >> desc
  where
    inst = if iUse `elem` pUse
            then putStr " + " >> red
            else putStr "   " >> blue

    desc = do
      end <- desc' useDescPackage'
      unless end (do
        end' <- desc' useDesc'
        unless end' (putStrLn "<< no description >>"))

    desc' descs = do
      r <- HT.lookup descs iUse
      case r of
        Just d  -> puts d >> return True
        Nothing -> return False

puts :: String -> IO ()
puts d@('!':'!':_) = red >> putStr d >> off2
puts d = putStrLn d
