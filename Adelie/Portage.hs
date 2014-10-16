-- Portage.hs
--
-- Portage directories and file paths.

module Adelie.Portage (
  portageTree,
  portageDB,
  useDesc,
  useDescPackage,
  useExpDescDir,

  dropVersion,
  concatPath,
  fullnameFromCatName,
  allInstalledPackages,
  findInstalledPackages
) where

import Data.Char         (isDigit)
import System.Directory  (getDirectoryContents, doesDirectoryExist)
import Data.List         (sort)
import Control.Monad     (liftM)

import Adelie.FileEx
import Adelie.ListEx
import Adelie.Config

-- Skips non-directory entries from returned list
-- (symlinks to world file in our case)
-- racy actually
safeGetDirectoryContents :: FilePath -> IO [FilePath]
safeGetDirectoryContents dir =
    do yet <- doesDirectoryExist dir
       if yet then getDirectoryContents dir
              else return []

portageProfiles :: String
portageProfiles = portageTree ++ "/profiles"

-- Where the global use flag descriptions are, 'use.desc'.
useDesc :: String
useDesc = portageProfiles ++ "/use.desc"

-- Where the package specific use flag descriptions are, 'use.local.desc'.
useDescPackage :: String
useDescPackage = portageProfiles ++ "/use.local.desc"

-- Where the USE Expand descriptions are.
useExpDescDir :: String
useExpDescDir = portageProfiles ++ "/desc"

----------------------------------------------------------------

dropVersion :: String -> String
dropVersion [] = []
dropVersion ('-':x:xs)
  | isDigit x = []
  | otherwise = '-':x:(dropVersion xs)
dropVersion (x:xs) = x:(dropVersion xs)

fullnameFromCatName :: (String, String) -> String
fullnameFromCatName (cat, name) = cat ++ '/':name

----------------------------------------------------------------

allInstalledPackages :: IO [(String, String)]
allInstalledPackages = do
  cats <- liftM (sort.filter filterHidden) (safeGetDirectoryContents portageDB)
  concatMapM allInstalledPackagesInCategory cats

allInstalledPackagesInCategory :: String -> IO [(String, String)]
allInstalledPackagesInCategory cat = do
  names <- liftM (sort.filter filterHidden) (safeGetDirectoryContents path)
  return $ map (\ a -> (cat, a)) names
  where path = portageDB ++ '/':cat

----------------------------------------------------------------

findInstalledPackages :: [String] -> IO [(String, String)]
findInstalledPackages names = concatMapM findInstPackages names

findInstPackages :: String -> IO [(String, String)]
findInstPackages name =
  if null b
    then findInstPackages' a
    else findInstPackagesInCategory a b
  where (a, b) = break2 (== '/') name

findInstPackages' :: String -> IO [(String, String)]
findInstPackages' pack = do
  cats  <- liftM (sort.filter filterHidden) (safeGetDirectoryContents portageDB)
  concatMapM (flip findInstPackagesInCategory pack) cats

findInstPackagesInCategory :: String -> String -> IO [(String, String)]
findInstPackagesInCategory cat pack = do
  packs <- liftM (sort.filter cond) (safeGetDirectoryContents (portageDB++'/':cat))
  return (zip (repeat cat) packs)
  where
    cond ('.':_) = False
    cond p = (pack == p) || (pack == dropVersion p)

----------------------------------------------------------------
filterHidden :: String -> Bool
filterHidden ('.':_) = False
filterHidden _ = True
