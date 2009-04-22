-- Portage.hs
--
-- Portage directories and file paths.

module Adelie.Portage (
  portageTree,
  portageDB,
  useDesc,
  useDescPackage,

  dropVersion,
  concatPath,
  fullnameFromCatName,
  allInstalledPackages,
  findInstalledPackages
) where

import Char       (isDigit)
import Directory  (getDirectoryContents)
import List       (intersperse, sort)
import Monad      (liftM)

import Adelie.ListEx

portageTree :: String
portageTree = "/usr/portage"

-- Where portage's database is located.
portageDB :: String
portageDB = "/var/db/pkg"

portageProfiles :: String
portageProfiles = portageTree ++ "/profiles"

-- Where the global use flag descriptions are, 'use.desc'.
useDesc :: String
useDesc = portageProfiles ++ "/use.desc"

-- Where the package specific use flag descriptions are, 'use.local.desc'.
useDescPackage :: String
useDescPackage = portageProfiles ++ "/use.local.desc"

----------------------------------------------------------------

dropVersion :: String -> String
dropVersion [] = []
dropVersion ('-':x:xs)
  | isDigit x = []
  | otherwise = '-':x:(dropVersion xs)
dropVersion (x:xs) = x:(dropVersion xs)

concatPath :: [String] -> String
concatPath = concat.intersperse "/"

fullnameFromCatName :: (String, String) -> String
fullnameFromCatName (cat, name) = cat ++ '/':name

----------------------------------------------------------------

allInstalledPackages :: IO [(String, String)]
allInstalledPackages = do
  cats <- liftM (sort.filter filterHidden) (getDirectoryContents portageDB)
  concatMapM allInstalledPackagesInCategory cats

allInstalledPackagesInCategory :: String -> IO [(String, String)]
allInstalledPackagesInCategory cat = do
  names <- liftM (sort.filter filterHidden) (getDirectoryContents path)
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

findInstPackages' pack = do
  cats  <- liftM (sort.filter filterHidden) (getDirectoryContents portageDB)
  concatMapM (flip findInstPackagesInCategory pack) cats

findInstPackagesInCategory cat pack = do
  packs <- liftM (sort.filter cond) (getDirectoryContents (portageDB++'/':cat))
  return (zip (repeat cat) packs)
  where
    cond ('.':_) = False
    cond p = (pack == p) || (pack == dropVersion p)

----------------------------------------------------------------

filterHidden ('.':_) = False
filterHidden _ = True
