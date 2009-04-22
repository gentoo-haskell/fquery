-- QDepend.hs
--
-- Module to list packages depending on an installed package.

module Adelie.QDepend (qDepend) where

import Adelie.Colour
import Adelie.CompareVersion
import Adelie.Depend
import Adelie.ListEx
import Adelie.Portage
import Adelie.Pretty
import Adelie.Provide
import Adelie.Use

----------------------------------------------------------------

qDepend :: [String] -> IO ()
qDepend [] = return ()
qDepend args = qDepend' =<< findInstalledPackages args

qDepend' :: [(String, String)] -> IO ()
qDepend' catnames = do
  allPacks <- allInstalledPackages
  let allPacks2 = map fullnameFromCatName allPacks
  mapM_ (dep allPacks2) catnames

dep :: [String] -> (String, String) -> IO ()
dep allPacks catname = do
  putStr "Packages depending on " >> putCatNameLn catname
  provide <- readProvide fnProvide
  mapM_ (dep' (fullname:provide)) allPacks
  putChar '\n'
  where fullname = fullnameFromCatName catname
        fnProvide = provideFromCatName catname

dep' :: [String] -> String -> IO ()
dep' provided fullname =
  readUse fnIUse >>= readDepend fnDepend >>= puts fullname provided
  where fnDepend = concatPath [portageDB,fullname,"RDEPEND"]
        fnIUse = concatPath [portageDB,fullname,"USE"]

puts :: String -> [String] -> [Dependency] -> IO ()
puts str provided iWant = mapM_ print perms
  where perms = [ (p, w) | p <- provided, w <- iWant, w `satisfiedBy` p ]
        print (p, w) =
          white >> putStr (pad 32 ' ' str) >> off >>
          putStr "\t( " >> putDependency w >> putStrLn " )"

-------------------------------------------------------------

breakVersion :: String -> (String, String)
breakVersion str = (n, v)
  where n = dropVersion str
        v = drop (length n+1) str

satisfiedBy :: Dependency -> String -> Bool

(GreaterEqual wantName wantVer) `satisfiedBy` provided =
  (wantName == provName) && (compareVersion provVer wantVer) /= LT
  where (provName, provVer) = breakVersion provided

(Equal wantName wantVer) `satisfiedBy` provided = 
  (wantName == provName) && (compareVersion provVer wantVer) == EQ
  where (provName, provVer) = breakVersion provided

(Unstable wantName wantVer) `satisfiedBy` provided =
  (wantName == provName) && (compareVersion provVer wantVer) == EQ
  where (provName, provVer) = breakVersion provided

(NotInstalled _) `satisfiedBy` _ = False

(Any wantName) `satisfiedBy` provided =
  wantName == provName
  where provName = dropVersion provided