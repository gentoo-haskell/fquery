-- QWant.hs
--
-- List the direct dependencies of a file.

module Adelie.QWant (qWant) where

import Adelie.Depend
import Adelie.Portage
import Adelie.Pretty
import Adelie.Use

-------------------------------------------------------------

qWant :: [String] -> IO ()
qWant args = mapM_ qWant' =<< findInstalledPackages args

qWant' :: (String, String) -> IO ()
qWant' catname = do
  putStr "Dependencies for " >> putCatNameLn catname
  readUse fnIUse >>= readDepend fnDepend >>= mapM_ puts
  putChar '\n'
  where fnDepend = dependFromCatName catname
        fnIUse = useFromCatName catname

puts :: Dependency -> IO ()
puts a = putDependency a >> putChar '\n'
