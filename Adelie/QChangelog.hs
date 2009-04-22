-- QChangelog.hs
--
-- Module to find the changelog of a package.

module Adelie.QChangelog (
  qChangelog
) where

import Adelie.Portage

----------------------------------------------------------------

qChangelog :: [String] -> IO ()
qChangelog [] = return ()
qChangelog args = mapM_ changelog =<< findInstalledPackages args

changelog :: (String, String) -> IO ()
changelog (cat, name) = do
  putStrLn $ portageTree ++ fullname ++ "/ChangeLog"
  where fullname = '/':cat ++ '/':(dropVersion name)
