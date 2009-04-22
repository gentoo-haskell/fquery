-- QList.hs
--
-- Module to list the contents of an installed package.

module Adelie.QList (
  ListTypes(..),

  qList
) where

import Adelie.Contents
import Adelie.Portage
import Adelie.Pretty

data ListTypes
  = ListDirs
  | ListFiles
  | ListLinks
  | ListAll
  deriving Eq

---------------------------------------------------------------- 

qList :: ListTypes -> [String] -> IO ()
qList types args = mapM_ (list puts) =<< findInstalledPackages args
  where
    puts = case types of
      ListDirs  -> putsD
      ListFiles -> putsF
      ListLinks -> putsL
      otherwise -> putsA

list puts catname = do
  putStr "Contents of " >> putCatNameLn catname
  readContents puts contents ()
  putChar '\n'
  where contents = contentsFromCatName catname

----------------------------------------------------------------

putsA, putsD, putsF, putsL :: Contents -> () -> IO (Bool, ())

putsA (Obj o _ _) _ = putStrLn o      >> return (False, ())
putsA c _           = putContentsLn c >> return (False, ())

putsD c@(Dir d) _   = putContentsLn c >> return (False, ())
putsD _ _           = return (False, ())

putsF (Obj o _ _) _ = putStrLn o >> return (False, ())
putsF _ _           = return (False, ())

putsL c@(Sym _ _ _)_= putContentsLn c >> return (False, ())
putsL _ _           = return (False, ())
