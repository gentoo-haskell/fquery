-- QSize.hs
--
-- Check the disk usage of an installed packaged.

module Adelie.QSize (qSize) where

import Directory            (doesDirectoryExist)
import IO                   (isDoesNotExistError, try)
import Monad                (mapM_, when)
import System.Posix.Files   (fileSize, getFileStatus)
import System.Posix.Types   (FileOffset)

import Adelie.Colour
import Adelie.Contents
import Adelie.Portage
import Adelie.Pretty

-- Size, Dir, Obj, Sym, Inaccessible
type Count = (FileOffset, Int, Int, Int, Int)

----------------------------------------------------------------

qSize :: [String] -> IO ()
qSize [] = return ()
qSize args = mapM_ size =<< findInstalledPackages args

size :: (String, String) -> IO ()
size catname = do
  putStr "Size of " >> putCatNameLn catname
  (size, dir, obj, sym, err) <- readContents count contents (0, 0, 0, 0, 0)
  putStr "    Directories: " >> putNumLn dir
  putStr "          Files: " >> putNumLn obj

  when (sym > 0) (putStr "          Links: " >> putNumLn sym)
  when (err > 0) (putStr "   Inaccessible: " >> putNumLn err)

  putStr "     Total size: " >> putSizeLn size
  putChar '\n'
  where contents = contentsFromCatName catname

----------------------------------------------------------------

count :: Contents -> Count -> IO (Bool, Count)

count c@(Dir d) (s,w,x,y,z) = do
  r <- doesDirectoryExist d
  if r
    then return (False, (s,w+1,x,y,z))
    else return (False, (s,w,x,y,z+1)) 

count c@(Obj o _ _) (s,w,x,y,z) = do
  r <- try (getFileStatus o)
  case r of
    Left err -> return (False, (s, w, x, y, z+1))
    Right st -> return (False, (s+fileSize st, w, x+1, y, z))

count (Sym _ _ _) (s,w,x,y,z) = return (False, (s,w,x,y+1,z))

----------------------------------------------------------------

putSizeLn :: FileOffset -> IO ()
putSizeLn n = cyan >> putStr (show (n `div` 1024)) >> putStr " kb" >> off2
