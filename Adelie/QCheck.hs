-- QMD5sum.hs
--
-- Check MD5sums and timestamps of installed packages.

module Adelie.QCheck (qCheck) where

import Char               (isHexDigit)
import IO
import Monad              (unless)
import System
import System.Process     (runProcess, waitForProcess)
import System.Posix.Files (getFileStatus)
import System.Posix.IO    (createPipe, fdToHandle)

import Adelie.Colour
import Adelie.Contents
import Adelie.Portage
import Adelie.Pretty

-- Good, Bad
type Count = (Int, Int)

----------------------------------------------------------------

qCheck :: [String] -> IO ()
qCheck [] = return ()
qCheck args = mapM_ check =<< findInstalledPackages args

check :: (String, String) -> IO ()
check catname = do
  putStr "Checking " >> putCatNameLn catname
  (g, b) <- readContents check' contents (0, 0)
  putNum g >> putStr " out of " >> putNum (b+g) >> putStrLn " files good"
  putChar '\n'
  where contents = contentsFromCatName catname

check' :: Contents -> Count -> IO (Bool, Count)

check' (Dir _) (g, b) = return (False, (g+1, b))

check' (Obj o m _) (g, b) = do
  r <- try (getFileStatus o)
  case r of
    Left e -> do
      red >> putStr "!!! " >> off >> putStr o >> putStrLn " does not exist"
      return (False, (g, b+1))
    Right stat -> do
      (rd, wr) <- createPipeHandle
      runMD5sum o (Just wr) >>= waitForProcess
      ln <- hGetLine rd
      hClose rd
      hClose wr
      if m == (takeWhile isHexDigit ln)
        then return (False, (g+1, b))
        else putMD5error o >> return (False, (g, b+1))

check' (Sym _ _ _) (g, b) = return (False, (g+1, b))

----------------------------------------------------------------

createPipeHandle :: IO (Handle, Handle)
createPipeHandle = do
  (read, write) <- createPipe
  hRead  <- fdToHandle read
  hWrite <- fdToHandle write
  return (hRead, hWrite)

----------------------------------------------------------------

runMD5sum f stdout = runProcess md5sum [f] Nothing Nothing stdin stdout stderr
  where md5sum = "/usr/bin/md5sum"
        stdin  = Nothing
        stderr = Nothing

putMD5error :: String -> IO ()
putMD5error file =
  red >> putStr "!!! " >> off >> putStrLn (file ++ " has incorrect md5sum")
