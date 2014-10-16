module Adelie.FileEx where

import Adelie.ListEx
import Control.Monad
import Data.Char
import Data.List
import System.Directory


type FileExt = String


-- |Converts '/home/user/foo.png' to 'foo.png'
basename :: FilePath -> FilePath
basename xs = case splitBy (== '/') xs of
  [] -> []
  xs' -> last xs'


-- |Remove all non-directories from a list of FilePaths.
noDirs :: IO [FilePath] -> IO [FilePath]
noDirs ioFps = do
  fps <- ioFps
  filterM doesFileExist fps


concatPath :: [String] -> String
concatPath = intercalate "/"


addPathPrefix :: FilePath -> IO [FilePath] -> IO [FilePath]
addPathPrefix prefix ioFps = do
  fps <- ioFps
  return (fmap (\x -> concatPath [prefix, x]) fps)


-- |Compare the extension of a file with the given String.
cmpExt :: FileExt -> FilePath -> Bool
cmpExt checkExt = (==) checkExt . getExt


-- |Get the extension of a file.
getExt :: FilePath -> FileExt
getExt fp
  | hasExt fp = last .
    splitBy (== '.') .
    last             .
    splitBy (== '/') $
    fp
  | otherwise = ""


-- |Check if the file has an extension.
hasExt :: FilePath -> Bool
hasExt = (>1) . length . splitBy (== '.')


-- |Return the FilePath without the extension.
noExt :: FilePath -> FilePath
noExt fp
  | hasExt fp = take (length fp - (length . getExt $ fp) - 1) fp
  | otherwise = fp


-- |Filter all comments and empty lines of a file content.
filterComments :: String -> String
filterComments xs =
  unlines . fmap (fc' . dropWhile isSpace) . lines $ xs
  where
    fc' []      = ""
    fc' ('#':_) = ""
    fc' xs'     = xs'
