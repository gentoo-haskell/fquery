module Adelie.FileEx where

import Adelie.ListEx
import Control.Monad
import Data.Char
import Data.List
import System.Directory


type FileExt = String
type Line    = String


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


-- |Rm all commentary lines of a file content.
rmComments :: String -> String
rmComments xs =
  rmTrailingNewline  .
  unlines            .
  rmCommentsL        .
  lines              $
  xs


-- |Same as 'rmComments', but on a list of lines.
rmCommentsL :: [Line] -> [Line]
rmCommentsL = filter (not . isComment)


-- |Rm all blank lines.
rmBlank :: String -> String
rmBlank xs =
  rmTrailingNewline  .
  unlines            .
  rmBlankL           .
  lines              $
  xs


-- |Same as 'rmBlank', but on a list of lines.
rmBlankL :: [Line] -> [Line]
rmBlankL = filter (not . isBlank)


-- |Whether the line is a comment.
isComment :: Line -> Bool
isComment = iC' . dropWhile isSpace
  where
    iC' ('#':_) = True
    iC' _       = False


-- |Whether the line is blank (only spaces or tabs).
isBlank :: Line -> Bool
isBlank = iB' . dropWhile isSpace
  where
    iB' [] = True
    iB' _  = False


-- |Removes a trailing newline.
rmTrailingNewline :: String -> String
rmTrailingNewline [] = []
rmTrailingNewline xs = case last xs of
  '\n' -> init xs
  _    -> xs
