module Adelie.FileEx where

import Control.Monad
import Data.Char
import Data.List
import System.Directory


type FileExt = String
type Line    = String


-- |Remove all non-directories from a list of FilePaths.
noDirs :: IO [FilePath] -> IO [FilePath]
noDirs ioFps = filterM doesFileExist =<< ioFps


concatPath :: [String] -> String
concatPath = intercalate "/"


addPathPrefix :: FilePath -> IO [FilePath] -> IO [FilePath]
addPathPrefix prefix ioFps = do
  fps <- ioFps
  return (fmap (\x -> concatPath [prefix, x]) fps)


-- |Rm all commentary lines of a file content.
rmComments :: String -> String
rmComments =
  rmTrailingNewline
  . unlines
  . rmCommentsL
  . lines


-- |Same as 'rmComments', but on a list of lines.
rmCommentsL :: [Line] -> [Line]
rmCommentsL = filter (not . isComment)


-- |Rm all blank lines.
rmBlank :: String -> String
rmBlank =
  rmTrailingNewline
  . unlines
  . rmBlankL
  . lines


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
isBlank = null . dropWhile isSpace


-- |Removes a trailing newline.
rmTrailingNewline :: String -> String
rmTrailingNewline [] = []
rmTrailingNewline [x, '\n'] = [x]
rmTrailingNewline (x:xs)    = x : rmTrailingNewline xs
