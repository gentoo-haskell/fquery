#!/usr/bin/runhugs -98
-- Main.hs
--
-- Adelie is a collection of scripts to querying portage packages. 

module Main (main) where

import System

import Adelie.Colour
import Adelie.QChangelog
import Adelie.QCheck
import Adelie.QDepend
import Adelie.QHasUse
import Adelie.QList
import Adelie.QOwn
import Adelie.QSize
import Adelie.QUse
import Adelie.QWant

data Command
  = Short String String ([String] -> IO ())

logCommands = [
  (Short "c"  "find the changelog of a package"     qChangelog)
  ]

listCommands = [
  (Short "f"  "list the contents of a package"      (qList ListAll)),
  (Short "fd" "list the directories in a package"   (qList ListDirs)),
  (Short "ff" "list the files in a package"         (qList ListFiles)),
  (Short "fl" "list the links in a package"         (qList ListLinks))
  ]

ownCommands = [
  (Short "b"  "find the package(s) owning a file"                   qOwn),
  (Short "bp" "find the package(s) owning a file with regexp"       qOwnRegex),
  (Short "s"  "find the size of files in a package"                 qSize),
  (Short "k"  "check MD5sums and timestamps of a package"           qCheck)
  ]

dependCommands = [
  (Short "d"  "list packages directly depending on this package"    qDepend),
  (Short "dd" "list direct dependencies of a package"               qWant)
  ]

useCommands = [
  (Short "u"  "describe a package's USE flags"      qUse),
  (Short "h"  "list all packages with a USE flag"   qHasUse)
  ]

allCommands =
  -- All packages
  logCommands ++ 
  -- Installed packages only
  listCommands ++ ownCommands ++ dependCommands ++ useCommands

----------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    (cmd:cargs) -> main' cmd cargs allCommands

main' :: String -> [String] -> [Command] -> IO ()
main' _ _ [] = usage
main' command args ((Short cmd _ f):xs)
  | command == cmd  = f args
  | otherwise       = main' command args xs

----------------------------------------------------------------

usage :: IO ()
usage = do
  putStrLn "Adelie v0.1\n"
  putStrLn "Usage: adelie <command> <arguments>\n"

  cyan >> putStr "Commands for Installed Packages:" >> off2
  mapM_ putCommand logCommands; nl
  mapM_ putCommand listCommands; nl
  mapM_ putCommand ownCommands; nl
  mapM_ putCommand dependCommands; nl
  mapM_ putCommand useCommands; nl

putCommand :: Command -> IO ()
putCommand (Short cmd desc _) =
  putStr "    " >> green >> putStr cmd >> off >> tab >> putStrLn desc

tab = putChar '\t'
nl  = putChar '\n'
