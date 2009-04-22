-- Main.hs
--
-- Adelie is a collection of scripts to querying portage packages. 

module Main (main) where

import System (getArgs, getProgName)

import Adelie.Colour
import Adelie.Options
import Adelie.QChangelog
import Adelie.QCheck
import Adelie.QDepend
import Adelie.QHasUse
import Adelie.QList
import Adelie.QOwn
import Adelie.QSize
import Adelie.QUse
import Adelie.QWant

type CommandProc = [String] -> IO ()

data Command
  = Short String String CommandProc
  | Long  String String String CommandProc

logCommands = [
  (Long   "c" "changes"
          "list changes since the installed version"
          qChangelog),
  (Short  "cl"
          "find the changelog of a package"
          qLogFile)
  ]

listCommands = [
  (Long   "f" "files"
          "list the contents of a package"
          (qList ListAll)),
  (Short  "fd"
          "list the directories in a package"
          (qList ListDirs)),
  (Short  "ff"
          "list the files in a package"
          (qList ListFiles)),
  (Short  "fl"
          "list the links in a package"
          (qList ListLinks))
  ]

ownCommands = [
  (Long   "b" "belongs"
          "find the package(s) owning a file"
          qOwn),
  (Short  "bp"
          "find the package(s) owning a file with regexp"
          qOwnRegex),
  (Long   "s" "size"
          "find the size of files in a package"
          qSize),
  (Long   "k" "check"
          "check MD5sums and timestamps of a package"
          qCheck)
  ]

dependCommands = [
  (Long   "d" "depends"
          "list packages directly depending on this package"
          qDepend),
  (Short  "dd"
          "list direct dependencies of a package"
          qWant)
  ]

useCommands = [
  (Long   "u" "uses"
          "describe a package's USE flags"
          qUse),
  (Long   "h" "hasuse"
          "list all packages with a USE flag"
          qHasUse)
  ]

allCommands = logCommands ++ listCommands ++ ownCommands ++ dependCommands ++ useCommands

----------------------------------------------------------------

main :: IO ()
main = do
  args0 <- getArgs
  let (options, commands) = span isOption args0
  mapM_ parseOptions options
  case commands of
    [] -> usage
    (cmd:cargs) -> (runCommand cmd allCommands) cargs


isOption :: String -> Bool
isOption = ('-' ==) . head

----------------------------------------------------------------

parseOptions :: String -> IO ()
parseOptions [] = return ()

parseOptions "-C"         = setColourEnabled False
parseOptions "--nocolor"  = setColourEnabled False
parseOptions "--nocolour" = setColourEnabled False

parseOptions _ = return ()

----------------------------------------------------------------

runCommand :: String -> [Command] -> CommandProc
runCommand _ [] = (\ _ -> usage)

runCommand command ((Short cmd _ f):cs)
  | command == cmd  = f
  | otherwise       = runCommand command cs

runCommand command ((Long cmd0 cmd1 _ f):cs)
  | command == cmd0 = f
  | command == cmd1 = f
  | otherwise       = runCommand command cs

----------------------------------------------------------------

usage :: IO ()
usage = do
  prog <- getProgName
  putStrLn "fquery 0.2\n"
  putStrLn $ "Usage: " ++ prog ++ " [options] <command> <arguments>\n"

  cyan >> putStr "Options:" >> off2
  inYellow (putStr "    -C --nocolour") >> tab >> putStrLn "turn off colours"
  nl

  cyan >> putStr "Commands for Installed Packages:" >> off2
  mapM_ putCommand logCommands; nl
  mapM_ putCommand listCommands; nl
  mapM_ putCommand ownCommands; nl
  mapM_ putCommand dependCommands; nl
  mapM_ putCommand useCommands; nl

putCommand :: Command -> IO ()
putCommand (Short cmd desc _) = f `withDesc` desc
  where f = green >> putStr cmd >> off >> tab

putCommand (Long cmd0 cmd1 desc _) = f `withDesc` desc
  where f = green >> putStr (cmd0 ++ "  " ++ cmd1) >> off

withDesc :: IO () -> String -> IO ()
f `withDesc` desc = putStr "    " >> f >> tab >> putStrLn desc

tab = putChar '\t'
nl  = putChar '\n'
