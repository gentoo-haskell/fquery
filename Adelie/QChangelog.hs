-- QChangelog.hs
--
-- Module to find the changelog of a package.

module Adelie.QChangelog (
  qChangelog,
  qLogFile
) where

import Char   (isDigit, isSpace)
import Monad  (unless)

import Adelie.Colour
import Adelie.CompareVersion
import Adelie.ListEx
import Adelie.Portage
import Adelie.Pretty

----------------------------------------------------------------

qLogFile :: [String] -> IO ()
qLogFile args = mapM_ (putStrLn.logFile) =<< findInstalledPackages args

logFile :: (String, String) -> String
logFile (cat, name) = 
  portageTree ++ '/':cat ++ '/':(dropVersion name) ++ "/ChangeLog"

----------------------------------------------------------------

qChangelog :: [String] -> IO ()
qChangelog []       = return ()
qChangelog [x]      = mapM_ (changelog Nothing)  =<< findInstalledPackages [x]
qChangelog (x:y:_)  = mapM_ (changelog (Just y)) =<< findInstalledPackages [x]

changelog :: Maybe String -> (String, String) -> IO ()
changelog end catname@(_, name) = do
  putStr "ChangeLog since " >> putCatNameLn catname
  log' <- readFile (logFile catname)
  puts name end log'

----------------------------------------------------------------

puts :: String -> Maybe String -> String -> IO ()

puts _ _ [] = return ()
puts inst end ('#':l) = puts inst end (dropUntilAfter isNewLine l)

puts inst end ('*':l0) = do
  case maybeCompareVersion package end of
    GT -> puts inst end next
    _  -> case compareVersion inst package of
      LT -> putSection package date >> putDesc ls >> puts inst end next
      _  -> puts inst end next
  where (section, next) = breakSection l0
        (line, ls) = break2 isNewLine section
        (l1, l2) = break2 ('(' ==) line
        package = takeWhile (not.isSpace) l1
        date = takeWhile (')' /=) l2

puts inst end (_:ls) = puts inst end ls

maybeCompareVersion :: String -> Maybe String -> Ordering
maybeCompareVersion _ Nothing   = LT
maybeCompareVersion a (Just b)  = compareVersion a b

----------------------------------------------------------------

breakSection :: String -> (String, String)
breakSection str = (reverse h, t)
  where (h, t) = breakSection' [] str

breakSection' :: String -> String -> (String, String)
breakSection' acc [] = (acc, [])
breakSection' acc nx@('\n':'*':_) = (acc, nx)
breakSection' acc (x:xs) = breakSection' (x:acc) xs

----------------------------------------------------------------

putSection :: String -> String -> IO ()
putSection package date = do
  putStr "* " >> inYellow (putStr package) 
  putStr " (" >> inWhite (putStr date) >> putStrLn ")"

----------------------------------------------------------------

putDesc :: String -> IO ()
putDesc [] = return ()
putDesc str = mapM_ putDesc' ls
  where ls = doublelines (dropWhile isNewLine str)

putDesc' :: String -> IO ()
putDesc' str = do
  if beginsWithDate (dropWhile isSpace str)
    then do
      putHeader header
      putBody body
    else
      putBody str
  where (header, body) = break2 (':' ==) str

doublelines :: String -> [String]
doublelines str =
  case t of
    []           -> h : []
    (_:'\n':xs)  -> h : doublelines xs
    (_:xs)       -> let (l0:l1) = doublelines xs 
                    in (h ++ ('\n' : l0)) : l1
  where (h, t) = break ('\n' ==) str

beginsWithDate :: String -> Bool
beginsWithDate []   = False
beginsWithDate [_]  = False
beginsWithDate (x:y:_)
  | not (isDigit x)         = False
  | not (isDigitOrSpace y)  = False
  | otherwise = True

----------------------------------------------------------------

putHeader :: String -> IO ()
putHeader str0 = do
  putChar '\n'
  inWhite (putStr date)
  unless (null name) (putStr name)
  red >> putStr ('<':mail) >> putChar '>' >> off2
  putStr "    " >> putFiles 76 files
  where
    (date, str1) = break2 (';' ==) str0
    (name, str2) = break2 ('<' ==) str1
    (mail, str3) = break2 ('>' ==) str2
    files = words str3

putFiles :: Int -> [String] -> IO ()
putFiles _ [] = putChar '\n'
putFiles rem' (";":files) = putFiles rem' files
putFiles rem' (f:files) =
  if rem' < len
    then do
      putChar '\n' >> putStr "    "
      putFiles 76 (f:files)
    else do
      cyan
      if (last f == ',')
        then putStr (dropTail 1 f) >> off >> putStr ", "
        else putStr f >> off >> putChar ' '
      putFiles (rem' - len - 1) files
  where len = length f

----------------------------------------------------------------

putBody :: String -> IO ()
putBody [] = putChar '\n'

putBody ('#':c0) =
  case bug of
    [] -> putChar '#' >> putBody cs
    _  -> inMagenta (putStr ('#':bug)) >> putBody cs
  where (bug, cs) = span (isDigitOrSpace) c0

putBody (c:cs) = putChar c >> putBody cs

----------------------------------------------------------------

isNewLine :: Char -> Bool
isNewLine '\n'  = True
isNewLine _     = False

isDigitOrSpace :: Char -> Bool
isDigitOrSpace x = isDigit x || isSpace x
