-- Depend.hs
--
-- Module for parsing DEPEND and RDEPEND files, located in
-- portageDB/cateogry/package/.

module Adelie.Depend (
  Version,
  Dependency(..),

  dependFromCatName,
  readDepend,
  putDependency
) where

import Char (isSpace)
import List (nub)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token

import Adelie.Portage

type Version = String

data Dependency
  = GreaterEqual  String Version
  | Equal         String Version
  | Unstable      String Version
  | NotInstalled  String
  | Any           String
    deriving (Eq, Show)

----------------------------------------------------------------

dependFromCatName :: (String, String) -> String
dependFromCatName (cat, name) = concatPath [portageDB,cat,name,"RDEPEND"]

----------------------------------------------------------------

readDepend :: FilePath -> [String] -> IO [Dependency]
readDepend fn iUse = do
  r <- parseFromFile (dependParser iUse) fn
  case r of
    Left err -> putStr "Parse error at " >> print err >> error "Aborting"
    Right x  -> return $ nub x

----------------------------------------------------------------

putDependency :: Dependency -> IO ()
putDependency (GreaterEqual p v) = putStr $ ">=" ++ p ++ '-':v
putDependency (Equal p v)        = putStr $ '=':p ++ '-':v
putDependency (Unstable p v)     = putStr $ '~':p ++ '-':v
putDependency (NotInstalled p)   = putStr $ '!':p
putDependency (Any p)            = putStr p

----------------------------------------------------------------

dependParser :: [String] -> Parser [Dependency]
dependParser iUse = do
  skip <- spaces
  packages <- many (dependParser' iUse)
  return $ concat packages

dependParser' :: [String] -> Parser [Dependency]
dependParser' iUse = lexeme tp pp
  where tp = makeTokenParser emptyDef
        pp = parseOr iUse
         <|> parsePackageOrUse iUse

parseOr :: [String] -> Parser [Dependency]
parseOr iUse = do { string "||"
                  ; spaces
                  ; parseBrackets iUse
                  }

parsePackageOrUse :: [String] -> Parser [Dependency]
parsePackageOrUse iUse =
  do { p <- parsePackageOrUseWord
     ; do { char '?'        -- useFlag
          ; spaces
          ; r <- parseBrackets iUse
          ; let filt | head p == '!' = not $ (tail p) `elem` iUse
                     | otherwise     = p `elem` iUse
          ; if filt
              then return r
              else return []
          }
   <|> return [toDependency p]
     }

parsePackageOrUseWord :: Parser String
parsePackageOrUseWord = many1 (satisfy cond)
  where
    cond '?' = False
    cond ')' = False
    cond x = not $ isSpace x

parseBrackets :: [String] -> Parser [Dependency]
parseBrackets iUse = do { char '('
                        ; spaces
                        ; r <- manyTill (dependParser' iUse) (try (char ')'))
                        ; return $ concat r
                        }

----------------------------------------------------------------

breakVersion :: String -> (String, String)
breakVersion str = (n, v)
  where n = dropVersion str
        v = drop (length n+1) str

toDependency :: String -> Dependency

toDependency ('>':'=':str) = (GreaterEqual n v)
  where (n, v) = breakVersion str

toDependency ('=':str) = (Equal n v)
  where (n, v) = breakVersion str

toDependency ('~':str) = (Unstable n v)
  where (n, v) = breakVersion str

toDependency ('!':n) = (NotInstalled n)

toDependency n = (Any n)
