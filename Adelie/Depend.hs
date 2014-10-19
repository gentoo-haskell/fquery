-- Depend.hs
--
-- Module for parsing DEPEND and RDEPEND files, located in
-- portageDB/category/package/.

module Adelie.Depend (
  Version,
  Dependency(..),

  dependFromCatName,
  readDepend,
  putDependency
) where

import Data.Char (isSpace)
import Data.List (nub)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token

import Adelie.Portage
import qualified Adelie.Util as E

type Version = String

data Dependency
  = GreaterEqual  String Version -- '>=c/p-v'
  | Equal         String Version -- '=c/p-v*'
  | Pinned        String Version -- '~c/p-v'
  | Blocker       String         -- '!c/p-v'
  | Any           String         -- 'c/p'
    deriving (Eq, Show)

----------------------------------------------------------------

dependFromCatName :: (String, String) -> String
dependFromCatName (cat, name) = concatPath [portageDB,cat,name,"RDEPEND"]

----------------------------------------------------------------

readDepend :: FilePath -> [String] -> IO [Dependency]
readDepend fn iUse = do
  r <- (start_parser fn) `E.catchIOE` (\ _ -> return $ Right [])
  case r of
    Left err -> putStr "Parse error at " >> print err >> error "Aborting"
    Right x  -> return $ nub x
  where start_parser = parseFromFile (dependParser iUse)

----------------------------------------------------------------

putDependency :: Dependency -> IO ()
putDependency (GreaterEqual p v) = putStr $ ">=" ++ p ++ '-':v
putDependency (Equal p v)        = putStr $ '=':p ++ '-':v
putDependency (Pinned p v)       = putStr $ '~':p ++ '-':v
putDependency (Blocker p)        = putStr $ '!':p
putDependency (Any p)            = putStr p

----------------------------------------------------------------

dependParser :: [String] -> Parser [Dependency]
dependParser iUse = do
  _skip <- spaces
  packages <- many (dependParser' iUse)
  eof
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
             <|>
                parseBrackets iUse

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
parsePackageOrUseWord = do { result <- many1 (satisfy cond)
                           -- skip[use]
                           -- TODO: add it to output
                           ; optionMaybe (do { char '['
                                             ; _use <- many1 (satisfy (/= ']'))
                                             ; char ']'
                                             -- ; return use
                                             ; return ()
                                             })
                           ; return result
                           }
  where
    cond '?' = False
    cond ')' = False
    cond '[' = False
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
toDependency s =
    case s of
        ('>':'=':str) ->
            let (n, v) = breakVersion str
            in GreaterEqual n v

        ('=':str) ->
            let (n, v) = breakVersion str
            in Equal n v

        ('~':str) ->
            let (n, v) = breakVersion str
            in Pinned n v

        ('!':n) ->
            Blocker n

        n ->
            Any n
