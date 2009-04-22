-- UseDesc.hs
--
-- Module for parsing portageProfile/use.desc and use.local.desc files.

module Adelie.UseDesc (
  UseDescriptions,
  readUseDesc,
  readUseDescPackage
) where

import Data.HashTable as HashTable
import Text.ParserCombinators.Parsec

import Adelie.Portage

type UseDesc = (String, String)
type UseDescriptions = HashTable String String

----------------------------------------------------------------

genReadDesc :: Parser [UseDesc] -> String -> IO [UseDesc]
genReadDesc p fn = do
  r <- parseFromFile p fn
  case r of
    Left err -> putStr "Parse error at " >> print err >> error "Aborting"
    Right x  -> return x

----------------------------------------------------------------

readUseDesc :: IO UseDescriptions
readUseDesc = genReadDesc useParser useDesc >>= HashTable.fromList hashString

useParser :: Parser [UseDesc]
useParser = useParser' `sepEndBy` newline

useParser' :: Parser UseDesc
useParser' = parseComment useParser'
         <|> parseUse

----------------------------------------------------------------

readUseDescPackage :: String -> String -> IO UseDescriptions
readUseDescPackage start end = 
  genReadDesc (useParser2 start end) useDescPackage >>=
  HashTable.fromList hashString

useParser2 :: String -> String -> Parser [UseDesc]
useParser2 start end = (useParser2' start end) `sepEndBy` newline

useParser2' :: String -> String -> Parser UseDesc
useParser2' start end =
      parseComment (useParser2' start end)
  <|> do { readname <- many1 (satisfy notColon)
         ; case mid start readname end of
             LT -> skipMany (satisfy notNewline) >> useParser2' start end
             GT -> return ("","")
             otherise -> char ':' >> parseUse
         }

----------------------------------------------------------------

-- In Haskell, vim-core > vim
-- In sort,    vim-core < vim
-- Work around it.
myCompare :: String -> String -> Ordering
myCompare [] [] = EQ
myCompare  _ [] = LT
myCompare []  _ = GT
myCompare (a:as) (b:bs) =
  if r == EQ
    then myCompare as bs 
    else r
  where r = compare a b

mid :: String -> String -> String -> Ordering
mid l m r
  | myCompare l m == GT = LT
  | myCompare m r == GT = GT
  | otherwise = EQ

----------------------------------------------------------------

parseUse :: Parser UseDesc
parseUse = do { use <- useFlag
              ; spaces
              ; string "- "
              ; desc <- description
              ; return (use, desc)
              }

parseComment :: Parser UseDesc -> Parser UseDesc
parseComment cont = do { char '#'
                       ; skipMany (satisfy notNewline)
                       ; cont
                       }
                <|> do { space
                       ; cont
                       }

useFlag :: Parser String
useFlag = many1 (satisfy (/= ' '))

description :: Parser String
description = many1 (satisfy notNewline)

notColon, notNewline :: Char -> Bool
notColon ':'    = False
notColon _      = True
notNewline '\n' = False
notNewline _    = True
