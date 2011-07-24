-- CompareVersion.hs
--
-- Version string comparitor, which handles numbers differently: 1.10 > 1.9.

module Adelie.CompareVersion (compareVersion) where

import Data.Char (isDigit)

import Adelie.ListEx (digitsToInt)

----------------------------------------------------------------

compareVersion :: String -> String -> Ordering
compareVersion = cmpA

cmpA :: String -> String -> Ordering
cmpA a0 b0
  | r == EQ   = cmpB as bs
  | otherwise = r
  where (a, as) = break (not.isDigit) a0
        (b, bs) = break (not.isDigit) b0
        r = compare (digitsToInt a) (digitsToInt b)

cmpB :: String -> String -> Ordering
cmpB [] []      = EQ
cmpB ('*':_) _  = EQ
cmpB _ ('*':_)  = EQ
cmpB []  _      = LT
cmpB  _ []      = GT
cmpB a0 b0
  | r == EQ   = cmpA as bs
  | otherwise = r
  where (a, as) = break isDigit a0
        (b, bs) = break isDigit b0
        r = compare a b
