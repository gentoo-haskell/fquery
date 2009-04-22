-- Pretty.hs
--
-- Colourful output.

module Adelie.Pretty (
  putCatName,
  putCatNameLn,
  putNum,
  putNumLn
) where

import Adelie.Colour

----------------------------------------------------------------

putCatName, putCatNameLn :: (String, String) -> IO ()
putCatName   = putCatName' off
putCatNameLn = putCatName' off2

putCatName' :: IO () -> (String, String) -> IO ()
putCatName' f (c, n) =
  yellow >> putStr c >> off >> putChar '/' >> yellow >> putStr n >> f

----------------------------------------------------------------

putNum, putNumLn :: Int -> IO ()
putNum   = putNum' off
putNumLn = putNum' off2

putNum' :: IO () -> Int -> IO ()
putNum' f n = cyan >> putStr (show n) >> f
