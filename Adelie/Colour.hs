-- Colour.hs
--
-- Escape codes for colouring in output.

module Adelie.Colour (
    gray,     inGray,
    red,      inRed,
    green,    inGreen,
    yellow,   inYellow,
    blue,     inBlue,
    magenta,  inMagenta,
    cyan,     inCyan,
    white,    inWhite,
    off,
    off2
) where

import Control.Monad (when)

import Adelie.Options

gray, red, green, yellow, blue, magenta, cyan, white, off, off2 :: IO ()

gray    = whenM colourEnabled $ putStr "\27[30;01m"
red     = whenM colourEnabled $ putStr "\27[31;01m"
green   = whenM colourEnabled $ putStr "\27[32;01m"
yellow  = whenM colourEnabled $ putStr "\27[33;01m"
blue    = whenM colourEnabled $ putStr "\27[34;01m"
magenta = whenM colourEnabled $ putStr "\27[35;01m"
cyan    = whenM colourEnabled $ putStr "\27[36;01m"
white   = whenM colourEnabled $ putStr "\27[37;01m"
off     = whenM colourEnabled $ putStr "\27[0m"
off2    = do
  true <- colourEnabled
  if true
    then putStrLn "\27[0m"
    else putChar '\n'

whenM :: IO Bool -> IO () -> IO ()
whenM cond f = cond >>= flip when f

----------------------------------------------------------------

inGray, inRed, inGreen, inYellow    :: IO () -> IO ()
inBlue, inMagenta, inCyan, inWhite  :: IO () -> IO ()

inGray    f = gray    >> f >> off
inRed     f = red     >> f >> off
inGreen   f = yellow  >> f >> off
inYellow  f = yellow  >> f >> off
inBlue    f = blue    >> f >> off
inMagenta f = magenta >> f >> off
inCyan    f = cyan    >> f >> off
inWhite   f = white   >> f >> off

