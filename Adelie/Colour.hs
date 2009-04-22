-- Colour.hs
--
-- Escape codes for colouring in output.

module Adelie.Colour (
    gray,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    white,
    off,
    off2
) where

gray    = putStr "\27[30;01m"
red     = putStr "\27[31;01m"
green   = putStr "\27[32;01m"
yellow  = putStr "\27[33;01m"
blue    = putStr "\27[34;01m"
magenta = putStr "\27[35;01m"
cyan    = putStr "\27[36;01m"
white   = putStr "\27[37;01m"
off     = putStr "\27[0m"
off2    = putStrLn "\27[0m"
