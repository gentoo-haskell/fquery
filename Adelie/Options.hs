{-# OPTIONS -fglasgow-exts #-}

-- Options.hs
--
-- We keep some globals in C so we don't have to pass them around everywhere.

module Adelie.Options (
  colourEnabled,
  setColourEnabled
) where

foreign import ccall unsafe "opts.h colour_enabled" colourEnabled :: IO Bool
foreign import ccall unsafe "opts.h set_colour_enabled" setColourEnabled :: Bool -> IO ()
