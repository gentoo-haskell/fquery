-- Module to wrap all exception code
-- TODO: distinct different kinds of error in own datatype

module Adelie.Error
    ( try
    , bracket
) where

import qualified Control.Exception.Extensible as E

try :: IO a -> IO (Either E.SomeException a)
try = E.try

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket = E.bracket
