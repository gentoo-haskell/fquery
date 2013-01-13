module Adelie.Util (
  catchIOE
) where

import qualified Control.Exception as E

catchIOE :: IO a -> (E.IOException -> IO a) -> IO a
catchIOE = E.catch
