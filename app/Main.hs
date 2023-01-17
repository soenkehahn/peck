module Main where

import Peck.Context
import Peck.Run (run)
import System.Exit

main :: IO ()
main =
  exitWith =<< run Peck.Context.production
