module Main where

import Peck.Context
import Peck.Run (run)

main :: IO ()
main = run Peck.Context.production
