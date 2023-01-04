module Main where

import Context
import Run (run)

main :: IO ()
main = run Context.production
