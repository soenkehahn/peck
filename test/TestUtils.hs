{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module TestUtils where

import Data.String.Interpolate
import Data.String.Interpolate.Util
import Package

mkScript :: String -> Package
mkScript code =
  Package
    { name = "test-script",
      skip = [],
      install =
        unindent
          [i|
            #!/usr/bin/env bash

            #{code}
          |]
    }

skipScript :: [String] -> String -> Package
skipScript skip code =
  (mkScript code) {skip}
