{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module TestUtils where

import Data.String.Interpolate
import Data.String.Interpolate.Util
import Package

mkPackage :: String -> Package
mkPackage code =
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

mkSkipPackage :: [String] -> String -> Package
mkSkipPackage skip code =
  (mkPackage code) {skip}
