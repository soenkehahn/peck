{-# LANGUAGE NamedFieldPuns #-}

module TestUtils where

import Package

mkPackage :: String -> Package
mkPackage code =
  Package
    { name = "test package",
      skip = [],
      install = "#!/usr/bin/env bash\n\n" <> code
    }

mkSkipPackage :: [String] -> String -> Package
mkSkipPackage skip code =
  (mkPackage code) {skip}
