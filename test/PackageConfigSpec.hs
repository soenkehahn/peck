{-# LANGUAGE QuasiQuotes #-}

module PackageConfigSpec where

import Data.String.Interpolate
import Data.String.Interpolate.Util
import Package
import PackageConfig
import System.Directory
import Test.Hspec
import Test.Mockery.Directory

spec :: Spec
spec = around (inTempDirectory . (getCurrentDirectory >>=)) $ do
  describe "readPackageConfig" $ do
    it "reads a package config from a file" $ \_tempDir -> do
      writeFile "packages.yaml" $
        unindent
          [i|
            - name: foo
              install: \|
                install script
          |]
      readPackageConfig "packages.yaml"
        `shouldReturn` [ Package
                           { name = "foo",
                             skip = [],
                             install = "install script\n"
                           }
                       ]
