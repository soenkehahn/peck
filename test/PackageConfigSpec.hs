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
            packages:
              - name: foo
                install: \|
                  install script
          |]
      readPackageConfig "packages.yaml"
        `shouldReturn` PackageConfig
          [ Package
              { name = "foo",
                skip = [],
                install = "install script\n"
              }
          ]

    describe "dhall" $ do
      it "allows to use dhall for configuration" $ \_tempDir -> do
        writeFile "packages.dhall" $
          unindent
            [i|
              {
                packages = [
                  { name = "generated package",
                    skip = [] : List Text,
                    install = "install script",
                  }
                ],
              }
            |]
        readPackageConfig "packages.dhall"
          `shouldReturn` PackageConfig
            [ Package
                { name = "generated package",
                  skip = [],
                  install = "install script"
                }
            ]

      it "allows to omit optional fields, e.g. 'skip'" $ \_tempDir -> do
        writeFile "packages.dhall" $
          unindent
            [i|
              let def = { skip = [] : List Text }
              in {
                packages = [
                  def // {
                    name = "generated package",
                    install = "install script",
                  }
                ],
              }
            |]
        readPackageConfig "packages.dhall"
          `shouldReturn` PackageConfig
            [ Package
                { name = "generated package",
                  skip = [],
                  install = "install script"
                }
            ]
