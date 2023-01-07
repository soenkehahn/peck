{-# LANGUAGE QuasiQuotes #-}

module Peck.PackageConfigSpec where

import Data.String.Interpolate
import Data.String.Interpolate.Util
import Peck.Package
import Peck.PackageConfig
import Peck.TestUtils
import System.FilePath
import Test.Hspec
import Test.Mockery.Directory

spec :: Spec
spec = wrapTests $ do
  describe "readPackageConfig" $ do
    it "reads a package config from a file" $ \_tempDir -> do
      touch configPath
      writeFile configPath $
        unindent
          [i|
            packages:
              - name: foo
                install: \|
                  install script
          |]
      readPackageConfig
        `shouldReturn` PackageConfig
          [ Package
              { name = "foo",
                skip = [],
                install = "install script\n"
              }
          ]

    describe "dhall" $ do
      it "allows to use dhall for configuration" $ \_tempDir -> do
        touch (configPath -<.> "dhall")
        writeFile (configPath -<.> "dhall") $
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
        readPackageConfig
          `shouldReturn` PackageConfig
            [ Package
                { name = "generated package",
                  skip = [],
                  install = "install script"
                }
            ]

      it "allows to omit optional fields, e.g. 'skip'" $ \_tempDir -> do
        touch (configPath -<.> "dhall")
        writeFile (configPath -<.> "dhall") $
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
        readPackageConfig
          `shouldReturn` PackageConfig
            [ Package
                { name = "generated package",
                  skip = [],
                  install = "install script"
                }
            ]
