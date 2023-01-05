{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RunSpec where

import Context
import Control.Exception
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Yaml
import Db
import Development.Shake (Stdout (..), cmd, cmd_)
import Package
import PackageConfig
import Run
import System.Directory
import System.Environment
import System.FilePath
import Test.Hspec
import Test.Mockery.Directory
import TestUtils

testRun :: PackageConfig -> IO [InstalledPackage]
testRun config = do
  encodeFile "packages.yaml" config
  let args = ["--db-file", "db", "--package-file", "packages.yaml"]
  withArgs args $ run Context.test
  db <- initialize "db"
  readDb db

spec :: Spec
spec = around (inTempDirectory . (getCurrentDirectory >>=)) $ do
  describe "run" $ do
    it "installs packages that aren't installed, but in the configuration" $ \tempDir -> do
      let package = mkPackage [i|echo foo > #{tempDir}/file|]
      _ <- testRun [package]
      readFile "file" `shouldReturn` "foo\n"

    it "allows to uninstall packages by removing them from the config" $ \tempDir -> do
      let package = mkPackage [i|echo foo > #{tempDir}/file|]
      _ <- testRun [package]
      doesFileExist "file" `shouldReturn` True
      _ <- testRun []
      doesFileExist "file" `shouldReturn` False

    it "doesn't re-install packages that are in the configuration" $ \tempDir -> do
      let package = mkPackage [i|echo $RANDOM > #{tempDir}/file|]
      _ <- testRun [package]
      Stdout (before :: String) <- cmd "cat" "file"
      _ <- testRun [package]
      Stdout after <- cmd "cat" "file"
      after `shouldBe` before

    describe "written InstalledPackages" $ do
      it "returns newly installed packages" $ \tempDir -> do
        let package = mkPackage [i|echo foo > #{tempDir}/file|]
        installed <- testRun [package]
        installed `shouldBe` [InstalledPackage package [tempDir </> "file"]]

      it "returns already installed packages" $ \tempDir -> do
        let package = mkPackage [i|touch #{tempDir}/file|]
        [installedPackage] <- testRun [package]
        installedPackages <- testRun [package]
        installedPackages `shouldBe` [installedPackage]

      it "doesn't returned uninstalled packages" $ \tempDir -> do
        let package = mkPackage [i|touch #{tempDir}/file|]
        _ <- testRun [package]
        installedPackages <- testRun []
        installedPackages `shouldBe` []

    describe "uninstalling" $ do
      it "removes empty directories during installation" $ \tempDir -> do
        let package =
              mkPackage
                [i|
                    cd #{tempDir}
                    mkdir dir
                    touch dir/file
                  |]
        _ <- testRun [package]
        _ <- testRun []
        doesDirectoryExist "dir" `shouldReturn` False

      it "removes empty nested directories during installation" $ \tempDir -> do
        let package =
              mkPackage
                [i|
                    cd #{tempDir}
                    mkdir -p foo/bar
                    touch foo/bar/file
                  |]
        _ <- testRun [package]
        _ <- testRun []
        doesDirectoryExist "foo" `shouldReturn` False

      it "also removes pre-existing empty directories" $ \tempDir -> do
        cmd_ "mkdir dir"
        let package = mkPackage [i|touch #{tempDir}/dir/file|]
        _ <- testRun [package]
        _ <- testRun []
        doesDirectoryExist "dir" `shouldReturn` False

      it "doesn't remove pre-existing files when uninstallating a package" $ \tempDir -> do
        touch "other-file"
        let package = mkPackage [i|touch #{tempDir}/file|]
        _ <- testRun [package]
        _ <- testRun []
        doesFileExist "other-file" `shouldReturn` True

      it "removes installed files set to non-writeable" $ \tempDir -> do
        let package =
              mkPackage $
                unindent
                  [i|
                      cd #{tempDir}
                      mkdir dir
                      touch dir/file
                      chmod a-w -R dir
                    |]
        _ <- testRun [package]
        _ <- testRun []
        doesFileExist "file" `shouldReturn` False

      it "doesn't choke on files created in the temporary build directory" $ \_tempDir -> do
        let package =
              mkPackage $
                unindent
                  [i|
                      touch file
                    |]
        _ <- testRun [package]
        _ <- testRun []
        return ()

    describe "when subsequent packages fail" $ do
      it "saves successfully installed packages in the db" $ \tempDir -> do
        let goodPackage = mkPackage [i|echo foo > #{tempDir}/file|]
            failingPackage = mkPackage "false"
            config = [goodPackage, failingPackage]
        testRun config `shouldThrow` (\(_ :: SomeException) -> True)
        db :: [InstalledPackage] <- readDb =<< initialize "db"
        db
          `shouldBe` [ InstalledPackage
                         { package = goodPackage,
                           files = [tempDir </> "file"]
                         }
                     ]

    it "allows to use dhall for configuration" $ \tempDir -> do
      writeFile
        "packages.dhall"
        [i|
          [
            { name = "generated package",
              skip = [] : List Text,
              install =
                ''
                #!/usr/bin/env bash
                echo foo > #{tempDir}/file
                ''
            }
          ]
        |]
      let args = ["--db-file", "db", "--package-file", "packages.dhall"]
      withArgs args $ run Context.test
      readFile "file" `shouldReturn` "foo\n"
