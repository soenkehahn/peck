{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Peck.RunSpec where

import Control.Exception
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Yaml
import Development.Shake (Stdout (..), cmd, cmd_)
import Peck.Db
import Peck.Package
import Peck.PackageConfig
import Peck.Run
import Peck.TestUtils
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Silently
import Test.Hspec
import Test.Mockery.Directory

testRun :: [Package] -> IO [InstalledPackage]
testRun = testRunWithArgs []

testRunWithArgs :: [String] -> [Package] -> IO [InstalledPackage]
testRunWithArgs args (PackageConfig -> config) = do
  cmd_ "mkdir -p" $ takeDirectory configPath
  encodeFile configPath config
  withArgs args $ run testContext
  db <- initialize dbPath
  readDb db

shouldThrowError :: IO a -> String -> IO ()
shouldThrowError action error = do
  result :: Maybe Error <- (action >> return Nothing) `catch` (return . Just)
  case result of
    Nothing -> fail "didn't throw"
    Just (Error got) -> do
      got `shouldBe` error

spec :: Spec
spec = wrapTests $ do
  describe "run" $ do
    it "shows an informative message when no package configuration file exists" $ \_ -> do
      home <- getHomeDirectory
      let expected =
            unindent
              [i|
                No peck configuration found.
                Please, create one at: #{home}/.config/peck/packages.yaml
                or: #{home}/.config/peck/packages.dhall
              |]
      run testContext `shouldThrowError` expected

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

    describe "installed files with spaces" $ do
      it "installs files with spaces" $ \tempDir -> do
        let package = mkPackage [i|echo foo > '#{tempDir}/file with spaces'|]
        _ <- testRun [package]
        readFile "file with spaces" `shouldReturn` "foo\n"

      it "uninstall files with spaces" $ \tempDir -> do
        let package = mkPackage [i|echo foo > '#{tempDir}/file with spaces'|]
        _ <- testRun [package]
        _ <- testRun []
        doesFileExist "file with spaces" `shouldReturn` False

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
        db :: [InstalledPackage] <- readDb =<< initialize dbPath
        db
          `shouldBe` [ InstalledPackage
                         { package = goodPackage,
                           files = [tempDir </> "file"]
                         }
                     ]

    it "allows to use dhall for configuration" $ \tempDir -> do
      cmd_ "mkdir -p" peckConfigDir
      writeFile
        (configPath -<.> "dhall")
        [i|
          {
            packages = [
              { name = "generated package",
                skip = [] : List Text,
                install =
                  ''
                  #!/usr/bin/env bash
                  echo foo > #{tempDir}/file
                  '',
              }
            ],
          }
        |]
      run testContext
      readFile "file" `shouldReturn` "foo\n"

    describe "--list-files" $ do
      it "lists all installed files" $ \tempDir -> do
        let a =
              ( mkPackage $
                  unindent
                    [i|
                      touch #{tempDir}/a
                      touch #{tempDir}/b
                    |]
              )
                { name = "a"
                }
            b =
              ( mkPackage $
                  unindent
                    [i|
                      touch #{tempDir}/foo
                      touch #{tempDir}/bar
                    |]
              )
                { name = "b"
                }
        _ <- testRun [b, a]
        output <- capture_ $ testRunWithArgs ["--list-files"] [b, a]
        output
          `shouldBe` unindent
            [i|
              a:
                #{tempDir}/a
                #{tempDir}/b
              b:
                #{tempDir}/bar
                #{tempDir}/foo
            |]

    describe "--list" $ do
      it "lists all installed packages" $ \tempDir -> do
        let a =
              ( mkPackage $
                  unindent
                    [i|
                      touch #{tempDir}/a
                      touch #{tempDir}/b
                    |]
              )
                { name = "a"
                }
            b =
              ( mkPackage $
                  unindent
                    [i|
                      touch #{tempDir}/foo
                      touch #{tempDir}/bar
                    |]
              )
                { name = "b"
                }
        _ <- testRun [b, a]
        output <- capture_ $ testRunWithArgs ["--list"] [b, a]
        output
          `shouldBe` unindent
            [i|
              a
              b
            |]

    describe "--dry-run" $ do
      it "does not install packages" $ \tempDir -> do
        let package = mkPackage $ unindent [i| touch #{tempDir}/file |]
        _ <- testRunWithArgs ["--dry-run"] [package]
        doesFileExist "file" `shouldReturn` False

      it "does not uninstall packages" $ \tempDir -> do
        let package = mkPackage $ unindent [i| touch #{tempDir}/file |]
        _ <- testRun [package]
        _ <- testRunWithArgs ["--dry-run"] []
        readFile "file" `shouldReturn` ""

      it "shows packages to install" $ \tempDir -> do
        let package = mkPackage $ unindent [i| touch #{tempDir}/file |]
        _ <- testRunWithArgs ["--dry-run"] [package]
        output <- readTestLogs
        output
          `shouldBe` unindent
            [i|
              nothing to uninstall
              to install:
                - test package
            |]

      it "shows packages to uninstall" $ \tempDir -> do
        let package = mkPackage $ unindent [i| touch #{tempDir}/file |]
        _ <- testRun [package]
        resetTestLogs
        _ <- testRunWithArgs ["--dry-run"] []
        output <- readTestLogs
        output
          `shouldBe` unindent
            [i|
              to uninstall:
                - test package
              nothing to install
            |]

      it "shows empty install plan" $ \_ -> do
        _ <- testRunWithArgs ["--dry-run"] []
        output <- readTestLogs
        output
          `shouldBe` unindent
            [i|
              nothing to uninstall
              nothing to install
            |]
