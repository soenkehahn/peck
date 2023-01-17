{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Peck.RunSpec where

import Data.List
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Development.Shake (Stdout (..), cmd, cmd_)
import Peck.Db
import Peck.Package
import Peck.Run
import Peck.TestUtils
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Silently
import Test.Hspec
import Test.Mockery.Directory

testRun :: [Package] -> IO [InstalledPackage]
testRun = testRunWithArgs []

testRunWithArgs :: [String] -> [Package] -> IO [InstalledPackage]
testRunWithArgs args config = do
  writeConfig config
  exitCode <- withArgs args $ run testContext
  case exitCode of
    ExitFailure _ -> do
      error $ show exitCode
    ExitSuccess -> do
      db <- initialize dbPath
      readDb db

spec :: Spec
spec = wrapTests $ do
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

    describe "spaces in filepaths" $ do
      it "installs files with spaces" $ \tempDir -> do
        let package = mkPackage [i|echo foo > '#{tempDir}/file with spaces'|]
        _ <- testRun [package]
        readFile "file with spaces" `shouldReturn` "foo\n"

      it "uninstall files with spaces" $ \tempDir -> do
        let package = mkPackage [i|echo foo > '#{tempDir}/file with spaces'|]
        _ <- testRun [package]
        _ <- testRun []
        doesFileExist "file with spaces" `shouldReturn` False

      it "installs files in directories with spaces" $ \tempDir -> do
        let package =
              mkPackage
                [i|
                  mkdir '#{tempDir}/dir with spaces'
                  echo foo > '#{tempDir}/dir with spaces/file'
                |]
        _ <- testRun [package]
        readFile "dir with spaces/file" `shouldReturn` "foo\n"

      it "uninstalls files in directories with spaces" $ \tempDir -> do
        let package =
              mkPackage
                [i|
                  mkdir '#{tempDir}/dir with spaces'
                  echo foo > '#{tempDir}/dir with spaces/file'
                |]
        _ <- testRun [package]
        _ <- testRun []
        fileExists <- doesFileExist "dir with spaces/file"
        dirExists <- doesDirectoryExist "dir with spaces"
        (fileExists, dirExists) `shouldBe` (False, False)

    describe "symlinks" $ do
      it "installs relative symlinks" $ \tempDir -> do
        let package =
              mkPackage
                [i|
                  cd '#{tempDir}'
                  echo foo > file
                  ln -s ./file link
                |]
        _ <- testRun [package]
        pathIsSymbolicLink "link" `shouldReturn` True
        getSymbolicLinkTarget "link" `shouldReturn` "./file"

      it "uninstalls symlinks" $ \tempDir -> do
        let package =
              mkPackage
                [i|
                  cd '#{tempDir}'
                  echo foo > file
                  ln -s ./file link
                |]
        _ <- testRun [package]
        _ <- testRun []
        doesFileExist "link" `shouldReturn` False

      it "installs absolute symlinks correctly" $ \tempDir -> do
        let package =
              mkPackage
                [i|
                  cd '#{tempDir}'
                  echo foo > file
                  ln -s #{tempDir}/file link
                |]
        _ <- testRun [package]
        pathIsSymbolicLink "link" `shouldReturn` True
        getSymbolicLinkTarget "link" `shouldReturn` (tempDir </> "file")

    describe "on errors" $ do
      it "shows an informative message when no package configuration file exists" $ \_ -> do
        output <- hCapture_ [stderr] $ run testContext
        home <- getHomeDirectory
        let expected =
              unindent
                [i|
                  No peck configuration found.
                  Please, create one at: #{home}/.config/peck/packages.yaml
                  or: #{home}/.config/peck/packages.dhall
                |]
        output `shouldBe` expected

      it "doesn't write anything to stdout" $ \_ -> do
        output <- capture_ $ hSilence [stderr] $ run testContext
        output `shouldBe` ""

      it "yields a non-zero exit-code" $ \_ -> do
        exitCode <- hSilence [stderr] $ run testContext
        exitCode `shouldBe` ExitFailure 1

      it "prints out yaml errors nicely" $ \_ -> do
        cmd_ "mkdir -p" $ takeDirectory configPath
        writeFile configPath "invalid_yaml: ["
        output <- hCapture_ [stderr] $ run testContext
        configPath <- makeAbsolute configPath
        let expected =
              unindent
                [i|
                  Error reading #{configPath}:
                  YAML parse exception at line 1, column 0,
                  while parsing a flow node:
                  did not find expected node content
                |]
        output `shouldBe` expected

      it "prints out dhall errors nicely" $ \_ -> do
        cmd_ "mkdir -p" $ takeDirectory configPath
        writeFile (configPath -<.> "dhall") "{ invalid_dhall = [ }"
        output <- hCapture_ [stderr] $ run testContext
        configPath <- makeAbsolute (configPath -<.> "dhall")
        let expected =
              unindent
                [i|

                  \ESC[1;31mError\ESC[0m: Invalid input

                  #{configPath}:1:21:
                    |
                  1 | { invalid_dhall = [ }
                    |                     ^
                  unexpected '}'
                  expecting ',', ], expression, or whitespace

                |]
        output `shouldBe` expected

      it "prints out install script errors nicely" $ \_ -> do
        let package = mkPackage [i| does-not-exist |]
        writeConfig [package]
        output <- hCapture_ [stderr] $ run testContext
        let scriptError =
              "/install.sh: line 3: does-not-exist: command not found\n"
        output `shouldSatisfy` (scriptError `isInfixOf`)
        let peckError = "PECK ERROR: Install script for package 'test package' failed, see errors above."
        drop 1 (lines output) `shouldBe` [peckError]

      it "returns a non-zero exit code on install script errors" $ \_ -> do
        let package = mkPackage [i| false |]
        writeConfig [package]
        exitCode <- hSilence [stderr] $ run testContext
        exitCode `shouldBe` ExitFailure 1

      it "relays non-zero exit codes from install scripts" $ \_ -> do
        let package = mkPackage [i| exit 42 |]
        writeConfig [package]
        exitCode <- hSilence [stderr] $ run testContext
        exitCode `shouldBe` ExitFailure 42

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
        writeConfig config
        ExitFailure 1 <- hSilence [stderr] $ run testContext
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
      ExitSuccess <- run testContext
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
