{-# LANGUAGE QuasiQuotes #-}

module Peck.OverlayFSSpec where

import Data.String.Interpolate
import Data.String.Interpolate.Util
import Development.Shake (cmd, unit)
import Peck.OverlayFS
import System.Directory
import System.Exit
import System.FilePath ((</>))
import System.IO
import System.IO.Silently
import Test.Hspec
import Test.Mockery.Directory

writeScript :: String -> IO FilePath
writeScript code = do
  let script =
        unindent
          [i|
            #!/usr/bin/env bash

            #{code}
          |]
  writeFile "script.sh" script
  unit $ cmd "chmod +x" "script.sh"
  return "script.sh"

spec :: Spec
spec = focus $ do
  around_ inTempDirectory $ do
    describe "withMountedImageFile" $ do
      describe "without using the overlaid layer" $ do
        let test command = withMountedImageFile command (const $ return ())

        it "doesn't crash" $ do
          script <- writeScript "echo foo"
          silence $ test $ Script script

        it "performs the given action" $ do
          script <- writeScript "echo foo"
          output <- capture_ $ test $ Script script
          output `shouldBe` "foo\n"

        it "writing to /tmp isn't persistent" $ do
          script <- writeScript "echo foo > /tmp/test-file"
          test $ Script script
          doesFileExist "/tmp/test-file" `shouldReturn` False

        it "allows writing to subdirectories in /tmp" $ do
          script <-
            writeScript $
              unindent
                [i|
                mkdir /tmp/test-dir
                echo foo > /tmp/test-dir/test-file
              |]
          test $ Script script
          doesFileExist "/tmp/test-dir/test-file" `shouldReturn` False

        it "preserves the working directory" $ do
          workingDir <- getCurrentDirectory
          script <- writeScript "pwd"
          output <- capture_ $ test $ Script script
          output `shouldBe` workingDir <> "\n"

        it "allows to read existing files" $ do
          writeFile "file" "foo"
          script <- writeScript "cat file"
          output <- capture_ $ test $ Script script
          output `shouldBe` "foo"

        it "does not persist changes to existing files" $ do
          writeFile "file" "foo"
          script <- writeScript "echo bar > file"
          test $ Script script
          readFile "file" `shouldReturn` "foo"

      describe "looking at the overlaid layer" $ do
        it "allows reading created files" $ do
          script <- writeScript "echo foo > file"
          withMountedImageFile (Script script) $ \(Right dir) -> do
            workingDir <- getCurrentDirectory
            readFile (dir <> workingDir </> "file") `shouldReturn` "foo\n"

        it "allows reading modified files" $ do
          writeFile "pre-existing" "foo"
          script <- writeScript "echo bar > pre-existing"
          withMountedImageFile (Script script) $ \(Right dir) -> do
            workingDir <- getCurrentDirectory
            readFile (workingDir </> "pre-existing") `shouldReturn` "foo"
            readFile (dir <> workingDir </> "pre-existing") `shouldReturn` "bar\n"

        it "doesn't contain non-modified existing files" $ do
          writeFile "pre-existing" "foo"
          script <- writeScript "echo bar > other-file"
          withMountedImageFile (Script script) $ \(Right dir) -> do
            workingDir <- getCurrentDirectory
            doesFileExist (dir <> workingDir </> "pre-existing") `shouldReturn` False

        it "removes the overlaid layer at the end" $ do
          script <- writeScript "true"
          overlaidDir <- withMountedImageFile (Script script) $ \(Right dir) -> do
            return dir
          doesDirectoryExist overlaidDir `shouldReturn` False

        it "provides an error with exit code when the script execution failed" $ do
          script <- writeScript "exit 42"
          hSilence [stderr] $
            withMountedImageFile (Script script) $ \result -> do
              case result of
                Left exitCode -> do
                  exitCode `shouldBe` ExitFailure 42
                x -> error $ show x
