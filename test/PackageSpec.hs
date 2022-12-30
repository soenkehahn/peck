{-# LANGUAGE QuasiQuotes #-}

module PackageSpec where

import Data.String.Interpolate
import Data.String.Interpolate.Util
import Development.Shake (cmd, unit)
import Package
import System.Directory
import System.FilePath
import Test.Hspec
import Test.Mockery.Directory

mkScript :: String -> Package
mkScript code =
  Package
    { installScript =
        unindent
          [i|
            #!/usr/bin/env bash

            #{code}
          |]
    }

spec :: Spec
spec = do
  around_ inTempDirectory $ do
    describe "installPackage" $ do
      it "allows to install files" $ do
        _ <- installPackage (mkScript "echo foo > file")
        readFile "file" `shouldReturn` "foo\n"

      it "returns created files" $ do
        installedPackage <- installPackage (mkScript "echo foo > file")
        workingDir <- getCurrentDirectory
        files installedPackage `shouldBe` [workingDir </> "file"]

      it "returns multiple created files" $ do
        installedPackage <- installPackage (mkScript "touch foo ; touch bar")
        workingDir <- getCurrentDirectory
        files installedPackage `shouldBe` [workingDir </> "bar", workingDir </> "foo"]

      it "returns files in subdirectories" $ do
        installedPackage <- installPackage (mkScript "mkdir foo ; touch foo/bar")
        workingDir <- getCurrentDirectory
        files installedPackage `shouldBe` [workingDir </> "foo/bar"]

      it "allows to install hidden files" $ do
        installedPackage <- installPackage (mkScript "touch .hidden")
        workingDir <- getCurrentDirectory
        files installedPackage `shouldBe` [workingDir </> ".hidden"]
        readFile ".hidden" `shouldReturn` ""

    describe "applyConfig" $ do
      it "installs packages that aren't installed, but in the configuration" $ do
        let package = mkScript "echo foo > file"
        installed <- applyConfig [] [package]
        workingDir <- getCurrentDirectory
        installed `shouldBe` [InstalledPackage package [workingDir </> "file"] []]
        readFile "file" `shouldReturn` "foo\n"

      it "uninstalls installed packages that aren't in the configuration anymore" $ do
        touch "other-file"
        let package = mkScript "echo foo > file"
        installedPackage <- installPackage package
        readFile "file" `shouldReturn` "foo\n"
        _ <- applyConfig [installedPackage] []
        doesFileExist "file" `shouldReturn` False

      it "doesn't touch installed packages that are in the configuration" $ do
        let package = mkScript "echo foo > file"
        installedPackage <- installPackage package
        readFile "file" `shouldReturn` "foo\n"
        _ <- applyConfig [installedPackage] [package]
        readFile "file" `shouldReturn` "foo\n"

      it "removes empty directories during installation" $ do
        let package = mkScript "mkdir dir ; touch dir/file"
        installedPackage <- installPackage package
        _ <- applyConfig [installedPackage] []
        doesDirectoryExist "dir" `shouldReturn` False

      it "removes empty nested directories during installation" $ do
        let package = mkScript "mkdir -p foo/bar ; touch foo/bar/file"
        installedPackage <- installPackage package
        _ <- applyConfig [installedPackage] []
        doesDirectoryExist "foo" `shouldReturn` False

      it "also removes pre-existing empty directories" $ do
        unit $ cmd "mkdir dir"
        let package = mkScript "touch dir/file"
        installedPackage <- installPackage package
        _ <- applyConfig [installedPackage] []
        doesDirectoryExist "dir" `shouldReturn` False

      it "doesn't remove pre-existing files when uninstallating a package" $ do
        touch "other-file"
        let package = mkScript "touch file"
        installedPackage <- installPackage package
        _ <- applyConfig [installedPackage] []
        doesFileExist "other-file" `shouldReturn` True
