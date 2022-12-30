{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PackageSpec where

import Context
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Development.Shake (Stdout (..), cmd, unit)
import Package
import System.Directory
import System.FilePath
import Test.Hspec
import Test.Mockery.Directory

mkScript :: String -> Package
mkScript code =
  Package
    { name = "test-script",
      install =
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
      applyConfig <- return $ applyConfig Context.test
      it "installs packages that aren't installed, but in the configuration" $ do
        let package = mkScript "echo foo > file"
        _ <- applyConfig [] [package]
        readFile "file" `shouldReturn` "foo\n"

      it "uninstalls installed packages that aren't in the configuration anymore" $ do
        touch "other-file"
        let package = mkScript "echo foo > file"
        installedPackage <- installPackage package
        readFile "file" `shouldReturn` "foo\n"
        _ <- applyConfig [installedPackage] []
        doesFileExist "file" `shouldReturn` False

      it "doesn't re-install packages that are in the configuration" $ do
        let package = mkScript "echo $RANDOM > file"
        installedPackage <- installPackage package
        Stdout (before :: String) <- cmd "cat" "file"
        _ <- applyConfig [installedPackage] [package]
        Stdout after <- cmd "cat" "file"
        after `shouldBe` before

      it "doesn't allow packages to modify existing files" $ do
        touch "pre-existing"
        let package = mkScript "echo foo > pre-existing"
        workingDir <- getCurrentDirectory
        installPackage package
          `shouldThrow` ( ==
                            Error
                              ( "file already exists: "
                                  <> workingDir
                                  </> "pre-existing"
                              )
                        )

      it "doesn't install any files if some files already exist" $ do
        touch "b"
        let package = mkScript "echo foo > a ; echo bar > b"
        workingDir <- getCurrentDirectory
        installPackage package
          `shouldThrow` ( ==
                            Error
                              ( "file already exists: "
                                  <> workingDir
                                  </> "b"
                              )
                        )
        doesFileExist "a" `shouldReturn` False

      describe "returned InstalledPackages" $ do
        it "returns newly installed packages" $ do
          let package = mkScript "echo foo > file"
          installed <- applyConfig [] [package]
          workingDir <- getCurrentDirectory
          installed `shouldBe` [InstalledPackage package [workingDir </> "file"] []]

        it "returns already installed packages" $ do
          let package = mkScript "touch file"
          installedPackage <- installPackage package
          installedPackages <- applyConfig [installedPackage] [package]
          installedPackages `shouldBe` [installedPackage]

        it "doesn't returned uninstalled packages" $ do
          let package = mkScript "touch file"
          installedPackage <- installPackage package
          installedPackages <- applyConfig [installedPackage] []
          installedPackages `shouldBe` []

      describe "uninstalling" $ do
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

        it "removes installed files set to non-writeable" $ do
          let package =
                mkScript $
                  unindent
                    [i|
                      mkdir dir
                      touch dir/file
                      chmod a-w -R dir
                    |]
          installedPackage <- installPackage package
          _ <- applyConfig [installedPackage] []
          doesFileExist "file" `shouldReturn` False
