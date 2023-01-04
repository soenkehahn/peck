{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PackageSpec where

import Context
import Data.List
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Development.Shake (Stdout (..), cmd, unit)
import Package
import PackageConfig
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Silently
import Test.Hspec
import Test.Mockery.Directory
import Utils

mkScript :: String -> Package
mkScript code =
  Package
    { name = "test-script",
      skip = Nothing,
      install =
        unindent
          [i|
            #!/usr/bin/env bash

            #{code}
          |]
    }

skipScript :: String -> String -> Package
skipScript skip code =
  (mkScript code)
    { skip = Just skip
    }

spec :: Spec
spec = do
  around (inTempDirectory . (getCurrentDirectory >>=)) $ do
    describe "installPackage" $ do
      it "allows to install files" $ \installDir -> do
        _ <- installPackage (mkScript [i|echo foo > #{installDir}/file|])
        readFile "file" `shouldReturn` "foo\n"

      it "returns created files" $ \installDir -> do
        installedPackage <- installPackage (mkScript [i|echo foo > #{installDir}/file|])
        files installedPackage `shouldBe` [installDir </> "file"]

      it "returns multiple created files" $ \installDir -> do
        installedPackage <-
          installPackage $
            mkScript $
              unindent
                [i|
                  cd #{installDir}
                  touch foo
                  touch bar
                |]
        files installedPackage `shouldBe` [installDir </> "bar", installDir </> "foo"]

      it "returns files in subdirectories" $ \installDir -> do
        installedPackage <-
          installPackage $
            mkScript $
              unindent
                [i|
                  cd #{installDir}
                  mkdir foo
                  touch foo/bar
                |]
        files installedPackage `shouldBe` [installDir </> "foo/bar"]

      it "allows to install hidden files" $ \installDir -> do
        installedPackage <- installPackage (mkScript [i|touch #{installDir}/.hidden|])
        files installedPackage `shouldBe` [installDir </> ".hidden"]
        readFile ".hidden" `shouldReturn` ""

      it "runs install script in a temporary build directory" $ \installDir -> do
        buildDir <- fmap stripSpaces $ capture_ $ installPackage (mkScript "pwd")
        buildDir `shouldSatisfy` ("/tmp/" `isPrefixOf`)
        stripSpaces buildDir `shouldSatisfy` (/= installDir)
        doesDirectoryExist buildDir `shouldReturn` False

      it "does not install files created in the temporary build directory" $ \_installDir -> do
        buildDir <- fmap stripSpaces $ capture_ $ installPackage $ mkScript [i|echo foo > file ; pwd|]
        doesFileExist (buildDir </> "file") `shouldReturn` False

      describe "skip" $ do
        it "allows to skip created files from being installed" $ \installDir -> do
          _ <-
            installPackage $
              skipScript (installDir </> "file") $
                unindent
                  [i|
                    cd #{installDir}
                    touch file
                  |]
          doesFileExist "file" `shouldReturn` False

        it "allows to skip created directories from being installed" $ \installDir -> do
          _ <-
            installPackage $
              skipScript (installDir </> "dir") $
                unindent
                  [i|
                    cd #{installDir}
                    mkdir dir
                    touch dir/file
                  |]
          doesFileExist "dir/file" `shouldReturn` False

        it "allows to specify '~' for the $HOME directory" $ \_installDir -> do
          home <- getEnv "HOME"
          _isSkipped (skipScript "~/file" "") (home </> "file") `shouldReturn` True
          _isSkipped (skipScript "~/dir" "") (home </> "dir/file") `shouldReturn` True

        it "uninstalls unskipped files correctly" $ \installDir -> do
          installedPackage <-
            installPackage $
              skipScript (installDir </> "skipped") $
                unindent
                  [i|
                    cd #{installDir}
                    touch skipped
                    touch unskipped
                  |]
          uninstall installedPackage
          doesFileExist "unskipped" `shouldReturn` False

    describe "applyConfig" $ do
      applyConfig <- return $ applyConfig Context.test
      it "installs packages that aren't installed, but in the configuration" $ \installDir -> do
        let package = mkScript [i|echo foo > #{installDir}/file|]
        _ <- applyConfig [] [package]
        readFile "file" `shouldReturn` "foo\n"

      it "uninstalls installed packages that aren't in the configuration anymore" $ \installDir -> do
        touch "other-file"
        let package = mkScript [i|echo foo > #{installDir}/file|]
        installedPackage <- installPackage package
        readFile "file" `shouldReturn` "foo\n"
        _ <- applyConfig [installedPackage] []
        doesFileExist "file" `shouldReturn` False

      it "doesn't re-install packages that are in the configuration" $ \installDir -> do
        let package = mkScript [i|echo $RANDOM > #{installDir}/file|]
        installedPackage <- installPackage package
        Stdout (before :: String) <- cmd "cat" "file"
        _ <- applyConfig [installedPackage] [package]
        Stdout after <- cmd "cat" "file"
        after `shouldBe` before

      it "doesn't allow packages to modify existing files" $ \installDir -> do
        touch "pre-existing"
        let package = mkScript [i|echo foo > #{installDir}/pre-existing|]
        installPackage package
          `shouldThrow` ( ==
                            Error
                              ( "file already exists: "
                                  <> installDir
                                  </> "pre-existing"
                              )
                        )

      it "doesn't install any files if some files already exist" $ \installDir -> do
        touch "b"
        let package =
              mkScript
                [i|
                  cd #{installDir}
                  echo foo > a
                  echo bar > b
                |]
        installPackage package
          `shouldThrow` ( ==
                            Error
                              ( "file already exists: "
                                  <> installDir
                                  </> "b"
                              )
                        )
        doesFileExist "a" `shouldReturn` False

      describe "returned InstalledPackages" $ do
        it "returns newly installed packages" $ \installDir -> do
          let package = mkScript [i|echo foo > #{installDir}/file|]
          installed <- applyConfig [] [package]
          installed `shouldBe` [InstalledPackage package [installDir </> "file"]]

        it "returns already installed packages" $ \installDir -> do
          let package = mkScript [i|touch #{installDir}/file|]
          installedPackage <- installPackage package
          installedPackages <- applyConfig [installedPackage] [package]
          installedPackages `shouldBe` [installedPackage]

        it "doesn't returned uninstalled packages" $ \installDir -> do
          let package = mkScript [i|touch #{installDir}/file|]
          installedPackage <- installPackage package
          installedPackages <- applyConfig [installedPackage] []
          installedPackages `shouldBe` []

      describe "uninstalling" $ do
        it "removes empty directories during installation" $ \installDir -> do
          let package =
                mkScript
                  [i|
                    cd #{installDir}
                    mkdir dir
                    touch dir/file
                  |]
          installedPackage <- installPackage package
          _ <- applyConfig [installedPackage] []
          doesDirectoryExist "dir" `shouldReturn` False

        it "removes empty nested directories during installation" $ \installDir -> do
          let package =
                mkScript
                  [i|
                    cd #{installDir}
                    mkdir -p foo/bar
                    touch foo/bar/file
                  |]
          installedPackage <- installPackage package
          _ <- applyConfig [installedPackage] []
          doesDirectoryExist "foo" `shouldReturn` False

        it "also removes pre-existing empty directories" $ \installDir -> do
          unit $ cmd "mkdir dir"
          let package = mkScript [i|touch #{installDir}/dir/file|]
          installedPackage <- installPackage package
          _ <- applyConfig [installedPackage] []
          doesDirectoryExist "dir" `shouldReturn` False

        it "doesn't remove pre-existing files when uninstallating a package" $ \installDir -> do
          touch "other-file"
          let package = mkScript [i|touch #{installDir}/file|]
          installedPackage <- installPackage package
          _ <- applyConfig [installedPackage] []
          doesFileExist "other-file" `shouldReturn` True

        it "removes installed files set to non-writeable" $ \installDir -> do
          let package =
                mkScript $
                  unindent
                    [i|
                      cd #{installDir}
                      mkdir dir
                      touch dir/file
                      chmod a-w -R dir
                    |]
          installedPackage <- installPackage package
          _ <- applyConfig [installedPackage] []
          doesFileExist "file" `shouldReturn` False
