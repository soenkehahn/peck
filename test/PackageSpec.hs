{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PackageSpec where

import Context
import Data.Bifunctor
import Data.List
import Data.String.Conversions
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Yaml
import Development.Shake (Stdout (..), cmd, cmd_)
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
      skip = [],
      install =
        unindent
          [i|
            #!/usr/bin/env bash

            #{code}
          |]
    }

skipScript :: [String] -> String -> Package
skipScript skip code =
  (mkScript code) {skip}

spec :: Spec
spec = do
  around (inTempDirectory . (getCurrentDirectory >>=)) $ do
    describe "installPackage" $ do
      it "allows to install files" $ \tempDir -> do
        _ <- installPackage (mkScript [i|echo foo > #{tempDir}/file|])
        readFile "file" `shouldReturn` "foo\n"

      it "returns created files" $ \tempDir -> do
        installedPackage <- installPackage (mkScript [i|echo foo > #{tempDir}/file|])
        files installedPackage `shouldBe` [tempDir </> "file"]

      it "returns multiple created files" $ \tempDir -> do
        installedPackage <-
          installPackage $
            mkScript $
              unindent
                [i|
                  cd #{tempDir}
                  touch foo
                  touch bar
                |]
        files installedPackage `shouldBe` [tempDir </> "bar", tempDir </> "foo"]

      it "returns files in subdirectories" $ \tempDir -> do
        installedPackage <-
          installPackage $
            mkScript $
              unindent
                [i|
                  cd #{tempDir}
                  mkdir foo
                  touch foo/bar
                |]
        files installedPackage `shouldBe` [tempDir </> "foo/bar"]

      it "allows to install hidden files" $ \tempDir -> do
        installedPackage <- installPackage (mkScript [i|touch #{tempDir}/.hidden|])
        files installedPackage `shouldBe` [tempDir </> ".hidden"]
        readFile ".hidden" `shouldReturn` ""

      it "runs install script in a temporary build directory" $ \tempDir -> do
        buildDir <- fmap stripSpaces $ capture_ $ installPackage (mkScript "pwd")
        buildDir `shouldSatisfy` ("/tmp/" `isPrefixOf`)
        stripSpaces buildDir `shouldSatisfy` (/= tempDir)
        doesDirectoryExist buildDir `shouldReturn` False

      it "does not install files created in the temporary build directory" $ \_tempDir -> do
        (buildDir, installedPackage) <-
          fmap (first stripSpaces) $
            capture $
              installPackage $
                mkScript [i|echo foo > file ; pwd|]
        doesFileExist (buildDir </> "file") `shouldReturn` False
        files installedPackage `shouldBe` []

      describe "skip" $ do
        it "allows to skip created files from being installed" $ \tempDir -> do
          _ <-
            installPackage $
              skipScript [tempDir </> "file"] $
                unindent
                  [i|
                    cd #{tempDir}
                    touch file
                  |]
          doesFileExist "file" `shouldReturn` False

        it "allows to skip created directories from being installed" $ \tempDir -> do
          _ <-
            installPackage $
              skipScript [tempDir </> "dir"] $
                unindent
                  [i|
                    cd #{tempDir}
                    mkdir dir
                    touch dir/file
                  |]
          doesFileExist "dir/file" `shouldReturn` False

        it "allows to specify '~' for the $HOME directory" $ \_tempDir -> do
          home <- getEnv "HOME"
          _isSkipped (skipScript ["~/file"] "") (home </> "file") `shouldReturn` True
          _isSkipped (skipScript ["~/dir"] "") (home </> "dir/file") `shouldReturn` True

        it "allows to skip multiple patterns" $ \tempDir -> do
          _ <-
            installPackage $
              skipScript [tempDir </> "a", tempDir </> "b"] $
                unindent
                  [i|
                    cd #{tempDir}
                    touch a
                    touch b
                  |]
          doesFileExist "a" `shouldReturn` False
          doesFileExist "b" `shouldReturn` False

        describe "parsing the skip field" $ do
          it "parses strings" $ \_tempDir -> do
            let yaml =
                  [i|
                    name: foo
                    skip: skipped
                    install: ""
                  |]
            parsed <- decodeThrow (cs yaml)
            skip parsed `shouldBe` ["skipped"]

          it "parses lists of strings" $ \_tempDir -> do
            let yaml =
                  [i|
                    name: foo
                    skip:
                      - a
                      - b
                    install: ""
                  |]
            parsed <- decodeThrow (cs yaml)
            skip parsed `shouldBe` ["a", "b"]

          it "parses omitted skip fields" $ \_tempDir -> do
            let yaml =
                  [i|
                    name: foo
                    install: ""
                  |]
            parsed <- decodeThrow (cs yaml)
            skip parsed `shouldBe` []

        it "uninstalls unskipped files correctly" $ \tempDir -> do
          installedPackage <-
            installPackage $
              skipScript [tempDir </> "skipped"] $
                unindent
                  [i|
                    cd #{tempDir}
                    touch skipped
                    touch unskipped
                  |]
          uninstall installedPackage
          doesFileExist "unskipped" `shouldReturn` False

    describe "applyConfig" $ do
      applyConfig <- return $ applyConfig Context.test
      it "installs packages that aren't installed, but in the configuration" $ \tempDir -> do
        let package = mkScript [i|echo foo > #{tempDir}/file|]
        _ <- applyConfig [] [package]
        readFile "file" `shouldReturn` "foo\n"

      it "uninstalls installed packages that aren't in the configuration anymore" $ \tempDir -> do
        touch "other-file"
        let package = mkScript [i|echo foo > #{tempDir}/file|]
        installedPackage <- installPackage package
        readFile "file" `shouldReturn` "foo\n"
        _ <- applyConfig [installedPackage] []
        doesFileExist "file" `shouldReturn` False

      it "doesn't re-install packages that are in the configuration" $ \tempDir -> do
        let package = mkScript [i|echo $RANDOM > #{tempDir}/file|]
        installedPackage <- installPackage package
        Stdout (before :: String) <- cmd "cat" "file"
        _ <- applyConfig [installedPackage] [package]
        Stdout after <- cmd "cat" "file"
        after `shouldBe` before

      it "doesn't allow packages to modify existing files" $ \tempDir -> do
        touch "pre-existing"
        let package = mkScript [i|echo foo > #{tempDir}/pre-existing|]
        installPackage package
          `shouldThrow` ( ==
                            Error
                              ( "file already exists: "
                                  <> tempDir
                                  </> "pre-existing"
                              )
                        )

      it "doesn't install any files if some files already exist" $ \tempDir -> do
        touch "b"
        let package =
              mkScript
                [i|
                  cd #{tempDir}
                  echo foo > a
                  echo bar > b
                |]
        installPackage package
          `shouldThrow` ( ==
                            Error
                              ( "file already exists: "
                                  <> tempDir
                                  </> "b"
                              )
                        )
        doesFileExist "a" `shouldReturn` False

      describe "returned InstalledPackages" $ do
        it "returns newly installed packages" $ \tempDir -> do
          let package = mkScript [i|echo foo > #{tempDir}/file|]
          installed <- applyConfig [] [package]
          installed `shouldBe` [InstalledPackage package [tempDir </> "file"]]

        it "returns already installed packages" $ \tempDir -> do
          let package = mkScript [i|touch #{tempDir}/file|]
          installedPackage <- installPackage package
          installedPackages <- applyConfig [installedPackage] [package]
          installedPackages `shouldBe` [installedPackage]

        it "doesn't returned uninstalled packages" $ \tempDir -> do
          let package = mkScript [i|touch #{tempDir}/file|]
          installedPackage <- installPackage package
          installedPackages <- applyConfig [installedPackage] []
          installedPackages `shouldBe` []

      describe "uninstalling" $ do
        it "removes empty directories during installation" $ \tempDir -> do
          let package =
                mkScript
                  [i|
                    cd #{tempDir}
                    mkdir dir
                    touch dir/file
                  |]
          installedPackage <- installPackage package
          _ <- applyConfig [installedPackage] []
          doesDirectoryExist "dir" `shouldReturn` False

        it "removes empty nested directories during installation" $ \tempDir -> do
          let package =
                mkScript
                  [i|
                    cd #{tempDir}
                    mkdir -p foo/bar
                    touch foo/bar/file
                  |]
          installedPackage <- installPackage package
          _ <- applyConfig [installedPackage] []
          doesDirectoryExist "foo" `shouldReturn` False

        it "also removes pre-existing empty directories" $ \tempDir -> do
          cmd_ "mkdir dir"
          let package = mkScript [i|touch #{tempDir}/dir/file|]
          installedPackage <- installPackage package
          _ <- applyConfig [installedPackage] []
          doesDirectoryExist "dir" `shouldReturn` False

        it "doesn't remove pre-existing files when uninstallating a package" $ \tempDir -> do
          touch "other-file"
          let package = mkScript [i|touch #{tempDir}/file|]
          installedPackage <- installPackage package
          _ <- applyConfig [installedPackage] []
          doesFileExist "other-file" `shouldReturn` True

        it "removes installed files set to non-writeable" $ \tempDir -> do
          let package =
                mkScript $
                  unindent
                    [i|
                      cd #{tempDir}
                      mkdir dir
                      touch dir/file
                      chmod a-w -R dir
                    |]
          installedPackage <- installPackage package
          _ <- applyConfig [installedPackage] []
          doesFileExist "file" `shouldReturn` False

        it "doesn't choke on files created in the temporary build directory" $ \_tempDir -> do
          let package =
                mkScript $
                  unindent
                    [i|
                      touch file
                    |]
          installedPackage <- installPackage package
          _ <- applyConfig [installedPackage] []
          return ()
