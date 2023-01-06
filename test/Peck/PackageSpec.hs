{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Peck.PackageSpec where

import Data.Bifunctor
import Data.List
import Data.String.Conversions
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Data.Yaml
import Peck.Package
import Peck.TestUtils
import Peck.Utils
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Silently
import Test.Hspec
import Test.Mockery.Directory

spec :: Spec
spec = do
  wrapTests $ do
    describe "installPackage" $ do
      it "allows to install files" $ \tempDir -> do
        _ <- installPackage (mkPackage [i|echo foo > #{tempDir}/file|])
        readFile "file" `shouldReturn` "foo\n"

      it "returns created files" $ \tempDir -> do
        installedPackage <- installPackage (mkPackage [i|echo foo > #{tempDir}/file|])
        files installedPackage `shouldBe` [tempDir </> "file"]

      it "returns multiple created files" $ \tempDir -> do
        installedPackage <-
          installPackage $
            mkPackage $
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
            mkPackage $
              unindent
                [i|
                  cd #{tempDir}
                  mkdir foo
                  touch foo/bar
                |]
        files installedPackage `shouldBe` [tempDir </> "foo/bar"]

      it "allows to install hidden files" $ \tempDir -> do
        installedPackage <- installPackage (mkPackage [i|touch #{tempDir}/.hidden|])
        files installedPackage `shouldBe` [tempDir </> ".hidden"]
        readFile ".hidden" `shouldReturn` ""

      it "runs install script in a temporary build directory" $ \tempDir -> do
        buildDir <- fmap stripSpaces $ capture_ $ installPackage (mkPackage "pwd")
        buildDir `shouldSatisfy` ("/tmp/" `isPrefixOf`)
        stripSpaces buildDir `shouldSatisfy` (/= tempDir)
        doesDirectoryExist buildDir `shouldReturn` False

      it "does not install files created in the temporary build directory" $ \_tempDir -> do
        (buildDir, installedPackage) <-
          fmap (first stripSpaces) $
            capture $
              installPackage $
                mkPackage [i|echo foo > file ; pwd|]
        doesFileExist (buildDir </> "file") `shouldReturn` False
        files installedPackage `shouldBe` []

      it "doesn't allow packages to modify existing files" $ \tempDir -> do
        touch "pre-existing"
        let package = mkPackage [i|echo foo > #{tempDir}/pre-existing|]
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
              mkPackage
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

      describe "skip" $ do
        it "allows to skip created files from being installed" $ \tempDir -> do
          _ <-
            installPackage $
              mkSkipPackage [tempDir </> "file"] $
                unindent
                  [i|
                    cd #{tempDir}
                    touch file
                  |]
          doesFileExist "file" `shouldReturn` False

        it "allows to skip created directories from being installed" $ \tempDir -> do
          _ <-
            installPackage $
              mkSkipPackage [tempDir </> "dir"] $
                unindent
                  [i|
                    cd #{tempDir}
                    mkdir dir
                    touch dir/file
                  |]
          doesFileExist "dir/file" `shouldReturn` False

        it "allows to specify '~' for the $HOME directory" $ \_tempDir -> do
          home <- getEnv "HOME"
          _isSkipped (mkSkipPackage ["~/file"] "") (home </> "file") `shouldReturn` True
          _isSkipped (mkSkipPackage ["~/dir"] "") (home </> "dir/file") `shouldReturn` True

        it "allows to skip multiple patterns" $ \tempDir -> do
          _ <-
            installPackage $
              mkSkipPackage [tempDir </> "a", tempDir </> "b"] $
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
              mkSkipPackage [tempDir </> "skipped"] $
                unindent
                  [i|
                    cd #{tempDir}
                    touch skipped
                    touch unskipped
                  |]
          uninstall installedPackage
          doesFileExist "unskipped" `shouldReturn` False
