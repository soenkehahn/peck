{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PackageConfigSpec where

import Context
import Data.String.Interpolate
import Data.String.Interpolate.Util
import Development.Shake (Stdout (..), cmd, cmd_)
import Package
import PackageConfig
import System.Directory
import System.FilePath
import Test.Hspec
import Test.Mockery.Directory
import TestUtils

spec :: Spec
spec = around (inTempDirectory . (getCurrentDirectory >>=)) $ do
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
