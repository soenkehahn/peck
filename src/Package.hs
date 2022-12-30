{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Package where

import Control.Monad
import Data.List
import Development.Shake (cmd, unit)
import OverlayFS (Command (..), withMountedImageFile)
import System.FilePath (takeDirectory, (</>))
import Utils

newtype Package = Package
  { installScript :: String
  }
  deriving stock (Show, Eq)

data InstalledPackage = InstalledPackage
  { package :: Package,
    files :: [FilePath],
    dirs :: [FilePath]
  }
  deriving stock (Show, Eq)

installPackage :: Package -> IO InstalledPackage
installPackage Package {installScript} = do
  withTempDir $ \tempDir -> do
    writeFile (tempDir </> "install.sh") installScript
    unit $ cmd "chmod +x" (tempDir </> "install.sh")
    files <- withMountedImageFile (Script (tempDir </> "install.sh")) $ \overlay -> do
      files <- readFilesRecursively overlay
      forM files $ \file -> do
        let installTarget = "/" </> file
        unit $ cmd "mkdir -p" (takeDirectory installTarget)
        unit $ cmd "cp" (overlay </> file) installTarget
        return installTarget
    return $ InstalledPackage (Package {installScript}) files []

applyConfig :: [InstalledPackage] -> [Package] -> IO [InstalledPackage]
applyConfig installedPackages packages = do
  let toUninstall = filter (not . (`elem` packages) . package) installedPackages
      toInstall = filter (not . (`elem` map package installedPackages)) packages
  forM_ toUninstall $ \package -> do
    forM_ (files package) $ \file -> do
      unit $ cmd "rm" file
      removeEmptyParents file
  newlyInstalled <- forM toInstall $ \package -> do
    installPackage package
  return $
    nub $
      filter (not . (`elem` toUninstall)) $
        installedPackages ++ newlyInstalled

removeEmptyParents :: FilePath -> IO ()
removeEmptyParents path = do
  let parent = takeDirectory path
  files <- readFiles parent
  when (null files) $ do
    unit $ cmd "rm -rf" parent
    removeEmptyParents parent
