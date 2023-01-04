{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Package
  ( Package (..),
    InstalledPackage (..),
    Error (..),
    installPackage,
    uninstall,
    _isSkipped,
  )
where

import Control.Exception
import Control.Monad
import Data.List
import Data.Yaml (FromJSON)
import Development.Shake (cmd, unit)
import GHC.Generics (Generic)
import OverlayFS (Command (..), withMountedImageFile)
import System.Directory
import System.Environment
import System.FilePath (splitDirectories, takeDirectory, (</>))
import System.IO
import Utils
import Prelude hiding (log)

data Package = Package
  { name :: String,
    skip :: Maybe String,
    install :: String
  }
  deriving stock (Show, Read, Eq, Generic)

instance FromJSON Package

data InstalledPackage = InstalledPackage
  { package :: Package,
    files :: [FilePath]
  }
  deriving stock (Show, Read, Eq)

newtype Error = Error String
  deriving stock (Show, Eq)

instance Exception Error

installPackage :: Package -> IO InstalledPackage
installPackage package = do
  withTempDir $ \((</> "install.sh") -> installScript) -> do
    writeFile installScript $ install package
    unit $ cmd "chmod +x" installScript
    withTempDir $ \buildDir -> do
      withCurrentDirectory buildDir $ do
        files <- withMountedImageFile (Script installScript) $ \overlay -> do
          files <- listFilesFromOverlay buildDir package overlay
          forM_ files $ \(_, installTarget) -> do
            exists <- doesFileExist installTarget
            when exists $ do
              throwIO $ Error $ "file already exists: " <> installTarget
          forM files $ \(source, installTarget) -> do
            exists <- doesFileExist installTarget
            when exists $ do
              throwIO $
                Error $
                  "PANIC: while installing package, found existing file: " <> installTarget
            unit $ cmd "mkdir -p" (takeDirectory installTarget)
            unit $ cmd "cp" source installTarget
            return installTarget
        return $ InstalledPackage package files

listFilesFromOverlay :: FilePath -> Package -> FilePath -> IO [(FilePath, FilePath)]
listFilesFromOverlay buildDir package overlay = do
  filesInOverlay <- readFilesRecursively overlay
  let copyPairs = map (\file -> (overlay </> file, "/" </> file)) filesInOverlay
      withoutTemporaryBuildDir = filter (not . (buildDir `isPrefixOf`) . snd) copyPairs
  filterM (fmap not . _isSkipped package . snd) withoutTemporaryBuildDir

_isSkipped :: Package -> FilePath -> IO Bool
_isSkipped package (splitDirectories -> path) =
  case skip package of
    Nothing -> return False
    Just pattern -> do
      pattern <- expandTilde $ splitDirectories pattern
      return $ pattern `isPrefixOf` path

expandTilde :: [String] -> IO [String]
expandTilde = \case
  "~" : rest -> do
    home <- getEnv "HOME"
    return $ splitDirectories home ++ rest
  pattern -> return pattern

uninstall :: InstalledPackage -> IO ()
uninstall package =
  forM_ (files package) $ \file -> do
    unit $ cmd "rm" file
    removeEmptyParents file

removeEmptyParents :: FilePath -> IO ()
removeEmptyParents path = do
  let parent = takeDirectory path
  files <- readFiles parent
  when (null files) $ do
    unit $ cmd "rm -rf" parent
    removeEmptyParents parent
