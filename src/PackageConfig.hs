module PackageConfig where

import Context
import Control.Exception
import Control.Monad
import Data.List
import Data.Yaml
import Db
import Dhall
import Package
import System.FilePath
import Prelude hiding (log)

type PackageConfig = [Package]

readPackageConfig :: FilePath -> IO PackageConfig
readPackageConfig path = do
  case takeExtension path of
    ".yaml" -> do
      result <- decodeFileEither path
      case result of
        Right packages -> return packages
        Left e -> throwIO $ ErrorCall $ show e
    ".dhall" -> do
      inputFile auto path
    extension -> do
      throwIO $ ErrorCall $ "unknown config file extension: " <> extension

applyConfig :: Context -> Db InstalledPackage -> PackageConfig -> IO ()
applyConfig context db packages = do
  installedPackages <- readDb db
  let toUninstall = filter (not . (`elem` packages) . package) installedPackages
      toInstall = filter (not . (`elem` fmap package installedPackages)) packages
  log context $ "uninstalling: " <> unwords (fmap (name . package) toUninstall)
  log context $ "installing: " <> unwords (fmap name toInstall)
  forM_ toUninstall $ \package -> do
    uninstall package
    removeElement db package
  forM_ toInstall $ \package -> do
    installPackage package
      >>= addElement db
