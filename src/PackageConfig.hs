module PackageConfig where

import Context
import Control.Exception
import Control.Monad
import Data.List
import Data.Yaml
import Db
import Package
import Prelude hiding (log)

type PackageConfig = [Package]

readPackageConfig :: FilePath -> IO PackageConfig
readPackageConfig path = do
  result <- decodeFileEither path
  case result of
    Right packages -> return packages
    Left e -> throwIO $ ErrorCall $ show e

applyConfig :: Context -> Db InstalledPackage -> PackageConfig -> IO ()
applyConfig context db packages = do
  installedPackages <- readDb db
  let toUninstall = filter (not . (`elem` packages) . package) installedPackages
      toInstall = filter (not . (`elem` map package installedPackages)) packages
  log context $ "uninstalling: " <> unwords (map (name . package) toUninstall)
  log context $ "installing: " <> unwords (map name toInstall)
  forM_ toUninstall $ \package -> do
    uninstall package
    removeElement db package
  forM_ toInstall $ \package -> do
    installPackage package
      >>= addElement db
