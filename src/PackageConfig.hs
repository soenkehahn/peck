module PackageConfig where

import Context
import Control.Monad
import Data.List
import Db
import Package
import Prelude hiding (log)

type PackageConfig = [Package]

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
