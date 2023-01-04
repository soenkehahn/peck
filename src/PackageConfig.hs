module PackageConfig where

import Context
import Control.Monad
import Data.List
import Package
import Prelude hiding (log)

type PackageConfig = [Package]

applyConfig :: Context -> [InstalledPackage] -> PackageConfig -> IO [InstalledPackage]
applyConfig context installedPackages packages = do
  let toUninstall = filter (not . (`elem` packages) . package) installedPackages
      toInstall = filter (not . (`elem` map package installedPackages)) packages
  log context $ "uninstalling: " <> unwords (map (name . package) toUninstall)
  log context $ "installing: " <> unwords (map name toInstall)
  forM_ toUninstall uninstall
  newlyInstalled <- forM toInstall $ \package -> do
    installPackage package
  return $
    nub $
      filter (not . (`elem` toUninstall)) $
        installedPackages ++ newlyInstalled
