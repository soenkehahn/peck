{-# LANGUAGE DeriveGeneric #-}

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

newtype PackageConfig = PackageConfig
  { packages :: [Package]
  }
  deriving (Show, Eq, Generic)

instance FromJSON PackageConfig

instance ToJSON PackageConfig

instance FromDhall PackageConfig

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
applyConfig context db config = do
  (toUninstall, toInstall) <- getApplyPlan context db config
  forM_ toUninstall $ \package -> do
    uninstall package
    removeElement db package
  forM_ toInstall $ \package -> do
    installPackage package
      >>= addElement db

getApplyPlan ::
  Context ->
  Db InstalledPackage ->
  PackageConfig ->
  IO ([InstalledPackage], [Package])
getApplyPlan context db config = do
  installedPackages <- readDb db
  let toUninstall = filter (not . (`elem` packages config) . package) installedPackages
      toInstall = filter (not . (`elem` fmap package installedPackages)) $ packages config
  logPlan "uninstall" $ fmap package toUninstall
  logPlan "install" toInstall
  return (toUninstall, toInstall)
  where
    logPlan verb packages = log context $ case packages of
      [] -> "nothing to " <> verb
      packages ->
        intercalate
          "\n"
          ( ("to " <> verb <> ":")
              : fmap (\p -> "  - " <> name p) packages
          )
