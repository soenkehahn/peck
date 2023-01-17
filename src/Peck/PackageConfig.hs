{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Peck.PackageConfig where

import Control.Exception
import Control.Monad
import Data.List
import Data.Yaml
import Dhall
import Peck.Context
import Peck.Db
import Peck.Error
import Peck.Package
import Peck.Utils
import System.Directory
import System.FilePath
import Prelude hiding (log)

newtype PackageConfig = PackageConfig
  { packages :: [Package]
  }
  deriving (Show, Eq, Generic)

instance FromJSON PackageConfig

instance ToJSON PackageConfig

instance FromDhall PackageConfig

getPackageFile :: IO FilePath
getPackageFile = do
  configDir <- getPeckConfigDir
  files <- listDirectory configDir
  let possibleConfigFiles = ["packages.yaml", "packages.dhall"]
  case filter (`elem` possibleConfigFiles) files of
    [] -> do
      throwIO $
        peckError $
          intercalate
            "\n"
            [ "No peck configuration found.",
              "Please, create one at: " <> (configDir </> "packages.yaml"),
              "or: " <> (configDir </> "packages.dhall")
            ]
    [file] -> return $ configDir </> file
    files -> throwIO $ peckError $ "multiple config files found: " <> unwords files

readPackageConfig :: IO PackageConfig
readPackageConfig = do
  path <- getPackageFile
  case takeExtension path of
    ".yaml" -> do
      result <- decodeFileEither path
      case result of
        Right packages -> return packages
        Left e ->
          throwIO $
            peckError $
              ("Error reading " <> path <> ":\n")
                <> prettyPrintParseException e
    ".dhall" -> do
      catch (inputFile auto path) $ \(e :: SomeException) -> do
        throwIO $ peckError $ show e
    extension -> do
      throwIO $ peckError $ "unknown config file extension: " <> extension

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
