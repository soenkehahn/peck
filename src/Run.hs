{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Run where

import Context
import Control.Exception
import Data.Yaml
import Db
import Package
import PackageConfig
import WithCli

data Args = Args
  { dbFile :: FilePath,
    packageFile :: FilePath
  }
  deriving stock (Show, Generic)

instance HasArguments Args

run :: IO ()
run = withCli $ \args -> do
  packages :: PackageConfig <- do
    result <- decodeFileEither (packageFile args)
    case result of
      Right packages -> return packages
      Left e -> throwIO $ ErrorCall $ show e
  withState_ (dbFile args) ([] :: [InstalledPackage]) $ \installed -> do
    applyConfig Context.production installed packages
