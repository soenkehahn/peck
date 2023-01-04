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

run :: Context -> IO ()
run context = withCli $ \args -> do
  packages :: PackageConfig <- do
    result <- decodeFileEither (packageFile args)
    case result of
      Right packages -> return packages
      Left e -> throwIO $ ErrorCall $ show e
  db :: Db InstalledPackage <- initialize (dbFile args)
  applyConfig context db packages
  return ()
