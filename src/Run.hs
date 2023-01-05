{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Run where

import Context
import Db
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
  packageConfig <- readPackageConfig (packageFile args)
  db <- initialize (dbFile args)
  applyConfig context db packageConfig
