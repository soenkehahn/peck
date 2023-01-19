{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list literal" #-}

module Peck.CliArgs where

import Data.Default
import WithCli

data CliArgs = CliArgs
  { listFiles :: Bool,
    list :: Bool,
    dryRun :: Bool,
    configDir :: Maybe FilePath
  }
  deriving stock (Show, Generic)

instance HasArguments CliArgs

instance Default CliArgs where
  def :: CliArgs
  def =
    CliArgs
      { listFiles = False,
        list = False,
        dryRun = False,
        configDir = Nothing
      }
