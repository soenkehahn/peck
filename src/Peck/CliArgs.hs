{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}

module Peck.CliArgs where

import Data.Default
import Data.String.Interpolate
import Data.String.Interpolate.Util
import WithCli

data CliArgs = CliArgs
  { dryRun :: Bool,
    configDir :: Maybe FilePath,
    list :: Bool,
    listFiles :: Bool
  }
  deriving stock (Show, Generic)

instance HasArguments CliArgs

instance Default CliArgs where
  def :: CliArgs
  def =
    CliArgs
      { dryRun = False,
        configDir = Nothing,
        list = False,
        listFiles = False
      }

withArgs :: (CliArgs -> IO ()) -> IO ()
withArgs =
  withCliModified Peck.CliArgs.modifiers

modifiers :: [Modifier]
modifiers =
  [ AddOptionHelp
      "dryRun"
      ( unindent
          [i|
            Print what peck would do without actually
            installing or uninstalling anything.
          |]
      ),
    AddOptionHelp
      "configDir"
      ( unindent
          [i|
            Set the directory where peck looks for
            packages.yaml, packages.dhall and the
            database file. (default: ~/.config/peck/)
          |]
      ),
    AddOptionHelp
      "list"
      ( unindent
          [i|
            List all installed packages.
          |]
      ),
    AddOptionHelp
      "listFiles"
      ( unindent
          [i|
            List all installed packages, including
            all files installed by each package.
          |]
      )
  ]
