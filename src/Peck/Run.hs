{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Peck.Run where

import Control.Monad
import Data.Function
import Data.List
import Peck.Context
import Peck.Db
import Peck.Error
import Peck.Package
import Peck.PackageConfig
import Peck.Utils
import System.Exit
import System.FilePath
import WithCli

data Args = Args
  { listFiles :: Bool,
    list :: Bool,
    dryRun :: Bool
  }
  deriving stock (Show, Generic)

instance HasArguments Args

getDbFile :: IO FilePath
getDbFile = do
  (</> "db") <$> getPeckConfigDir

run :: Context -> IO ExitCode
run context =
  handleErrors $ do
    withCli $ \args -> do
      db <- initialize =<< getDbFile
      if list args
        then listPackages db
        else
          if listFiles args
            then listPackagesWithFiles db
            else apply context args db

apply :: Context -> Args -> Db InstalledPackage -> IO ()
apply context args db = do
  packageConfig <- readPackageConfig
  if dryRun args
    then void $ getApplyPlan context db packageConfig
    else applyConfig context db packageConfig

listPackages :: Db InstalledPackage -> IO ()
listPackages db = do
  packages <- readDb db
  putStr $ unlines $ sort $ map (name . package) packages

listPackagesWithFiles :: Db InstalledPackage -> IO ()
listPackagesWithFiles db = do
  packages <- readDb db
  putStr $
    mconcat $
      map printPackage $
        sortBy (compare `on` (name . package)) packages
  where
    printPackage :: InstalledPackage -> String
    printPackage p =
      name (package p)
        ++ ":\n"
        ++ concatMap (\file -> "  " <> file <> "\n") (files p)
