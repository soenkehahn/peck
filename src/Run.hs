{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Run where

import Context
import Control.Monad
import Data.Function
import Data.List
import Db
import Package
import PackageConfig
import WithCli

data Args = Args
  { dbFile :: FilePath,
    packageFile :: FilePath,
    listFiles :: Bool,
    list :: Bool,
    dryRun :: Bool
  }
  deriving stock (Show, Generic)

instance HasArguments Args

run :: Context -> IO ()
run context = withCli $ \args -> do
  db <- initialize (dbFile args)
  if list args
    then listPackages db
    else
      if listFiles args
        then listPackagesWithFiles db
        else apply context args db

apply :: Context -> Args -> Db InstalledPackage -> IO ()
apply context args db = do
  packageConfig <- readPackageConfig (packageFile args)
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
