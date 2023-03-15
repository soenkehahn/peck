module Peck.Run where

import Control.Monad
import Data.Function
import Data.List
import Peck.CliArgs
import Peck.Context
import Peck.Db
import Peck.Error
import Peck.Package
import Peck.PackageConfig
import Peck.Utils
import System.Exit
import System.FilePath

getDbFile :: CliArgs -> IO FilePath
getDbFile args = do
  (</> "db") <$> getPeckConfigDir args

run :: Context -> IO ExitCode
run context =
  handleErrors $ do
    withArgs $ \args -> do
      dbFile <- getDbFile args
      withDb context dbFile $ \db ->
        if list args
          then listPackages db
          else
            if listFiles args
              then listPackagesWithFiles db
              else apply context args db

apply :: Context -> CliArgs -> Db InstalledPackage -> IO ()
apply context args db = do
  packageConfig <- readPackageConfig args
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
