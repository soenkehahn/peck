{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Package
  ( Package (..),
    InstalledPackage (..),
    Error (..),
    installPackage,
    uninstall,
    _isSkipped,
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import Data.String
import Data.Yaml (FromJSON (parseJSON), Object, Parser, Value, withObject, (.!=), (.:), (.:?))
import Development.Shake (cmd, unit)
import GHC.Generics (Generic)
import OverlayFS (Command (..), withMountedImageFile)
import System.Directory
import System.Environment
import System.FilePath (splitDirectories, takeDirectory, (</>))
import System.IO
import Utils
import Prelude hiding (log)

data Package = Package
  { name :: String,
    skip :: [String],
    install :: String
  }
  deriving stock (Show, Read, Eq, Generic)

instance FromJSON Package where
  parseJSON :: Value -> Parser Package
  parseJSON = withObject "Package" $ \o ->
    Package
      <$> (o .: fromString "name")
      <*> parseSkip o .!= []
      <*> (o .: fromString "install")
    where
      parseSkip :: Object -> Parser (Maybe [String])
      parseSkip o =
        (o .:? fromString "skip")
          <|> (fmap pure <$> o .: fromString "skip")

data InstalledPackage = InstalledPackage
  { package :: Package,
    files :: [FilePath]
  }
  deriving stock (Show, Read, Eq)

newtype Error = Error String
  deriving stock (Show, Eq)

instance Exception Error

data CopyPair = FilePath :-> FilePath

infix 4 :->

source :: CopyPair -> FilePath
source (a :-> _) = a

target :: CopyPair -> FilePath
target (_ :-> b) = b

copy :: CopyPair -> IO ()
copy pair = do
  unit $ cmd "mkdir -p" (takeDirectory $ target pair)
  unit $ cmd "cp" (source pair) (target pair)

installPackage :: Package -> IO InstalledPackage
installPackage package = do
  withTempDir $ \((</> "install.sh") -> installScript) -> do
    writeFile installScript $ install package
    unit $ cmd "chmod +x" installScript
    withTempDir $ \buildDir -> do
      withCurrentDirectory buildDir $ do
        files <- withMountedImageFile (Script installScript) $ \overlay -> do
          files <- listFilesFromOverlay buildDir package overlay
          forM_ files $ throwOnFileClash "file already exists"
          forM files $ \pair -> do
            throwOnFileClash "PANIC: while installing package, found existing file" pair
            copy pair
            return $ target pair
        return $ InstalledPackage package files

listFilesFromOverlay :: FilePath -> Package -> FilePath -> IO [CopyPair]
listFilesFromOverlay buildDir package overlay = do
  filesInOverlay <- readFilesRecursively overlay
  let copyPairs = map (\file -> overlay </> file :-> "/" </> file) filesInOverlay
      withoutTemporaryBuildDir = filter (not . (buildDir `isPrefixOf`) . target) copyPairs
  filterM (fmap not . _isSkipped package . target) withoutTemporaryBuildDir

_isSkipped :: Package -> FilePath -> IO Bool
_isSkipped package (splitDirectories -> path) = do
  patterns <- mapM (expandTilde . splitDirectories) $ skip package
  return $ any (`isPrefixOf` path) patterns

expandTilde :: [String] -> IO [String]
expandTilde = \case
  "~" : rest -> do
    home <- getEnv "HOME"
    return $ splitDirectories home ++ rest
  pattern -> return pattern

throwOnFileClash :: String -> CopyPair -> IO ()
throwOnFileClash error pair = do
  exists <- doesFileExist $ target pair
  when exists $ do
    throwIO $ Error $ error <> ": " <> target pair

uninstall :: InstalledPackage -> IO ()
uninstall package =
  forM_ (files package) $ \file -> do
    unit $ cmd "rm" file
    removeEmptyParents file

removeEmptyParents :: FilePath -> IO ()
removeEmptyParents path = do
  let parent = takeDirectory path
  files <- readFiles parent
  when (null files) $ do
    unit $ cmd "rm -rf" parent
    removeEmptyParents parent
