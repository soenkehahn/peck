{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use unless" #-}

module Peck.Package
  ( Package (..),
    InstalledPackage (..),
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
import Data.Yaml
import Dhall (FromDhall)
import GHC.Generics (Generic)
import Peck.Error
import Peck.OverlayFS (Command (..), withMountedImageFile)
import Peck.Utils
import System.Directory
import System.Environment
import System.FilePath (splitDirectories, takeDirectory, (</>))
import System.IO
import System.Posix.Files
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

instance ToJSON Package

instance FromDhall Package

data InstalledPackage = InstalledPackage
  { package :: Package,
    files :: [FilePath]
  }
  deriving stock (Show, Read, Eq)

data CopyPair = FilePath :-> FilePath
  deriving stock (Show, Eq)

infix 4 :->

source :: CopyPair -> FilePath
source (a :-> _) = a

target :: CopyPair -> FilePath
target (_ :-> b) = b

copy :: CopyPair -> IO ()
copy pair = do
  createDirectoryIfMissing True (takeDirectory $ target pair)
  symLink <- pathIsSymbolicLink (source pair)
  if symLink
    then do
      linkTarget <- getSymbolicLinkTarget (source pair)
      createFileLink linkTarget (target pair)
    else do
      copyFile (source pair) (target pair)

installPackage :: Package -> IO InstalledPackage
installPackage package = do
  withTempDir $ \((</> "install.sh") -> installScript) -> do
    writeFile installScript $ install package
    addExecutePermissions installScript
    withTempDir $ \buildDir -> do
      withCurrentDirectory buildDir $ do
        files <- withMountedImageFile (Script installScript) $ \overlay -> do
          case overlay of
            Left exitCode ->
              throwIO $
                PeckError
                  (Just exitCode)
                  ( "PECK ERROR: Install script for package '"
                      <> name package
                      <> "' failed, see errors above."
                  )
            Right overlay -> do
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
    throwIO $ peckError $ error <> ": " <> target pair

uninstall :: InstalledPackage -> IO ()
uninstall package =
  forM_ (files package) $ \file -> do
    addWritePermission file
    removeFile file
    removeEmptyParents file

addWritePermission :: FilePath -> IO ()
addWritePermission path = do
  isSymLink <- pathIsSymbolicLink path
  when (not isSymLink) $ do
    mode <- fileMode <$> getFileStatus path
    setFileMode path $ mode `unionFileModes` ownerWriteMode

removeEmptyParents :: FilePath -> IO ()
removeEmptyParents path = do
  let parent = takeDirectory path
  files <- readFiles parent
  when (null files) $ do
    removeDirectory parent
    removeEmptyParents parent
