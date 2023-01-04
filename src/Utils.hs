module Utils where

import Control.Exception
import Control.Monad
import Data.Char (isSpace)
import Data.List
import Development.Shake (cmd, unit)
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

-- 'withSystemTempDirectory' somehow doesn't work, maybe because of the mounts?
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = do
  systemTempDir <- getCanonicalTemporaryDirectory
  bracket (createTempDirectory systemTempDir "packager") removeDir action
  where
    removeDir dir = do
      unit $ cmd "chmod u+rwX -R" dir
      unit $ cmd "rm" dir "-rf"

readFilesRecursively :: FilePath -> IO [FilePath]
readFilesRecursively dir = do
  directChildren <- readFiles dir
  fmap mconcat $ forM directChildren $ \child -> do
    isDir <- doesDirectoryExist (dir </> child)
    if isDir
      then sort . map (child </>) <$> readFilesRecursively (dir </> child)
      else return [child]

readFiles :: FilePath -> IO [FilePath]
readFiles dir = filter (not . (`elem` [".", ".."])) <$> getDirectoryContents dir

stripSpaces :: String -> String
stripSpaces = dropWhile isSpace . reverse . dropWhile isSpace . reverse

deb :: Show a => a -> IO ()
deb = hPrint stderr
