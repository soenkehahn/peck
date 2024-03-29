{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Peck.OverlayFS
  ( Command (..),
    withMountedImageFile,
  )
where

import Data.String.Interpolate
import Data.String.Interpolate.Util
import Peck.Utils
import System.Directory
import System.Exit
import System.FilePath
import System.Process

newtype Command = Script FilePath

withMountedImageFile :: Command -> (Either ExitCode FilePath -> IO a) -> IO a
withMountedImageFile command action = do
  withTempDir $ \tempDir -> do
    upper <- performInOverlayFS tempDir command
    action upper

performInOverlayFS :: FilePath -> Command -> IO (Either ExitCode FilePath)
performInOverlayFS tempDir (Script path) = do
  workingDir <- getCurrentDirectory
  path <- makeAbsolute path
  innerScript <-
    writeScript tempDir "inner.sh" $
      unindent
        [i|
          mkdir -p #{tempDir}/new-root
          mkdir -p #{tempDir}/upper
          mkdir -p #{tempDir}/workdir
          mkdir -p #{tempDir}/overlay

          mount.mergerfs /=RO #{tempDir}/new-root \\
            -o allow_other,use_ino,cache.files=partial,dropcacheonclose=true,category.create=mfs

          mkdir -p #{tempDir}/upper$HOME
          mkdir -p #{tempDir}/upper/tmp

          mount -t overlay overlay #{tempDir}/overlay \\
            -o lowerdir=#{tempDir}/new-root,upperdir=#{tempDir}/upper,workdir=#{tempDir}/workdir

          mount --rbind /dev #{tempDir}/overlay/dev
          mount --rbind /proc #{tempDir}/overlay/proc

          unshare -Umr --root #{tempDir}/overlay --wd #{workingDir} #{path}
        |]
  outerScript <- writeScript tempDir "outer.sh" ("unshare -Umr " <> innerScript)
  exitCode <- runScript outerScript
  return $ case exitCode of
    ExitFailure _ -> Left exitCode
    ExitSuccess -> Right $ tempDir </> "upper"

runScript :: FilePath -> IO ExitCode
runScript script = do
  (_, _, _, handle) <- createProcess (proc script [])
  waitForProcess handle

writeScript :: FilePath -> FilePath -> String -> IO FilePath
writeScript dir path code = do
  let script = dir </> path
  writeFile script $
    unindent
      [i|
        #!/usr/bin/env bash

        set -eu

        #{code}
      |]
  addExecutePermissions script
  return script
