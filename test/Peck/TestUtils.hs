{-# LANGUAGE NamedFieldPuns #-}

module Peck.TestUtils where

import Control.Concurrent
import Development.Shake (cmd_)
import Peck.Context
import Peck.Package
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Unsafe
import Test.Hspec
import Test.Mockery.Directory
import Prelude hiding (log)

mkPackage :: String -> Package
mkPackage code =
  Package
    { name = "test package",
      skip = [],
      install = "#!/usr/bin/env bash\n\n" <> code
    }

mkSkipPackage :: [String] -> String -> Package
mkSkipPackage skip code =
  (mkPackage code) {skip}

wrapTests :: SpecWith FilePath -> Spec
wrapTests =
  around $ \test -> inTempDirectory $ do
    currentDir <- getCurrentDirectory
    cmd_ "mkdir test-home"
    setEnv "HOME" (currentDir </> "test-home")
    resetTestLogs
    test currentDir

{-# NOINLINE __testLogs__ #-}
__testLogs__ :: MVar [String]
__testLogs__ = unsafePerformIO $ newMVar []

resetTestLogs :: IO ()
resetTestLogs =
  modifyMVar_ __testLogs__ (const $ return [])

testContext :: Context
testContext =
  Context
    { log = \line -> modifyMVar_ __testLogs__ $ \acc -> do
        return $ line : acc
    }

readTestLogs :: IO String
readTestLogs =
  unlines
    . reverse
    <$> readMVar __testLogs__
