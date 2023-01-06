{-# LANGUAGE NamedFieldPuns #-}

module TestUtils where

import Context
import Control.Concurrent
import Package
import System.Directory
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
