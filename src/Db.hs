{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fmap" #-}
{-# HLINT ignore "Use unless" #-}

module Db where

import Control.Monad
import System.Directory

withState_ :: (Show state, Read state) => FilePath -> state -> (state -> IO state) -> IO ()
withState_ dbPath initial action = do
  withState dbPath initial $ \state ->
    ((),) <$> action state

withState :: (Show state, Read state) => FilePath -> state -> (state -> IO (a, state)) -> IO a
withState dbPath initial action = do
  exists <- doesFileExist dbPath
  when (not exists) $ do
    writeFile dbPath (show initial)
  state <- readFile dbPath
  seq (length state) (return ())
  (a, next) <- action (read state)
  let nextString = show next
  seq (length nextString) (return ())
  writeFile dbPath nextString
  return a
