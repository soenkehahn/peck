{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use unless" #-}

module Peck.Db
  ( Db,
    initialize,
    readDb,
    addElement,
    removeElement,
  )
where

import Control.Monad
import Development.Shake (cmd_)
import System.Directory
import System.FilePath

data Db a where
  Db :: (Show a, Read a, Eq a) => FilePath -> Db a

initialize :: (Show a, Read a, Eq a) => FilePath -> IO (Db a)
initialize path = do
  exists <- doesFileExist path
  when (not exists) $ do
    cmd_ "mkdir -p" $ takeDirectory path
    writeFile path "[]"
  return $ Db path

readDb :: Db a -> IO [a]
readDb (Db path) = do
  s <- readFile path
  seq (length s) (return ())
  return $ read s

addElement :: Db a -> a -> IO ()
addElement db@(Db path) a = do
  state <- readDb db
  writeFile path $ show (state ++ [a])

removeElement :: Db a -> a -> IO ()
removeElement db@(Db path) a = do
  state <- readDb db
  writeFile path $ show (filter (/= a) state)
