{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use unless" #-}

module Peck.Db
  ( Db,
    withDb,
    readDb,
    addElement,
    removeElement,
  )
where

import Control.Monad
import Data.Proxy (Proxy (..))
import Database.SQLite.Simple
import System.Directory
import System.FilePath
import Text.Read (readMaybe)

data Db a where
  Db :: (Show a, Read a, Eq a) => Connection -> Db a

withDb :: forall a b. (Show a, Read a, Eq a) => FilePath -> (Db a -> IO b) -> IO b
withDb path action = do
  migrate (Proxy :: Proxy a) path
  createDirectoryIfMissing True $ takeDirectory path
  withConnection path $ \connection -> do
    execute_ connection "CREATE TABLE IF NOT EXISTS main (serialized TEXT)"
    action $ Db connection

readDb :: Db a -> IO [a]
readDb (Db connection) = do
  s :: [Only String] <- query_ connection "SELECT serialized FROM main"
  seq (length s) (return ())
  return $ map (read . fromOnly) s

addElement :: Db a -> a -> IO ()
addElement (Db connection) a = do
  execute connection "INSERT INTO main (serialized) VALUES (?)" (Only (show a))

removeElement :: Db a -> a -> IO ()
removeElement (Db connection) a = do
  execute connection "DELETE FROM main WHERE serialized = ?" (Only (show a))

migrate :: forall a. (Read a, Show a, Eq a) => Proxy a -> FilePath -> IO ()
migrate Proxy path = do
  exists <- doesFileExist path
  when exists $ do
    maybeContents :: Maybe [a] <- readMaybe <$> readFile path
    case maybeContents of
      Nothing -> return ()
      Just elements -> do
        seq (length elements) (return ())
        removeFile path
        withDb path $ \db -> do
          mapM_ (addElement db) elements
