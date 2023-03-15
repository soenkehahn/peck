{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fmap" #-}
{-# HLINT ignore "Use unless" #-}

module Peck.Db
  ( Db,
    withDb,
    readDb,
    addElement,
    removeElement,
  )
where

import Control.Exception (throwIO)
import Control.Monad
import Data.Aeson (FromJSON, ToJSON, eitherDecode', encode)
import Data.Proxy (Proxy (..))
import Data.String.Conversions
import Database.SQLite.Simple
import Peck.Error (peckError)
import System.Directory
import System.FilePath
import System.IO
import Text.Read (readMaybe)

data Db a where
  Db :: (Eq a, Show a, Read a, FromJSON a, ToJSON a) => Connection -> Db a

withDb ::
  forall a b.
  (Eq a, Show a, Read a, ToJSON a, FromJSON a) =>
  FilePath ->
  (Db a -> IO b) ->
  IO b
withDb path action = do
  migrate (Proxy :: Proxy a) path
  createDirectoryIfMissing True $ takeDirectory path
  withConnection path $ \connection -> do
    execute_ connection "CREATE TABLE IF NOT EXISTS main (serialized TEXT)"
    action $ Db connection

readDb :: Db a -> IO [a]
readDb (Db connection) = do
  rawElements :: [Only String] <- query_ connection "SELECT serialized FROM main"
  seq (length rawElements) (return ())
  forM rawElements $ \rawElement -> do
    case eitherDecode' (cs (fromOnly rawElement)) of
      Right element -> return element
      Left error -> throwIO $ peckError ("Error decoding JSON from database: " <> error)

addElement :: Db a -> a -> IO ()
addElement (Db connection) a = do
  execute
    connection
    "INSERT INTO main (serialized) VALUES (?)"
    (Only (cs (encode a) :: String))

removeElement :: Db a -> a -> IO ()
removeElement (Db connection) a = do
  execute
    connection
    "DELETE FROM main WHERE serialized = ?"
    (Only (cs (encode a) :: String))

migrate ::
  forall a.
  (Read a, Show a, Eq a, ToJSON a) =>
  Proxy a ->
  FilePath ->
  IO ()
migrate Proxy path = do
  exists <- doesFileExist path
  when exists $ do
    forM_ migrations $ \(migration :: Migration a) -> do
      maybeContents :: Maybe [a] <- readAll migration path
      case maybeContents of
        Nothing -> return ()
        Just elements -> do
          -- todo: use Context
          hPutStrLn stderr ("performing migration: " ++ name migration)
          seq (length elements) (return ())
          removeFile path
          writeAll migration path elements

data Migration a = Migration
  { name :: String,
    readAll :: FilePath -> IO (Maybe [a]),
    writeAll :: FilePath -> [a] -> IO ()
  }

migrations :: forall a. (Read a, Show a, ToJSON a) => [Migration a]
migrations =
  [ Migration
      { name = "introduce sqlite",
        readAll = \path -> readMaybe <$> readFile path,
        writeAll = \path elements -> do
          withConnection path $ \connection -> do
            execute_
              connection
              "CREATE TABLE IF NOT EXISTS main (serialized TEXT)"
            forM_ elements $ \element -> do
              execute
                connection
                "INSERT INTO main (serialized) VALUES (?)"
                (Only (show element))
      },
    Migration
      { name = "switch to JSON",
        readAll = \path -> do
          withConnection path $ \connection -> do
            mapM (readMaybe . fromOnly)
              <$> query_ connection "SELECT serialized FROM main",
        writeAll = \path elements -> do
          withConnection path $ \connection -> do
            execute_
              connection
              "CREATE TABLE IF NOT EXISTS main (serialized TEXT)"
            forM_ elements $ \element -> do
              execute
                connection
                "INSERT INTO main (serialized) VALUES (?)"
                (Only (cs (encode element) :: String))
      }
  ]
