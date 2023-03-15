{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Peck.DbSpec where

import Control.Monad
import Data.Aeson (FromJSON, ToJSON (..), Value, eitherDecode')
import Data.String.Conversions (cs)
import Database.SQLite.Simple
import GHC.Generics (Generic)
import Peck.Db
import Peck.TestUtils (testContext)
import Test.Hspec
import Test.Mockery.Directory

data TestRecord = TestRecord
  { a :: Int,
    b :: String
  }
  deriving stock (Eq, Read, Show, Generic)

instance FromJSON TestRecord

instance ToJSON TestRecord

spec :: Spec
spec = do
  let ctx = testContext
  around_ inTempDirectory $ do
    describe "withDb" $ do
      it "initializes a non-existing db with the empty state" $ do
        withDb ctx "db" $ \(db :: Db ()) ->
          readDb db `shouldReturn` []

      it "doesn't modify existing db files" $ do
        withDb ctx "db" $ \(db :: Db Int) -> addElement db 42
        withDb ctx "db" (\(db :: Db Int) -> readDb db) `shouldReturn` [42]

    describe "readDb" $ do
      it "reads the db state" $ do
        withDb ctx "db" $ \(db :: Db Int) -> do
          addElement db 42
          readDb db `shouldReturn` [42]

      describe "old show-based list format" $ do
        let init = do
              writeFile "db" (show [42, 23 :: Int])
        before init $ do
          it "reads the old show format" $ do
            withDb ctx "db" readDb `shouldReturn` [42, 23 :: Int]

          it "converts to an sqlite table" $ do
            withDb ctx "db" $ \(_ :: Db Int) -> return ()
            withConnection "db" $ \connection -> do
              ns :: [Only Int] <- query_ connection "SELECT 1 + 1"
              ns `shouldBe` [Only 2]
              return ()

      describe "old show-based element format" $ do
        let testData = [TestRecord 42 "foo", TestRecord 23 "bar"]
            init = do
              withConnection "db" $ \connection -> do
                execute_
                  connection
                  "CREATE TABLE IF NOT EXISTS main (serialized TEXT)"
                forM_ testData $ \e -> do
                  execute
                    connection
                    "INSERT INTO main (serialized) VALUES (?)"
                    (Only (show e))
        before_ init $ do
          it "reads the old format" $ do
            withDb ctx "db" readDb `shouldReturn` testData

          it "converts to json" $ do
            withDb ctx "db" $ \(_ :: Db TestRecord) -> return ()
            withConnection "db" $ \connection -> do
              result :: [Only String] <- query_ connection "SELECT serialized FROM main"
              let expected :: [Either String Value] = map (Right . toJSON) testData
              map (eitherDecode' . cs . fromOnly) result `shouldBe` expected

    describe "removeElement" $ do
      it "removes elements" $ do
        withDb ctx "db" $ \db -> do
          addElement db (42 :: Int)
          addElement db 23
          removeElement db 42
          readDb db `shouldReturn` [23]
