{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Peck.DbSpec where

import Database.SQLite.Simple
import Peck.Db
import Test.Hspec
import Test.Mockery.Directory

spec :: Spec
spec = do
  around_ inTempDirectory $ do
    describe "withDb" $ do
      it "initializes a non-existing db with the empty state" $ do
        withDb "db" $ \(db :: Db ()) ->
          readDb db `shouldReturn` []

      it "doesn't modify existing db files" $ do
        withDb "db" $ \(db :: Db Int) -> addElement db 42
        withDb "db" (\(db :: Db Int) -> readDb db) `shouldReturn` [42]

    describe "readDb" $ do
      it "reads the db state" $ do
        withDb "db" $ \(db :: Db Int) -> do
          addElement db 42
          readDb db `shouldReturn` [42]

      describe "old show-based format" $ do
        it "converts to an sqlite table" $ do
          writeFile "db" (show [42, 23 :: Int])
          withDb "db" $ \(_ :: Db Int) -> return ()
          withConnection "db" $ \connection -> do
            ns :: [Only Int] <- query_ connection "SELECT 1 + 1"
            ns `shouldBe` [Only 2]
            return ()

        it "reads the old show format" $ do
          writeFile "db" (show [42, 23 :: Int])
          withDb "db" readDb `shouldReturn` [42, 23 :: Int]

    describe "removeElement" $ do
      it "removes elements" $ do
        withDb "db" $ \db -> do
          addElement db (42 :: Int)
          addElement db 23
          removeElement db 42
          readDb db `shouldReturn` [23]
