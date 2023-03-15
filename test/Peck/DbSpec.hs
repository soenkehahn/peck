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
    describe "initialize" $ do
      it "initializes a non-existing db with the empty state" $ do
        db :: Db [()] <- initialize "db"
        readDb db `shouldReturn` []

      it "doesn't modify existing db files" $ do
        db :: Db Int <- initialize "db"
        addElement db 42
        db :: Db Int <- initialize "db"
        readDb db `shouldReturn` [42]

    describe "readDb" $ do
      it "reads the db state" $ do
        db :: Db Int <- initialize "db"
        addElement db 42
        readDb db `shouldReturn` [42]

      describe "old show-based format" $ do
        it "converts to an sqlite table" $ do
          writeFile "db" (show [42, 23 :: Int])
          _ :: Db Int <- initialize "db"
          withConnection "db" $ \connection -> do
            ns :: [Only Int] <- query_ connection "SELECT 1 + 1"
            ns `shouldBe` [Only 2]
            return ()

        it "reads the old show format" $ do
          writeFile "db" (show [42, 23 :: Int])
          db :: Db Int <- initialize "db"
          readDb db `shouldReturn` [42, 23]

    describe "removeElement" $ do
      it "removes elements" $ do
        db :: Db Int <- initialize "db"
        addElement db 42
        addElement db 23
        removeElement db 42
        readDb db `shouldReturn` [23]
