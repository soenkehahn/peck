{-# LANGUAGE ScopedTypeVariables #-}

module Peck.DbSpec where

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

    it "reads the db state" $ do
      db :: Db Int <- initialize "db"
      addElement db 42
      readDb db `shouldReturn` [42]

    describe "removeElement" $ do
      it "removes elements" $ do
        db :: Db Int <- initialize "db"
        addElement db 42
        addElement db 23
        removeElement db 42
        readDb db `shouldReturn` [23]
