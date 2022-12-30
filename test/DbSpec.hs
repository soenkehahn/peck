module DbSpec where

import Db
import Test.Hspec
import Test.Mockery.Directory

spec :: Spec
spec = do
  around_ inTempDirectory $ do
    describe "withState" $ do
      it "initializes a non-existing db with the default state" $ do
        withState_ "db" "initial" $ \state -> do
          state `shouldBe` "initial"
          return state

      it "writes a modified state to disk" $ do
        withState_ "db" "initial" $ \_ -> do
          return "next"
        withState_ "db" "initial" $ \state -> do
          state `shouldBe` "next"
          return state
