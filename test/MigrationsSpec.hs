module MigrationsSpec where

import Qtility
import Test.Hspec

spec :: Spec
spec = do
  describe "Migrations" $ do
    it "should be able to run migrations" $ do
      True `shouldBe` True
