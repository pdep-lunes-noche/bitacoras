module Spec where
import PdePreludat
import Library
import Test.Hspec



correrTests :: IO ()
correrTests = hspec $ do
  describe "Barbaro" $ do
    describe "ardilla" $ do
      it "cuando recibe un barbaro me devuelve el mismo barbaro" $ do
        (nombre (ardilla dave)) `shouldBe` (nombre dave)
