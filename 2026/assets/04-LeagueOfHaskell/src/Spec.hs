module Spec where
import PdePreludat
import Library
import Test.Hspec

campeonNuevo :: Campeon
campeonNuevo = UnCampeon 0 8 TOP ["Ruptura", "Grito salvaje", "Clavos vorpalinos", "Festín"]

correrTests :: IO ()
correrTests = hspec $ do
  describe "Campeon" $ do
    describe "estaEnEarly" $ do
      it "cuando recibe un campeon con nivel 6 deberia dar false" $ do
        estaEnEarly (setNivel 6 chogat) `shouldBe` False
      it "cuando recibe un campeon con nivel 5 deberia dar true" $ do
        estaEnEarly (subirNivel jinx) `shouldBe` True
    describe "estaRoto" $ do
      it "cuando recibe un personaje con antiguedad 0 deberia dar true" $ do
        estaRoto campeonNuevo `shouldBe` True
  describe "Equipo" $ do
    describe "puedeGanar" $ do
      it "cuando recibe un equipo con un TOP devuelve True" $ do
        puedeGanar [chogat] `shouldBe` True