module PokerSpec where

import           Poker
import           Test.Hspec

spec :: Spec
spec = describe "Main de Poker" $ do

  it "fonctionne" $ do
    let main1 = []
        main2 = []

    mainDePoker main1 main2 `shouldBe` Egalite

  -- construire les combinaisons a partir d'une main
  -- definir ce que c'est qu'une carte
  --

  it "compare une seule carte" $ do
    let main1 = [card]
        main2 = [card2]
        card = Card As Coeur
        card2 = Card Deux Trefle

    mainDePoker main1 main2 `shouldBe` AGagne

  it "compare une seule carte" $ do
    let main1 = [card]
        main2 = [card2]
        card = Card Deux Trefle
        card2 = Card As Coeur

    mainDePoker main1 main2 `shouldBe` BGagne
