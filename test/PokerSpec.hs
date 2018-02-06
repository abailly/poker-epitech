module PokerSpec where

import           Test.Hspec

mainDePoker []    []    = Egalite
mainDePoker card1 card2 = AGagne

data Vainqueur = Egalite | AGagne
  deriving (Eq, Show)

data Color = Coeur | Trefle
  deriving (Eq, Show)

data Value = As  | Deux
  deriving (Eq, Show)

data Card = Card Value Color
  deriving (Eq, Show)

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
