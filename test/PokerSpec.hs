module PokerSpec where

import           Test.Hspec

mainDePoker main1 main2 = Egalite

data Vainqueur = Egalite

instance Show Vainqueur where
  show Egalite = "Egalite"

instance Eq Vainqueur where
  Egalite == Egalite = True

spec :: Spec
spec = describe "Main de Poker" $ do

  it "fonctionne" $ do
    let main1 = []
        main2 = []

    mainDePoker main1 main2 `shouldBe` Egalite
