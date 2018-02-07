module Poker where


mainDePoker []    []                                  = Egalite
mainDePoker [Card value1 color1] [Card value2 color2] =
  case compare value1 value2 of
    LT -> BGagne
    EQ -> Egalite
    GT -> AGagne

data Vainqueur = Egalite | AGagne | BGagne
  deriving (Eq, Show)

data Color = Coeur | Trefle | Pique
  deriving (Eq, Show)

data Value = Deux | As
  deriving (Ord, Eq, Show)

data Card = Card Value Color
  deriving (Eq, Show)
