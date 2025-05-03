module Pitboss.Blackjack.Offering.Matter where

data DeckCount = D1 | D2 | D6 | D8
  deriving (Show, Eq)

data Dealt = Pitch | FaceUp
  deriving (Show, Eq)

data Matter = Matter
  { matterDecks :: DeckCount,
    matterDealt :: Dealt
  }
  deriving (Show, Eq)

defaultDealt :: DeckCount -> Dealt
defaultDealt D1 = Pitch
defaultDealt D2 = Pitch
defaultDealt _ = FaceUp
