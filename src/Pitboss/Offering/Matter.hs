module Pitboss.Offering.Matter
  ( defaultPitch,
    DeckCount (..),
    Matter (..),
  )
where

data DeckCount = D1 | D2 | D6 | D8
  deriving (Show, Eq)

data Pitch = FaceUp | FaceDown
  deriving (Show, Eq)

data Matter = Matter
  { matterDecks :: DeckCount,
    matterPitch :: Pitch
  }
  deriving (Show, Eq)

defaultPitch :: DeckCount -> Pitch
defaultPitch D1 = FaceDown
defaultPitch D2 = FaceDown
defaultPitch _ = FaceUp
