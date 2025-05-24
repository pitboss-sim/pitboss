module Pitboss.Blackjack.Offering.Matter where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data DeckCount = D1 | D2 | D6 | D8
    deriving (Show, Eq, Generic)

data Dealt = Pitch | FaceUp
    deriving (Show, Eq, Generic)

data Matter = Matter
    { matterDecks :: DeckCount
    , matterDealt :: Dealt
    }
    deriving (Show, Eq, Generic)

defaultDealt :: DeckCount -> Dealt
defaultDealt D1 = Pitch
defaultDealt D2 = Pitch
defaultDealt _ = FaceUp

instance ToJSON DeckCount
instance FromJSON DeckCount

instance ToJSON Dealt
instance FromJSON Dealt

instance ToJSON Matter
instance FromJSON Matter

