module Pitboss.Blackjack.Types.Offering where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Blackjack.Types.GameRuleSet (GameRuleSet)
import Pitboss.Blackjack.Types.Table

data Offering = Offering
    { materia :: Materia
    , gameRuleSet :: GameRuleSet
    , tableRuleSet :: TableRuleSet
    }
    deriving (Eq, Show, Generic)

instance ToJSON Offering
instance FromJSON Offering

mkOffering :: Materia -> GameRuleSet -> TableRuleSet -> Offering
mkOffering m g t = Offering{materia = m, gameRuleSet = g, tableRuleSet = t}

data DeckCount = D1 | D2 | D6 | D8
    deriving (Show, Eq, Generic)

data Dealt = Pitch | FaceUp
    deriving (Show, Eq, Generic)

data Materia = Materia
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
instance ToJSON Materia
instance FromJSON Materia
