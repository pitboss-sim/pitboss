{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

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
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

mkOffering :: Materia -> GameRuleSet -> TableRuleSet -> Offering
mkOffering m g t = Offering{materia = m, gameRuleSet = g, tableRuleSet = t}

data DeckCount = D1 | D2 | D6 | D8
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Dealt = Pitch | FaceUp
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Materia = Materia
    { matterDecks :: DeckCount
    , matterDealt :: Dealt
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

defaultDealt :: DeckCount -> Dealt
defaultDealt D1 = Pitch
defaultDealt D2 = Pitch
defaultDealt _ = FaceUp
