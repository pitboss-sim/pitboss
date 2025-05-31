{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.Agency.Archetype.Player where

import Data.Aeson (FromJSON, ToJSON, object, toJSON, withObject, (.:), (.=))
import Data.Aeson.Types (Parser, parseJSON)
import GHC.Generics (Generic)
import Pitboss.Strategy.Chart.Types (StrategyChart)

-- Player archetypes
data PlayerArchetype t where
    BasicStrategyPlayer :: StrategyChart -> MistakeProfile -> PlayerArchetype 'UseChart
    PerfectPlayer :: PlayerArchetype 'UseEV
    AdvantagePlayer :: StrategyChart -> CountingConfig -> BettingSpread -> PlayerArchetype 'UseCount

data PlayerStrategy = UseChart | UseEV | UseCount

-- Mistake configuration for basic strategy players
data MistakeProfile = MistakeProfile
    { mistakeRate :: Double -- 0.0 to 1.0
    , mistakeDistribution :: MistakeDistribution
    }
    deriving (Eq, Show, Generic)

data MistakeDistribution = MistakeDistribution
    { hitInsteadOfStand :: Double
    , standInsteadOfHit :: Double
    , noDoubleWhenShould :: Double
    , noSplitWhenShould :: Double
    }
    deriving (Eq, Show, Generic)

-- Counting configuration for advantage players
data CountingConfig = CountingConfig
    { countingSystem :: CountingSystem
    , countingAccuracy :: Double -- 0.0 to 1.0, affects error rate
    }
    deriving (Eq, Show, Generic)

data CountingSystem
    = HiLo
    | KO
    | OmegaII
    deriving (Eq, Show, Generic)

-- Betting configuration for advantage players
data BettingSpread
    = FlatBetting
    | SimpleSpread Double Double -- min/max units
    | TrueCountSpread [(Int, Double)] -- TC thresholds and units
    deriving (Eq, Show, Generic)

data SomePlayerArchetype where
    SomePlayerArchetype :: PlayerArchetype t -> SomePlayerArchetype

-- Helper constructors
perfectBasicStrategy :: StrategyChart -> PlayerArchetype 'UseChart
perfectBasicStrategy chart = BasicStrategyPlayer chart noMistakes

noMistakes :: MistakeProfile
noMistakes = MistakeProfile 0.0 uniformDistribution

uniformDistribution :: MistakeDistribution
uniformDistribution = MistakeDistribution 0.25 0.25 0.25 0.25

-- Instances
instance Eq SomePlayerArchetype where
    SomePlayerArchetype (BasicStrategyPlayer _ m1) == SomePlayerArchetype (BasicStrategyPlayer _ m2) = m1 == m2
    SomePlayerArchetype PerfectPlayer == SomePlayerArchetype PerfectPlayer = True
    SomePlayerArchetype (AdvantagePlayer _ c1 b1) == SomePlayerArchetype (AdvantagePlayer _ c2 b2) = c1 == c2 && b1 == b2
    _ == _ = False

instance Show SomePlayerArchetype where
    show (SomePlayerArchetype (BasicStrategyPlayer _ mistakes)) =
        "BasicStrategyPlayer (mistake rate: " ++ show (mistakeRate mistakes) ++ ")"
    show (SomePlayerArchetype PerfectPlayer) = "PerfectPlayer"
    show (SomePlayerArchetype (AdvantagePlayer _ config _)) =
        "AdvantagePlayer (" ++ show (countingSystem config) ++ ")"

instance ToJSON MistakeProfile
instance FromJSON MistakeProfile
instance ToJSON MistakeDistribution
instance FromJSON MistakeDistribution
instance ToJSON CountingConfig
instance FromJSON CountingConfig
instance ToJSON CountingSystem
instance FromJSON CountingSystem
instance ToJSON BettingSpread
instance FromJSON BettingSpread

instance ToJSON SomePlayerArchetype where
    toJSON (SomePlayerArchetype (BasicStrategyPlayer _ mistakes)) =
        object
            [ "type" .= ("BasicStrategy" :: String)
            , "mistakes" .= mistakes
            ]
    toJSON (SomePlayerArchetype PerfectPlayer) =
        object ["type" .= ("Perfect" :: String)]
    toJSON (SomePlayerArchetype (AdvantagePlayer _ counting betting)) =
        object
            [ "type" .= ("Advantage" :: String)
            , "counting" .= counting
            , "betting" .= betting
            ]

instance FromJSON SomePlayerArchetype where
    parseJSON = withObject "SomePlayerArchetype" $ \o -> do
        typ <- o .: "type" :: Parser String
        case typ of
            "BasicStrategy" -> do
                mistakes <- o .: "mistakes"
                pure $ SomePlayerArchetype (BasicStrategyPlayer [] mistakes)
            "Perfect" -> pure $ SomePlayerArchetype PerfectPlayer
            "Advantage" -> do
                counting <- o .: "counting"
                betting <- o .: "betting"
                pure $ SomePlayerArchetype (AdvantagePlayer [] counting betting)
            _ -> fail $ "Unknown player archetype: " ++ typ
