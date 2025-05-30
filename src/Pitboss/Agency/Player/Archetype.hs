{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.Agency.Player.Archetype where

import Data.Aeson (FromJSON, ToJSON, (.:), (.=), object, withObject, toJSON)
import Data.Aeson.Types (Parser, parseJSON)
import GHC.Generics (Generic)
import Pitboss.Strategy.Chart.Types (StrategyChart)

-- Player archetypes
data PlayerArchetype t where
    BasicStrategyPlayer :: StrategyChart -> PlayerArchetype 'UseChart
    PerfectPlayer :: PlayerArchetype 'UseEV
    AdvantagePlayer :: StrategyChart -> CountingSystem -> PlayerArchetype 'UseCount

data CountingSystem
    = HiLo
    | KO
    | OmegaII
    deriving (Eq, Show, Generic)

data PlayerStrategy = UseChart | UseEV | UseCount

data SomePlayerArchetype where
    SomePlayerArchetype :: PlayerArchetype t -> SomePlayerArchetype

instance Eq SomePlayerArchetype where
    SomePlayerArchetype (BasicStrategyPlayer _) == SomePlayerArchetype (BasicStrategyPlayer _) = True
    SomePlayerArchetype PerfectPlayer == SomePlayerArchetype PerfectPlayer = True
    SomePlayerArchetype (AdvantagePlayer _ s1) == SomePlayerArchetype (AdvantagePlayer _ s2) = s1 == s2
    _ == _ = False

instance Show SomePlayerArchetype where
    show (SomePlayerArchetype (BasicStrategyPlayer _)) = "BasicStrategyPlayer"
    show (SomePlayerArchetype PerfectPlayer) = "PerfectPlayer"
    show (SomePlayerArchetype (AdvantagePlayer _ system)) = "AdvantagePlayer (" ++ show system ++ ")"

instance ToJSON CountingSystem
instance FromJSON CountingSystem

instance ToJSON SomePlayerArchetype where
    toJSON (SomePlayerArchetype (BasicStrategyPlayer _)) =
        object ["type" .= ("BasicStrategy" :: String)]
    toJSON (SomePlayerArchetype PerfectPlayer) =
        object ["type" .= ("Perfect" :: String)]
    toJSON (SomePlayerArchetype (AdvantagePlayer _ system)) =
        object ["type" .= ("Advantage" :: String), "system" .= system]

instance FromJSON SomePlayerArchetype where
    parseJSON = withObject "SomePlayerArchetype" $ \o -> do
        typ <- o .: "type" :: Parser String
        case typ of
            "BasicStrategy" -> pure $ SomePlayerArchetype (BasicStrategyPlayer [])
            "Perfect" -> pure $ SomePlayerArchetype PerfectPlayer
            "Advantage" -> do
                system <- o .: "system"
                pure $ SomePlayerArchetype (AdvantagePlayer [] system)
            _ -> fail $ "Unknown player archetype: " ++ typ
