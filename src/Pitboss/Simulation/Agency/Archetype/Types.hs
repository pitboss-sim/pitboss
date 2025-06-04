{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Simulation.Agency.Archetype.Types where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Map (Map)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Strategy.Chart

data PlayerArchetypeKind
    = BasicStrategy
    | Perfect
    | Advantage
    | Superstitious
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

data DealerArchetypeKind
    = ByTheBook
    | Rookie
    | Veteran
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

data family PlayerArchetype (k :: PlayerArchetypeKind)
data family DealerArchetype (k :: DealerArchetypeKind)

data family ArchetypeConfig (k :: PlayerArchetypeKind)
data family ArchetypeState (k :: PlayerArchetypeKind)

data family DealerArchetypeConfig (k :: DealerArchetypeKind)
data family DealerArchetypeState (k :: DealerArchetypeKind)

data instance PlayerArchetype 'BasicStrategy = BasicStrategyArchetype
    { bsConfig :: ArchetypeConfig 'BasicStrategy
    , bsState :: ArchetypeState 'BasicStrategy
    }
    deriving (Show, Eq, Generic)

data instance PlayerArchetype 'Perfect = PerfectArchetype
    { pfConfig :: ArchetypeConfig 'Perfect
    , pfState :: ArchetypeState 'Perfect
    }
    deriving (Show, Eq, Generic)

data instance PlayerArchetype 'Advantage = AdvantageArchetype
    { advConfig :: ArchetypeConfig 'Advantage
    , advState :: ArchetypeState 'Advantage
    }
    deriving (Show, Eq, Generic)

data instance PlayerArchetype 'Superstitious = SuperstitiousArchetype
    { ssConfig :: ArchetypeConfig 'Superstitious
    , ssState :: ArchetypeState 'Superstitious
    }
    deriving (Show, Eq, Generic)

data instance ArchetypeConfig 'BasicStrategy = BasicConfig
    { bcStrategyChart :: StrategyChart
    , bcMistakeProfile :: MistakeProfile
    }
    deriving (Show, Eq, Generic)

data instance ArchetypeConfig 'Perfect = PerfectConfig
    { pcUseEV :: Bool
    }
    deriving (Show, Eq, Generic)

data instance ArchetypeConfig 'Advantage = AdvantageConfig
    { acCountingSystem :: CountingSystem
    , acBettingSpread :: BettingSpread
    , acDeviationChart :: DeviationChart
    }
    deriving (Show, Eq, Generic)

data instance ArchetypeConfig 'Superstitious = SuperstitionConfig
    { scBeliefs :: [Superstition]
    , scFallbackChart :: StrategyChart
    }
    deriving (Show, Eq, Generic)

data instance ArchetypeState 'BasicStrategy = BasicState
    { bsMistakesMade :: Int
    , bsSessionStats :: SessionStats
    }
    deriving (Show, Eq, Generic)

data instance ArchetypeState 'Perfect = PerfectState
    { psHandsPlayed :: Int
    , psSessionStats :: SessionStats
    }
    deriving (Show, Eq, Generic)

data instance ArchetypeState 'Advantage = AdvantageState
    { asRunningCount :: Int
    , asTrueCount :: Double
    , asDecksRemaining :: Double
    , asSessionStats :: SessionStats
    }
    deriving (Show, Eq, Generic)

data instance ArchetypeState 'Superstitious = SuperstitionState
    { ssHandsPlayed :: Int
    , ssBeliefsForcedMoves :: Int
    , ssSessionStats :: SessionStats
    }
    deriving (Show, Eq, Generic)

data instance DealerArchetype 'ByTheBook = ByTheBookDealerArchetype
    { btbConfig :: DealerArchetypeConfig 'ByTheBook
    , btbState :: DealerArchetypeState 'ByTheBook
    }
    deriving (Show, Eq, Generic)

data instance DealerArchetype 'Rookie = RookieDealerArchetype
    { rkConfig :: DealerArchetypeConfig 'Rookie
    , rkState :: DealerArchetypeState 'Rookie
    }
    deriving (Show, Eq, Generic)

data instance DealerArchetype 'Veteran = VeteranDealerArchetype
    { vtConfig :: DealerArchetypeConfig 'Veteran
    , vtState :: DealerArchetypeState 'Veteran
    }
    deriving (Show, Eq, Generic)

data instance DealerArchetypeConfig 'ByTheBook = ByTheBookConfig
    { btbPenetration :: PenetrationProfile
    , btbPace :: PaceProfile
    }
    deriving (Show, Eq, Generic)

data instance DealerArchetypeConfig 'Rookie = RookieConfig
    { rkPenetration :: PenetrationProfile
    , rkErrors :: ErrorProfile
    , rkPace :: PaceProfile
    }
    deriving (Show, Eq, Generic)

data instance DealerArchetypeConfig 'Veteran = VeteranConfig
    { vtPenetration :: PenetrationProfile
    , vtPace :: PaceProfile
    , vtTellProfile :: TellProfile
    }
    deriving (Show, Eq, Generic)

data instance DealerArchetypeState 'ByTheBook = ByTheBookState
    { btbRoundsDealt :: Int
    }
    deriving (Show, Eq, Generic)

data instance DealerArchetypeState 'Rookie = RookieState
    { rkRoundsDealt :: Int
    , rkErrorsMade :: Int
    }
    deriving (Show, Eq, Generic)

data instance DealerArchetypeState 'Veteran = VeteranState
    { vtRoundsDealt :: Int
    , vtTellsGiven :: Int
    }
    deriving (Show, Eq, Generic)

data SomePlayerArchetype where
    SomePlayerBasicStrategy :: PlayerArchetype 'BasicStrategy -> SomePlayerArchetype
    SomePlayerPerfect :: PlayerArchetype 'Perfect -> SomePlayerArchetype
    SomePlayerAdvantage :: PlayerArchetype 'Advantage -> SomePlayerArchetype
    SomePlayerSuperstitious :: PlayerArchetype 'Superstitious -> SomePlayerArchetype

deriving instance Show SomePlayerArchetype
deriving instance Eq SomePlayerArchetype

data SomeDealerArchetype where
    SomeDealerByTheBook :: DealerArchetype 'ByTheBook -> SomeDealerArchetype
    SomeDealerRookie :: DealerArchetype 'Rookie -> SomeDealerArchetype
    SomeDealerVeteran :: DealerArchetype 'Veteran -> SomeDealerArchetype

deriving instance Show SomeDealerArchetype
deriving instance Eq SomeDealerArchetype

data MistakeProfile = MistakeProfile
    { _mistakeRate :: Double
    , _mistakeDistribution :: MistakeDistribution
    }
    deriving (Eq, Show, Generic)

data MistakeDistribution = MistakeDistribution
    { _hitInsteadOfStand :: Double
    , _standInsteadOfHit :: Double
    , _noDoubleWhenShould :: Double
    , _noSplitWhenShould :: Double
    , _doubleWhenShouldnt :: Double
    , _splitWhenShouldnt :: Double
    }
    deriving (Eq, Show, Generic)

data CountingSystem
    = HiLo
    | KO
    | OmegaII
    deriving (Eq, Show, Generic)

data BettingSpread
    = FlatBetting
    | SimpleSpread Double Double
    | TrueCountSpread [(Int, Double)]
    deriving (Eq, Show, Generic)

data DeviationChart = DeviationChart
    { dcTrueCountDeviations :: Map Int [Deviation]
    }
    deriving (Show, Eq, Generic)

data Deviation = Deviation
    { devPlayerTotal :: Int
    , devDealerUpcard :: Int
    , devAction :: DeviationAction
    }
    deriving (Show, Eq, Generic)

data DeviationAction
    = DevStandInsteadOfHit
    | DevHitInsteadOfStand
    | DevDoubleInsteadOfHit
    deriving (Show, Eq, Generic)

data Superstition
    = NeverHitOn16
    | AlwaysStandSoft18
    | NeverSplitTens
    | AlwaysSplitAces
    | NeverDoubleDown
    | StandOn12VsDealer2Or3
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

data SessionStats = SessionStats
    { statsHandsPlayed :: Int
    , statsHandsWon :: Int
    , statsBlackjacks :: Int
    , statsBusts :: Int
    }
    deriving (Show, Eq, Generic)

data HandResult = Win | Loss | Push | Blackjack | Bust
    deriving (Show, Eq, Generic)

data PenetrationProfile = PenetrationProfile
    { ppTarget :: Double
    , ppVariance :: Double
    }
    deriving (Eq, Show, Generic)

data ErrorProfile = ErrorProfile
    { epErrorRate :: Double
    , epErrorDistribution :: ErrorDistribution
    }
    deriving (Eq, Show, Generic)

data ErrorDistribution = ErrorDistribution
    { edMisreadHand :: Double
    , edIncorrectPayout :: Double
    , edExposedCard :: Double
    , edMisdeal :: Double
    }
    deriving (Eq, Show, Generic)

data PaceProfile = PaceProfile
    { ppHandsPerHour :: Int
    , ppPaceVariance :: Double
    }
    deriving (Eq, Show, Generic)

data TellProfile = TellProfile
    { tpTellRate :: Double
    , tpTellAccuracy :: Double
    }
    deriving (Eq, Show, Generic)

emptySessionStats :: SessionStats
emptySessionStats = SessionStats 0 0 0 0

makeLenses ''MistakeProfile
makeLenses ''MistakeDistribution

instance ToJSON PlayerArchetypeKind
instance FromJSON PlayerArchetypeKind
instance ToJSON DealerArchetypeKind
instance FromJSON DealerArchetypeKind
instance ToJSON MistakeProfile
instance FromJSON MistakeProfile
instance ToJSON MistakeDistribution
instance FromJSON MistakeDistribution
instance ToJSON CountingSystem
instance FromJSON CountingSystem
instance ToJSON BettingSpread
instance FromJSON BettingSpread
instance ToJSON DeviationChart where
    toJSON (DeviationChart m) = toJSON m
instance FromJSON DeviationChart where
    parseJSON v = DeviationChart <$> parseJSON v
instance ToJSON Deviation
instance FromJSON Deviation
instance ToJSON DeviationAction
instance FromJSON DeviationAction
instance ToJSON Superstition
instance FromJSON Superstition
instance ToJSON SessionStats
instance FromJSON SessionStats
instance ToJSON HandResult
instance FromJSON HandResult
instance ToJSON PenetrationProfile
instance FromJSON PenetrationProfile
instance ToJSON ErrorProfile
instance FromJSON ErrorProfile
instance ToJSON ErrorDistribution
instance FromJSON ErrorDistribution
instance ToJSON PaceProfile
instance FromJSON PaceProfile
instance ToJSON TellProfile
instance FromJSON TellProfile

instance ToJSON (ArchetypeConfig 'BasicStrategy)
instance FromJSON (ArchetypeConfig 'BasicStrategy)
instance ToJSON (ArchetypeConfig 'Perfect)
instance FromJSON (ArchetypeConfig 'Perfect)
instance ToJSON (ArchetypeConfig 'Advantage)
instance FromJSON (ArchetypeConfig 'Advantage)
instance ToJSON (ArchetypeConfig 'Superstitious)
instance FromJSON (ArchetypeConfig 'Superstitious)
instance ToJSON (ArchetypeState 'BasicStrategy)
instance FromJSON (ArchetypeState 'BasicStrategy)
instance ToJSON (ArchetypeState 'Perfect)
instance FromJSON (ArchetypeState 'Perfect)
instance ToJSON (ArchetypeState 'Advantage)
instance FromJSON (ArchetypeState 'Advantage)
instance ToJSON (ArchetypeState 'Superstitious)
instance FromJSON (ArchetypeState 'Superstitious)
instance ToJSON (DealerArchetypeConfig 'ByTheBook)
instance FromJSON (DealerArchetypeConfig 'ByTheBook)
instance ToJSON (DealerArchetypeConfig 'Rookie)
instance FromJSON (DealerArchetypeConfig 'Rookie)
instance ToJSON (DealerArchetypeConfig 'Veteran)
instance FromJSON (DealerArchetypeConfig 'Veteran)
instance ToJSON (DealerArchetypeState 'ByTheBook)
instance FromJSON (DealerArchetypeState 'ByTheBook)
instance ToJSON (DealerArchetypeState 'Rookie)
instance FromJSON (DealerArchetypeState 'Rookie)
instance ToJSON (DealerArchetypeState 'Veteran)
instance FromJSON (DealerArchetypeState 'Veteran)

instance ToJSON SomePlayerArchetype where
    toJSON = \case
        SomePlayerBasicStrategy (BasicStrategyArchetype{..}) ->
            object ["type" .= ("BasicStrategy" :: String), "config" .= bsConfig, "state" .= bsState]
        SomePlayerPerfect (PerfectArchetype{..}) ->
            object ["type" .= ("Perfect" :: String), "config" .= pfConfig, "state" .= pfState]
        SomePlayerAdvantage (AdvantageArchetype{..}) ->
            object ["type" .= ("Advantage" :: String), "config" .= advConfig, "state" .= advState]
        SomePlayerSuperstitious (SuperstitiousArchetype{..}) ->
            object ["type" .= ("Superstitious" :: String), "config" .= ssConfig, "state" .= ssState]

instance FromJSON SomePlayerArchetype where
    parseJSON = withObject "SomePlayerArchetype" $ \o -> do
        typ <- o .: "type" :: Parser String
        case typ of
            "BasicStrategy" -> do
                config <- o .: "config"
                state <- o .: "state"
                pure $ SomePlayerBasicStrategy $ BasicStrategyArchetype config state
            "Perfect" -> do
                config <- o .: "config"
                state <- o .: "state"
                pure $ SomePlayerPerfect $ PerfectArchetype config state
            "Advantage" -> do
                config <- o .: "config"
                state <- o .: "state"
                pure $ SomePlayerAdvantage $ AdvantageArchetype config state
            "Superstitious" -> do
                config <- o .: "config"
                state <- o .: "state"
                pure $ SomePlayerSuperstitious $ SuperstitiousArchetype config state
            _ -> fail $ "Unknown player archetype: " ++ typ

instance ToJSON SomeDealerArchetype where
    toJSON = \case
        SomeDealerByTheBook (ByTheBookDealerArchetype{..}) ->
            object ["type" .= ("ByTheBook" :: String), "config" .= btbConfig, "state" .= btbState]
        SomeDealerRookie (RookieDealerArchetype{..}) ->
            object ["type" .= ("Rookie" :: String), "config" .= rkConfig, "state" .= rkState]
        SomeDealerVeteran (VeteranDealerArchetype{..}) ->
            object ["type" .= ("Veteran" :: String), "config" .= vtConfig, "state" .= vtState]

instance FromJSON SomeDealerArchetype where
    parseJSON = withObject "SomeDealerArchetype" $ \o -> do
        typ <- o .: "type" :: Parser String
        case typ of
            "ByTheBook" -> do
                config <- o .: "config"
                state <- o .: "state"
                pure $ SomeDealerByTheBook $ ByTheBookDealerArchetype config state
            "Rookie" -> do
                config <- o .: "config"
                state <- o .: "state"
                pure $ SomeDealerRookie $ RookieDealerArchetype config state
            "Veteran" -> do
                config <- o .: "config"
                state <- o .: "state"
                pure $ SomeDealerVeteran $ VeteranDealerArchetype config state
            _ -> fail $ "Unknown dealer archetype: " ++ typ
