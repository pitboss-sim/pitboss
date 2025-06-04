{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.Simulation.Agents.Types where

import Control.Lens hiding ((.=))
import Data.Aeson.Types
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Pitboss.Blackjack
import Pitboss.Causality.Types.Core

data BoutContext = BoutContext
    { _contextBoutPlayerHand :: SomeHand
    , _contextDealerUpcard :: Card
    , _contextOffering :: Offering
    , _contextCanDouble :: Bool
    , _contextCanSplit :: Bool
    , _contextCanSurrender :: Bool
    , _contextHandIx :: HandIx
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeLenses ''BoutContext

data PlayerArchetypeKind
    = BasicStrategy
    | Perfect
    | Advantage
    | Superstitious
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DealerArchetypeKind
    = ByTheBook
    | Rookie
    | Veteran
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

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
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance PlayerArchetype 'Perfect = PerfectArchetype
    { pfConfig :: ArchetypeConfig 'Perfect
    , pfState :: ArchetypeState 'Perfect
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance PlayerArchetype 'Advantage = AdvantageArchetype
    { advConfig :: ArchetypeConfig 'Advantage
    , advState :: ArchetypeState 'Advantage
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance PlayerArchetype 'Superstitious = SuperstitiousArchetype
    { ssConfig :: ArchetypeConfig 'Superstitious
    , ssState :: ArchetypeState 'Superstitious
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance ArchetypeConfig 'BasicStrategy = BasicConfig
    { bcStrategyChart :: StrategyChart
    , bcMistakeProfile :: MistakeProfile
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance ArchetypeConfig 'Perfect = PerfectConfig
    { pcUseEV :: Bool
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance ArchetypeConfig 'Advantage = AdvantageConfig
    { acCountingSystem :: CountingSystem
    , acBettingSpread :: BettingSpread
    , acDeviationChart :: DeviationChart
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance ArchetypeConfig 'Superstitious = SuperstitionConfig
    { scBeliefs :: [Superstition]
    , scFallbackChart :: StrategyChart
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance ArchetypeState 'BasicStrategy = BasicState
    { bsMistakesMade :: Int
    , bsSessionStats :: SessionStats
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance ArchetypeState 'Perfect = PerfectState
    { psHandsPlayed :: Int
    , psSessionStats :: SessionStats
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance ArchetypeState 'Advantage = AdvantageState
    { asRunningCount :: Int
    , asTrueCount :: Double
    , asDecksRemaining :: Double
    , asSessionStats :: SessionStats
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance ArchetypeState 'Superstitious = SuperstitionState
    { ssHandsPlayed :: Int
    , ssBeliefsForcedMoves :: Int
    , ssSessionStats :: SessionStats
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance DealerArchetype 'ByTheBook = ByTheBookDealerArchetype
    { btbConfig :: DealerArchetypeConfig 'ByTheBook
    , btbState :: DealerArchetypeState 'ByTheBook
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance DealerArchetype 'Rookie = RookieDealerArchetype
    { rkConfig :: DealerArchetypeConfig 'Rookie
    , rkState :: DealerArchetypeState 'Rookie
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance DealerArchetype 'Veteran = VeteranDealerArchetype
    { vtConfig :: DealerArchetypeConfig 'Veteran
    , vtState :: DealerArchetypeState 'Veteran
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance DealerArchetypeConfig 'ByTheBook = ByTheBookConfig
    { btbPenetration :: PenetrationProfile
    , btbPace :: PaceProfile
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance DealerArchetypeConfig 'Rookie = RookieConfig
    { rkPenetration :: PenetrationProfile
    , rkErrors :: ErrorProfile
    , rkPace :: PaceProfile
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance DealerArchetypeConfig 'Veteran = VeteranConfig
    { vtPenetration :: PenetrationProfile
    , vtPace :: PaceProfile
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance DealerArchetypeState 'ByTheBook = ByTheBookState
    { btbRoundsDealt :: Int
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance DealerArchetypeState 'Rookie = RookieState
    { rkRoundsDealt :: Int
    , rkErrorsMade :: Int
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data instance DealerArchetypeState 'Veteran = VeteranState
    { vtRoundsDealt :: Int
    , vtTellsGiven :: Int
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

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
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data MistakeDistribution = MistakeDistribution
    { _hitInsteadOfStand :: Double
    , _standInsteadOfHit :: Double
    , _noDoubleWhenShould :: Double
    , _noSplitWhenShould :: Double
    , _doubleWhenShouldnt :: Double
    , _splitWhenShouldnt :: Double
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data CountingSystem
    = HiLo
    | KO
    | OmegaII
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data BettingSpread
    = FlatBetting
    | SimpleSpread Double Double
    | TrueCountSpread [(Int, Double)]
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DeviationChart = DeviationChart
    { dcTrueCountDeviations :: Map Int [Deviation]
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Deviation = Deviation
    { devPlayerTotal :: Int
    , devDealerUpcard :: Int
    , devAction :: DeviationAction
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DeviationAction
    = DevStandInsteadOfHit
    | DevHitInsteadOfStand
    | DevDoubleInsteadOfHit
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Superstition
    = NeverHitOn16
    | AlwaysStandSoft18
    | NeverSplitTens
    | AlwaysSplitAces
    | NeverDoubleDown
    | StandOn12VsDealer2Or3
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data SessionStats = SessionStats
    { statsHandsPlayed :: Int
    , statsHandsWon :: Int
    , statsBlackjacks :: Int
    , statsBusts :: Int
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data HandResult = Win | Loss | Push | Blackjack | Bust
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data PenetrationProfile = PenetrationProfile
    { ppTarget :: Double
    , ppVariance :: Double
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ErrorProfile = ErrorProfile
    { epErrorRate :: Double
    , epErrorDistribution :: ErrorDistribution
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ErrorDistribution = ErrorDistribution
    { edMisreadHand :: Double
    , edIncorrectPayout :: Double
    , edExposedCard :: Double
    , edMisdeal :: Double
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data PaceProfile = PaceProfile
    { ppHandsPerHour :: Int
    , ppPaceVariance :: Double
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

emptySessionStats :: SessionStats
emptySessionStats = SessionStats 0 0 0 0

makeLenses ''MistakeProfile
makeLenses ''MistakeDistribution

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
