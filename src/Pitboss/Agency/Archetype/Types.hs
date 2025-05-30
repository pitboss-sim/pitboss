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

module Pitboss.Agency.Archetype.Types where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Map (Map)
import GHC.Generics (Generic)
import Pitboss.Blackjack.BasicStrategy.Chart.Types (StrategyChart)

-- | Player strategy types
data PlayerStrategy = UseChart | UseEV | UseCount
    deriving (Eq, Show, Generic)

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

data CountingConfig = CountingConfig
    { _countingSystem :: CountingSystem
    , _countingAccuracy :: Double
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

data CoverPlay = CoverPlay
    { _varyBetsRandomly :: Double
    }
    deriving (Eq, Show, Generic)

-- | Dealer types
data PenetrationProfile = PenetrationProfile
    { targetPenetration :: Double -- TODO: no floating point
    , penetrationVariance :: Double -- TODO: no floating point
    }
    deriving (Eq, Show, Generic)

data ErrorProfile = ErrorProfile
    { errorRate :: Double -- TODO: no floating point
    , errorDistribution :: ErrorDistribution
    }
    deriving (Eq, Show, Generic)

data ErrorDistribution = ErrorDistribution
    { misreadHand :: Double -- TODO: no floating point
    , incorrectPayout :: Double -- TODO: no floating point
    , exposedCard :: Double -- TODO: no floating point
    }
    deriving (Eq, Show, Generic)

data PaceProfile = PaceProfile
    { handsPerHour :: Int
    , paceVariance :: Double -- TODO: no floating point
    }
    deriving (Eq, Show, Generic)

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

-- | Data families for archetype-specific data
data family PlayerArchetype (k :: PlayerArchetypeKind)

data family DealerArchetype (k :: DealerArchetypeKind)

-- | Archetype-specific configuration/state
data family ArchetypeConfig (k :: PlayerArchetypeKind)

data family ArchetypeState (k :: PlayerArchetypeKind)

data instance PlayerArchetype 'Superstitious = SuperstitiousArchetype
    { ssConfig :: ArchetypeConfig 'Superstitious
    , ssState :: ArchetypeState 'Superstitious
    }
    deriving (Show, Eq, Generic)
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

data instance ArchetypeConfig 'Superstitious = SuperstitionConfig
    { scBeliefs :: [Superstition]
    }
    deriving (Show, Eq, Generic)

data instance ArchetypeState 'Superstitious = SuperstitionState
    { ssHandsPlayed :: Int
    , ssBeliefsForcedMoves :: Int
    , ssSessionStats :: SessionStats
    }
    deriving (Show, Eq, Generic)
data instance ArchetypeConfig 'BasicStrategy = BasicConfig
    { bcStrategyChart :: StrategyChart
    , bcMistakeProfile :: MistakeProfile
    }
    deriving (Show, Eq, Generic)

data instance ArchetypeConfig 'Perfect = PerfectConfig
    { pcUseEV :: Bool -- Use expected value calculations
    }
    deriving (Show, Eq, Generic)

data instance ArchetypeConfig 'Advantage = AdvantageConfig
    { acCountingSystem :: CountingSystem
    , acBettingSpread :: BettingSpread
    , acDeviationChart :: DeviationChart
    }
    deriving (Show, Eq, Generic)

-- | State instances (runtime state)
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

data family DealerArchetypeConfig (k :: DealerArchetypeKind)
data family DealerArchetypeState (k :: DealerArchetypeKind)

data instance DealerArchetypeConfig 'ByTheBook = ByTheBookConfig
    { btbPenetration :: PenetrationProfile
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

mkSuperstitiousPlayer :: [Superstition] -> SomePlayerArchetype
mkSuperstitiousPlayer beliefs =
    SomePlayerSuperstitious $
        SuperstitiousArchetype
            { ssConfig = SuperstitionConfig beliefs
            , ssState = SuperstitionState 0 0 emptySessionStats
            }
mkBasicPlayer :: StrategyChart -> MistakeProfile -> SomePlayerArchetype
mkBasicPlayer chart mistakes =
    SomePlayerBasicStrategy $
        BasicStrategyArchetype
            { bsConfig = BasicConfig chart mistakes
            , bsState = BasicState 0 emptySessionStats
            }

mkPerfectPlayer :: Bool -> SomePlayerArchetype
mkPerfectPlayer useEV =
    SomePlayerPerfect $
        PerfectArchetype
            { pfConfig = PerfectConfig useEV
            , pfState = PerfectState 0 emptySessionStats
            }

mkAdvantagePlayer :: CountingSystem -> BettingSpread -> DeviationChart -> SomePlayerArchetype
mkAdvantagePlayer counting betting deviations =
    SomePlayerAdvantage $
        AdvantageArchetype
            { advConfig = AdvantageConfig counting betting deviations
            , advState = AdvantageState 0 0.0 6.0 emptySessionStats
            }

data Superstition
    = NeverHitOn16
    | AlwaysStandSoft18
    | NeverSplitTens
    | AlwaysSplitAces
    | NeverDoubleDown
    | StandOn12VsDealer2Or3
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)
class ArchetypeOps (k :: PlayerArchetypeKind) where
    -- Get strategy for this archetype
    getStrategy :: PlayerArchetype k -> PlayerStrategy

    -- Update archetype state after a hand
    updateState :: PlayerArchetype k -> HandResult -> PlayerArchetype k

    -- Check if archetype can perform certain actions
    canCount :: PlayerArchetype k -> Bool
    canMakeMistakes :: PlayerArchetype k -> Bool

instance ArchetypeOps 'BasicStrategy where
    getStrategy _ = UseChart
    updateState arch _ =
        arch
            { bsState = (bsState arch){bsMistakesMade = bsMistakesMade (bsState arch) + 1}
            }
    canCount _ = False
    canMakeMistakes _ = True

instance ArchetypeOps 'Perfect where
    getStrategy _ = UseEV
    updateState arch _ =
        arch
            { pfState = (pfState arch){psHandsPlayed = psHandsPlayed (pfState arch) + 1}
            }
    canCount _ = False
    canMakeMistakes _ = False

instance ArchetypeOps 'Advantage where
    getStrategy _ = UseCount
    updateState arch _ = arch
    canCount _ = True
    canMakeMistakes _ = False

instance ArchetypeOps 'Superstitious where
    getStrategy _ = UseChart
    updateState arch _ =
        arch
            { ssState = (ssState arch){ssHandsPlayed = ssHandsPlayed (ssState arch) + 1}
            }
    canCount _ = False
    canMakeMistakes _ = True

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

data DeviationAction = DevStandInsteadOfHit | DevHitInsteadOfStand | DevDoubleInsteadOfHit
    deriving (Show, Eq, Generic)

data SessionStats = SessionStats
    { statsHandsPlayed :: Int
    , statsHandsWon :: Int
    , statsBlackjacks :: Int
    , statsBusts :: Int
    }
    deriving (Show, Eq, Generic)

data HandResult = Win | Loss | Push | Blackjack | Bust
    deriving (Show, Eq, Generic)

emptySessionStats :: SessionStats
emptySessionStats = SessionStats 0 0 0 0

instance ToJSON PlayerArchetypeKind
instance FromJSON PlayerArchetypeKind

instance ToJSON DealerArchetypeKind
instance FromJSON DealerArchetypeKind

instance ToJSON Superstition
instance FromJSON Superstition

instance ToJSON DeviationAction
instance FromJSON DeviationAction

instance ToJSON Deviation
instance FromJSON Deviation

instance ToJSON DeviationChart where
    toJSON (DeviationChart m) = toJSON m

instance FromJSON DeviationChart where
    parseJSON v = DeviationChart <$> parseJSON v

instance ToJSON SessionStats
instance FromJSON SessionStats

instance ToJSON HandResult
instance FromJSON HandResult

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

mkByTheBookDealer :: PenetrationProfile -> SomeDealerArchetype
mkByTheBookDealer penetration =
    SomeDealerByTheBook $
        ByTheBookDealerArchetype
            { btbConfig = ByTheBookConfig penetration
            , btbState = ByTheBookState 0
            }

mkRookieDealer :: PenetrationProfile -> ErrorProfile -> PaceProfile -> SomeDealerArchetype
mkRookieDealer penetration errors pace =
    SomeDealerRookie $
        RookieDealerArchetype
            { rkConfig = RookieConfig penetration errors pace
            , rkState = RookieState 0 0
            }

mkVeteranDealer :: PenetrationProfile -> PaceProfile -> SomeDealerArchetype
mkVeteranDealer penetration pace =
    SomeDealerVeteran $
        VeteranDealerArchetype
            { vtConfig = VeteranConfig penetration pace
            , vtState = VeteranState 0 0
            }

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

-- | JSON instances for moved types
instance ToJSON PlayerStrategy

instance FromJSON PlayerStrategy

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
instance ToJSON CoverPlay
instance FromJSON CoverPlay

instance ToJSON PenetrationProfile
instance FromJSON PenetrationProfile
instance ToJSON ErrorProfile
instance FromJSON ErrorProfile
instance ToJSON ErrorDistribution
instance FromJSON ErrorDistribution
instance ToJSON PaceProfile
instance FromJSON PaceProfile

-- | Lens generation
makeLenses ''MistakeProfile
makeLenses ''MistakeDistribution
makeLenses ''CountingConfig
makeLenses ''CoverPlay
