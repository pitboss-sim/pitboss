{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Archetype.Player where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import System.Random (StdGen, randomR)

data PlayerArchetype
    = PerfectPlayer
    | BasicStrategyPlayer
        { _mistakeProfile :: MistakeProfile
        }
    | AdvantagePlayer
        { _counting :: CountingConfig
        , _betting :: BettingConfig
        , _deviations :: DeviationConfig
        }
    deriving (Eq, Show, Generic)

data MistakeProfile = MistakeProfile
    { _mistakeRate :: Double
    , _mistakeTypes :: MistakeDistribution
    }
    deriving (Eq, Show, Generic)

data MistakeDistribution = MistakeDistribution
    { _hitInsteadOfStand :: Double
    , _standInsteadOfHit :: Double
    , _noDoubleWhenShould :: Double
    , _doubleWhenShouldnt :: Double
    , _noSplitWhenShould :: Double
    , _splitWhenShouldnt :: Double
    }
    deriving (Eq, Show, Generic)

data CountingConfig = CountingConfig
    { _system :: CountingSystem
    , _accuracy :: CountingAccuracy
    }
    deriving (Eq, Show, Generic)

data CountingSystem
    = HiLo
    | HiOpt1
    | HiOpt2
    | OmegaII
    | ZenCount
    | KnockOut
    deriving (Eq, Show, Generic, Enum, Bounded)

data CountingAccuracy = CountingAccuracy
    { _deckEstimationError :: Double
    , _countingErrorRate :: Double
    }
    deriving (Eq, Show, Generic)

data BettingConfig = BettingConfig
    { _spread :: BetSpread
    , _kellyFraction :: Double
    , _coverBetting :: CoverBetting
    }
    deriving (Eq, Show, Generic)

data BetSpread
    = ConstantBet
    | SimpleSpread Int Int
    | CountBasedSpread [(Int, Int)]
    deriving (Eq, Show, Generic)

data CoverBetting = CoverBetting
    { _varyBetsRandomly :: Double
    , _capMaxBet :: Maybe Int
    }
    deriving (Eq, Show, Generic)

data DeviationConfig = DeviationConfig
    { _illustrious18 :: Bool
    , _fab4 :: Bool
    , _additionalIndices :: [(Int, DeviationPlay)]
    }
    deriving (Eq, Show, Generic)

data DeviationPlay = DeviationPlay
    { _deviationTrueCount :: Int
    , _deviationAction :: DeviationAction
    }
    deriving (Eq, Show, Generic)

data DeviationAction
    = StandOn16vT
    | HitOn13v2
    | DoubleOn9v2
    | SplitTTv5
    deriving (Eq, Show, Generic, Enum, Bounded)

makeLenses ''MistakeProfile
makeLenses ''MistakeDistribution
makeLenses ''CountingConfig
makeLenses ''CountingAccuracy
makeLenses ''BettingConfig
makeLenses ''CoverBetting
makeLenses ''DeviationConfig
makeLenses ''DeviationPlay

instance ToJSON PlayerArchetype
instance FromJSON PlayerArchetype
instance ToJSON MistakeProfile
instance FromJSON MistakeProfile
instance ToJSON MistakeDistribution
instance FromJSON MistakeDistribution
instance ToJSON CountingConfig
instance FromJSON CountingConfig
instance ToJSON CountingSystem
instance FromJSON CountingSystem
instance ToJSON CountingAccuracy
instance FromJSON CountingAccuracy
instance ToJSON BettingConfig
instance FromJSON BettingConfig
instance ToJSON BetSpread
instance FromJSON BetSpread
instance ToJSON CoverBetting
instance FromJSON CoverBetting
instance ToJSON DeviationConfig
instance FromJSON DeviationConfig
instance ToJSON DeviationPlay
instance FromJSON DeviationPlay
instance ToJSON DeviationAction
instance FromJSON DeviationAction

data SomePlayerArchetype = SomePlayerArchetype PlayerArchetype
    deriving (Eq, Show, Generic)

instance ToJSON SomePlayerArchetype
instance FromJSON SomePlayerArchetype

noMistakes :: MistakeProfile
noMistakes =
    MistakeProfile
        { _mistakeRate = 0.0
        , _mistakeTypes =
            MistakeDistribution
                { _hitInsteadOfStand = 0.0
                , _standInsteadOfHit = 0.0
                , _noDoubleWhenShould = 0.0
                , _doubleWhenShouldnt = 0.0
                , _noSplitWhenShould = 0.0
                , _splitWhenShouldnt = 0.0
                }
        }

uniformMistakeDistribution :: MistakeDistribution
uniformMistakeDistribution =
    MistakeDistribution
        { _hitInsteadOfStand = 1.0 / 6.0
        , _standInsteadOfHit = 1.0 / 6.0
        , _noDoubleWhenShould = 1.0 / 6.0
        , _doubleWhenShouldnt = 1.0 / 6.0
        , _noSplitWhenShould = 1.0 / 6.0
        , _splitWhenShouldnt = 1.0 / 6.0
        }

rollForMistake :: MistakeProfile -> StdGen -> (Bool, StdGen)
rollForMistake profile gen =
    let (roll, gen') = randomR (0.0, 1.0) gen
     in (roll < profile ^. mistakeRate, gen')
