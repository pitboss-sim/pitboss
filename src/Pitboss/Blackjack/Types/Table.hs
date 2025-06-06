{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Blackjack.Types.Table where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Blackjack.Types.Core

data TableRuleSet = TableRuleSet
    { minBet :: Chips
    , maxBet :: Chips
    , maxBoutsPerPlayer :: Int
    , midShoeEntry :: MidShoeEntryPolicy
    , multiSpotMinBetPolicy :: MultiSpotMinBetPolicy
    , burnPolicy :: BurnPolicy
    , standardPenetration :: Double
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data MidShoeEntryPolicy = AllowMidShoe | NoMidShoe | EntryOnlyAfterShuffle
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data MultiSpotMinBetPolicy = SameMinBet | MultipliedMinBet Double
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data BurnPolicy = NoBurn | SingleCardBurn | BaccaratBurn Int | TailDiscard Rational
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
