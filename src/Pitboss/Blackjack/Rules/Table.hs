module Pitboss.Blackjack.Rules.Table where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Types.Core

data TableRuleSet = TableRuleSet
    { minBet :: Chips
    , maxBet :: Chips
    , maxSpotsPerPlayer :: Int
    , midShoeEntry :: MidShoeEntryPolicy
    , multiSpotMinBetPolicy :: MultiSpotMinBetPolicy
    , burnPolicy :: BurnPolicy
    , standardPenetration :: Double
    }
    deriving (Eq, Show, Generic)

data MidShoeEntryPolicy = AllowMidShoe | NoMidShoe | EntryOnlyAfterShuffle
    deriving (Eq, Show, Generic)

data MultiSpotMinBetPolicy = SameMinBet | MultipliedMinBet Double
    deriving (Eq, Show, Generic)

data BurnPolicy = NoBurn | SingleCardBurn | BaccaratBurn Int | TailDiscard Rational
    deriving (Eq, Show, Generic)

instance ToJSON MidShoeEntryPolicy
instance FromJSON MidShoeEntryPolicy
instance ToJSON MultiSpotMinBetPolicy
instance FromJSON MultiSpotMinBetPolicy
instance ToJSON BurnPolicy
instance FromJSON BurnPolicy

instance ToJSON TableRuleSet
instance FromJSON TableRuleSet
