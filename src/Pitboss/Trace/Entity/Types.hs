{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Types where

import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import Data.Hashable (Hashable (..))
import Data.Word (Word64)
import GHC.Generics (Generic)

newtype Tick = Tick Word64
    deriving (Eq, Ord, Show, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data EntityKind
    = Dealer
    | DealerHand
    | DealerRound
    | Offering
    | Player
    | PlayerHand
    | PlayerSpot
    | Table
    | TableShoe
    deriving (Eq, Show, Generic)

data EntityStatePart = Meta | Attrs | Modes | Rels
    deriving (Eq, Show, Generic)

data EntityStateSelector
    = Whole
    | Part EntityStatePart
    deriving (Eq, Show, Generic)
