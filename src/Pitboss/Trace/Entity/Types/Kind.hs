{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Types.Kind where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data SomeKind where
    SomeKind :: EntityKind -> SomeKind
    deriving (Generic)

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
    | TableShoeCursor
    deriving (Eq, Show, Generic)

instance ToJSON SomeKind
instance FromJSON SomeKind

instance ToJSON EntityKind
instance FromJSON EntityKind
