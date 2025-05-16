{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.Offering where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Offering qualified as O
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.Identifier

mkOffering :: Meta OfferingId -> OfferingState -> OfferingRelations -> Offering
mkOffering = Offering

mkOfferingState :: O.Offering -> OfferingState
mkOfferingState = OfferingState

mkOfferingRelations :: OfferingRelations
mkOfferingRelations = OfferingRelations []

data Offering = Offering
  { _meta :: Meta OfferingId,
    _state :: OfferingState,
    _rels :: OfferingRelations
  }
  deriving (Eq, Show, Generic)

data OfferingState = OfferingState
  { _offering :: O.Offering
  }
  deriving (Eq, Show, Generic)

data OfferingRelations = OfferingRelations
  { _associatedTables :: [TableId]
  }
  deriving (Eq, Show, Generic)

instance ToJSON Offering

instance FromJSON Offering

instance ToJSON OfferingState

instance FromJSON OfferingState

instance ToJSON OfferingRelations

instance FromJSON OfferingRelations
