{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.Table.Delta where

import Data.Aeson (FromJSON, ToJSON)
import Data.Void
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips
import Pitboss.Trace.Entity.Types.EntityId

data DTableAttrs
    = DTableSetName String String
    | DTableSetMinBet Chips Chips
    | DTableSetOffering (ClockedRef EOfferingId) (ClockedRef EOfferingId)
    deriving (Eq, Show, Generic)

instance ToJSON DTableAttrs
instance FromJSON DTableAttrs

data DTableModes = DTableModes Void
    deriving (Eq, Show, Generic)

instance ToJSON DTableModes
instance FromJSON DTableModes

data DTableRels
    = DTableSetDealer (Maybe (ClockedRef EDealerId)) (Maybe (ClockedRef EDealerId))
    deriving (Eq, Show, Generic)

instance ToJSON DTableRels
instance FromJSON DTableRels
