{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.Table.Entity where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips
import Pitboss.FSM.Table
import Pitboss.Trace.Entity.Types.EntityId

mkETableAttrs :: String -> Maybe (ClockedRef EDealerRoundId) -> ClockedRef EOfferingId -> Chips -> ETableAttrs
mkETableAttrs = ETableAttrs

mkETableModes :: SomeTableFSM -> ETableModes
mkETableModes = ETableModes

mkETableRels :: Maybe (ClockedRef EDealerId) -> ETableRels
mkETableRels = ETableRels

data ETableAttrs = ETableAttrs
    { _tAttrsName :: String
    , _tAttrsCurrentRound :: Maybe (ClockedRef EDealerRoundId)
    , _tAttrsOfferingUsed :: ClockedRef EOfferingId
    , _tAttrsMinBet :: Chips
    }
    deriving (Eq, Show, Generic)

data ETableModes = ETableModes
    { _tModesFSM :: SomeTableFSM
    }
    deriving (Eq, Show, Generic)

data ETableRels = ETableRels
    { _tRelsManagedByDealer :: Maybe (ClockedRef EDealerId)
    }
    deriving (Eq, Show, Generic)

instance ToJSON ETableModes

instance FromJSON ETableModes

instance ToJSON ETableAttrs

instance FromJSON ETableAttrs

instance ToJSON ETableRels

instance FromJSON ETableRels
