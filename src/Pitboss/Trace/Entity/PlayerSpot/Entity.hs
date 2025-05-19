{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.PlayerSpot.Entity where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips
import Pitboss.FSM.PlayerSpot
import Pitboss.Trace.Entity.Types.EntityId
import Pitboss.Trace.Entity.Types.FiniteMap
import Pitboss.Trace.Entity.Types.FiniteMap.BoundedEnum
import Pitboss.Trace.Entity.Types.FiniteMap.Occupancy

mkEPlayerSpotAttrs :: PlayerSpotIx -> Chips -> EPlayerSpotAttrs
mkEPlayerSpotAttrs = EPlayerSpotAttrs

mkEPlayerSpotModes :: SomePlayerSpotFSM -> EPlayerSpotModes
mkEPlayerSpotModes = EPlayerSpotModes

mkEPlayerSpotRels :: ClockedRef EPlayerId -> ClockedRef EDealerRoundId -> FiniteMap PlayerSpotHandIx (Occupancy (ClockedRef EPlayerHandId)) -> EPlayerSpotRels
mkEPlayerSpotRels = EPlayerSpotRels

data PlayerSpotIx = EPlayerSpot1 | EPlayerSpot2 | EPlayerSpot3 | EPlayerSpot4
    deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSONKey PlayerSpotIx

instance FromJSONKey PlayerSpotIx

instance ToJSON PlayerSpotIx

instance FromJSON PlayerSpotIx

instance BoundedEnum PlayerSpotIx

data EPlayerSpotAttrs = EPlayerSpotAttrs
    { _psAttrsSpotIndex :: PlayerSpotIx
    , _psAttrsWager :: Chips
    }
    deriving (Eq, Show, Generic)

data EPlayerSpotModes = EPlayerSpotModes
    { _psModesPlayerSpot :: SomePlayerSpotFSM
    }
    deriving (Eq, Show, Generic)

data PlayerSpotHandIx = EPlayerSpotHand1 | EPlayerSpotHand2 | EPlayerSpotHand3 | EPlayerSpotHand4
    deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSONKey PlayerSpotHandIx

instance FromJSONKey PlayerSpotHandIx

instance ToJSON PlayerSpotHandIx

instance FromJSON PlayerSpotHandIx

instance BoundedEnum PlayerSpotHandIx

data EPlayerSpotRels = EPlayerSpotRels
    { _psEntityRelsPlayerId :: ClockedRef EPlayerId
    , _psEntityRelsRoundId :: ClockedRef EDealerRoundId
    , _psRelsHandOccupancy :: FiniteMap PlayerSpotHandIx (Occupancy (ClockedRef EPlayerHandId))
    }
    deriving (Eq, Show, Generic)

instance ToJSON EPlayerSpotAttrs

instance FromJSON EPlayerSpotAttrs

instance ToJSON EPlayerSpotModes

instance FromJSON EPlayerSpotModes

instance ToJSON EPlayerSpotRels

instance FromJSON EPlayerSpotRels
