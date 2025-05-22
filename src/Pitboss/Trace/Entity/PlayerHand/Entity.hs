{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.PlayerHand.Entity where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card
import Pitboss.Blackjack.Chips
import Pitboss.FSM.PlayerHand
import Pitboss.Trace.Entity.Types.EntityId

mkEPlayerHandAttrs :: [Card] -> Chips -> Int -> Int -> EPlayerHandAttrs
mkEPlayerHandAttrs = EPlayerHandAttrs

mkEPlayerHandModes :: SomePlayerHandFSM -> EPlayerHandModes
mkEPlayerHandModes = EPlayerHandModes

mkEPlayerHandRels :: ClockedRef EPlayerSpotId -> ClockedRef EDealerRoundId -> ClockedRef EPlayerId -> EPlayerHandRels
mkEPlayerHandRels = EPlayerHandRels

data EPlayerHandAttrs = EPlayerHandAttrs
    { _phAttrsHandCards :: [Card]
    , _phAttrsOriginalBet :: Chips
    , _phAttrsSplitDepth :: Int
    , _phAttrsHandIx :: Int
    }
    deriving (Eq, Show, Generic)

data EPlayerHandModes = EPlayerHandModes
    { _phFsm :: SomePlayerHandFSM
    }
    deriving (Eq, Show, Generic)

data EPlayerHandRels = EPlayerHandRels
    { _phRelsBelongsToPlayerSpot :: ClockedRef EPlayerSpotId
    , _phRelsBelongsToDealerRound :: ClockedRef EDealerRoundId
    , _phRelsOwnedByPlayer :: ClockedRef EPlayerId
    }
    deriving (Eq, Show, Generic)

instance ToJSON EPlayerHandAttrs

instance FromJSON EPlayerHandAttrs

instance ToJSON EPlayerHandModes

instance FromJSON EPlayerHandModes

instance ToJSON EPlayerHandRels

instance FromJSON EPlayerHandRels
