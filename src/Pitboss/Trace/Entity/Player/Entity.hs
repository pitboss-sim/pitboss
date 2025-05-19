{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.Player.Entity where

import Data.Aeson (FromJSON, ToJSON)
import Data.Void (Void)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips
import Pitboss.FSM.PlayerHand
import Pitboss.FSM.PlayerSpot
import Pitboss.FSM.PlayerTable

mkEPlayerAttrs :: String -> Chips -> EPlayerAttrs
mkEPlayerAttrs = EPlayerAttrs'

mkEPlayerModes :: Void -> EPlayerModes
mkEPlayerModes = EPlayerModes'

mkEPlayerRels :: SomePlayerTableFSM -> SomePlayerSpotFSM -> SomePlayerHandFSM -> EPlayerRels
mkEPlayerRels = EPlayerRels'

data EPlayerAttrs = EPlayerAttrs'
    { _pAttrsName :: String
    , _pAttrsBankroll :: Chips
    }
    deriving (Eq, Show, Generic)

data EPlayerModes = EPlayerModes' Void
    deriving (Eq, Show, Generic)

data EPlayerRels = EPlayerRels'
    { _pRelsPlayerTable :: SomePlayerTableFSM
    , _pRelsPlayerSpot :: SomePlayerSpotFSM
    , _pRelsPlayerHand :: SomePlayerHandFSM
    }
    deriving (Eq, Show, Generic)

instance ToJSON EPlayerAttrs

instance FromJSON EPlayerAttrs

instance ToJSON EPlayerModes

instance FromJSON EPlayerModes

instance ToJSON EPlayerRels

instance FromJSON EPlayerRels
