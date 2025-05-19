{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerHand.Delta.Modes where

import Data.Aeson
import GHC.Generics
import Pitboss.FSM.PlayerHand
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerHand.Types

data PlayerHandEntityModesDelta
    = ReplaceFSM SomePlayerHandFSM SomePlayerHandFSM
    deriving (Eq, Show, Generic)

instance ToJSON PlayerHandEntityModesDelta
instance FromJSON PlayerHandEntityModesDelta

instance Incremental PlayerHandEntityModesDelta where
    type Target PlayerHandEntityModesDelta = PlayerHandEntityModes

    applyDelta (ReplaceFSM _ new) state = state{_playerHandEntityFsm = new}

    previewDelta delta state = Just $ applyDelta delta state

    describeDelta (ReplaceFSM _ new) _ = "Replaced PlayerHand FSM with: " ++ show new

instance Reversible PlayerHandEntityModesDelta where
    invert (ReplaceFSM old new) = Right (ReplaceFSM new old)
