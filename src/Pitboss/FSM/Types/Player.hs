{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Types.Player where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.FSM.Transitionable
import Pitboss.FSM.Types.Core

data PlayerPhase
    = PIdle
    | PChoosingTable
    | PPlacingBet
    | PPlayingHand
    | PObserving
    | PDone
    deriving (Eq, Show, Generic)

instance ToJSON PlayerPhase
instance FromJSON PlayerPhase

data SomePlayerFSM = forall p. SomePlayerFSM (PlayerFSM p)

instance Transitionable SomePlayerFSM where
    transitionType (SomePlayerFSM fsm) = transitionType fsm

instance Show SomePlayerFSM where
    show (SomePlayerFSM fsm) = show fsm

instance Eq SomePlayerFSM where
    SomePlayerFSM f1 == SomePlayerFSM f2 = case (f1, f2) of
        (PIdleFSM, PIdleFSM) -> True
        (PChoosingTableFSM, PChoosingTableFSM) -> True
        (PPlacingBetFSM, PPlacingBetFSM) -> True
        (PPlayingHandFSM, PPlayingHandFSM) -> True
        (PObservingFSM, PObservingFSM) -> True
        (PDoneFSM, PDoneFSM) -> True
        _ -> False

instance ToJSON SomePlayerFSM where
    toJSON (SomePlayerFSM fsm) = case fsm of
        PIdleFSM -> object ["tag" .= String "Idle"]
        PChoosingTableFSM -> object ["tag" .= String "ChoosingTable"]
        PPlacingBetFSM -> object ["tag" .= String "PlacingBet"]
        PPlayingHandFSM -> object ["tag" .= String "PlayingHand"]
        PObservingFSM -> object ["tag" .= String "Observing"]
        PDoneFSM -> object ["tag" .= String "Done"]

instance FromJSON SomePlayerFSM where
    parseJSON = withObject "SomePlayerFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "Idle" -> pure $ SomePlayerFSM PIdleFSM
            "ChoosingTable" -> pure $ SomePlayerFSM PChoosingTableFSM
            "PlacingBet" -> pure $ SomePlayerFSM PPlacingBetFSM
            "PlayingHand" -> pure $ SomePlayerFSM PPlayingHandFSM
            "Observing" -> pure $ SomePlayerFSM PObservingFSM
            "Done" -> pure $ SomePlayerFSM PDoneFSM
            _ -> fail $ "Unknown tag for SomePlayerFSM: " ++ tag

data PlayerFSM (p :: PlayerPhase) where
    PIdleFSM :: PlayerFSM 'PIdle
    PChoosingTableFSM :: PlayerFSM 'PChoosingTable
    PPlacingBetFSM :: PlayerFSM 'PPlacingBet
    PPlayingHandFSM :: PlayerFSM 'PPlayingHand
    PObservingFSM :: PlayerFSM 'PObserving
    PDoneFSM :: PlayerFSM 'PDone

deriving instance Show (PlayerFSM p)
deriving instance Eq (PlayerFSM p)

instance Transitionable (PlayerFSM p) where
    transitionType = \case
        PIdleFSM -> AwaitInput
        PChoosingTableFSM -> AwaitInput
        PPlacingBetFSM -> AwaitInput
        PPlayingHandFSM -> AwaitInput
        PObservingFSM -> AutoAdvance
        PDoneFSM -> TerminalPhase
