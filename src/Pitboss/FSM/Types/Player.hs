{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Types.Player where

import Data.Aeson.Types
import Pitboss.FSM.Types.Core

data SomePlayerFSM = forall p. SomePlayerFSM (PlayerFSM p)

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
