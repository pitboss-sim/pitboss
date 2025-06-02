{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.PlayerTable (
    module Pitboss.FSM.PlayerTable.FSM,
    module Pitboss.FSM.PlayerTable.Phase,
    module Pitboss.FSM.PlayerTable.Transition,
    SomePlayerTableFSM (..),
)
where

import Data.Aeson.Types
import Pitboss.FSM.PlayerTable.FSM
import Pitboss.FSM.PlayerTable.Phase
import Pitboss.FSM.PlayerTable.Transition

data SomePlayerTableFSM = forall p. SomePlayerTableFSM (PlayerTableFSM p)

instance Show SomePlayerTableFSM where
    show (SomePlayerTableFSM fsm) = show fsm

instance Eq SomePlayerTableFSM where
    SomePlayerTableFSM f1 == SomePlayerTableFSM f2 = case (f1, f2) of
        (PTIdleFSM, PTIdleFSM) -> True
        (PTChoosingTableFSM, PTChoosingTableFSM) -> True
        (PTPlacingBetFSM, PTPlacingBetFSM) -> True
        (PTPlayingHandFSM, PTPlayingHandFSM) -> True
        (PTObservingFSM, PTObservingFSM) -> True
        (PTDoneFSM, PTDoneFSM) -> True
        _ -> False

instance ToJSON SomePlayerTableFSM where
    toJSON (SomePlayerTableFSM fsm) = case fsm of
        PTIdleFSM -> object ["tag" .= String "Idle"]
        PTChoosingTableFSM -> object ["tag" .= String "ChoosingTable"]
        PTPlacingBetFSM -> object ["tag" .= String "PlacingBet"]
        PTPlayingHandFSM -> object ["tag" .= String "PlayingHand"]
        PTObservingFSM -> object ["tag" .= String "Observing"]
        PTDoneFSM -> object ["tag" .= String "Done"]

instance FromJSON SomePlayerTableFSM where
    parseJSON = withObject "SomePlayerTableFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "Idle" -> pure $ SomePlayerTableFSM PTIdleFSM
            "ChoosingTable" -> pure $ SomePlayerTableFSM PTChoosingTableFSM
            "PlacingBet" -> pure $ SomePlayerTableFSM PTPlacingBetFSM
            "PlayingHand" -> pure $ SomePlayerTableFSM PTPlayingHandFSM
            "Observing" -> pure $ SomePlayerTableFSM PTObservingFSM
            "Done" -> pure $ SomePlayerTableFSM PTDoneFSM
            _ -> fail $ "Unknown tag for SomePlayerTableFSM: " ++ tag
