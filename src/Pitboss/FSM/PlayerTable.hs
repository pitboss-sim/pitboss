{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.PlayerTable where

import Data.Aeson.Types
import GHC.Generics (Generic)

data PlayerTablePhase
    = PTIdle
    | PTChoosingTable
    | PTPlacingBet
    | PTPlayingHand
    | PTObserving
    | PTDone
    deriving (Eq, Show, Generic)

instance ToJSON PlayerTablePhase
instance FromJSON PlayerTablePhase

data SomePlayerTableFSM = forall p. SomePlayerTableFSM (PlayerTableFSM p)

data PlayerTableFSM (p :: PlayerTablePhase) where
    PTIdleFSM :: PlayerTableFSM 'PTIdle
    PTChoosingTableFSM :: PlayerTableFSM 'PTChoosingTable
    PTPlacingBetFSM :: PlayerTableFSM 'PTPlacingBet
    PTPlayingHandFSM :: PlayerTableFSM 'PTPlayingHand
    PTObservingFSM :: PlayerTableFSM 'PTObserving
    PTDoneFSM :: PlayerTableFSM 'PTDone

deriving instance Show (PlayerTableFSM p)
deriving instance Eq (PlayerTableFSM p)

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

type family ValidPlayerTableTransition (from :: PlayerTablePhase) (to :: PlayerTablePhase) :: Bool where
    ValidPlayerTableTransition 'PTIdle 'PTChoosingTable = 'True
    ValidPlayerTableTransition 'PTChoosingTable 'PTPlacingBet = 'True
    ValidPlayerTableTransition 'PTPlacingBet 'PTPlayingHand = 'True
    ValidPlayerTableTransition 'PTPlayingHand 'PTObserving = 'True
    ValidPlayerTableTransition 'PTObserving 'PTDone = 'True
    ValidPlayerTableTransition _ _ = 'False

beginChoosingTable ::
    (ValidPlayerTableTransition 'PTIdle 'PTChoosingTable ~ 'True) =>
    PlayerTableFSM 'PTIdle ->
    PlayerTableFSM 'PTChoosingTable
beginChoosingTable PTIdleFSM = PTChoosingTableFSM

confirmTableChoice ::
    (ValidPlayerTableTransition 'PTChoosingTable 'PTPlacingBet ~ 'True) =>
    PlayerTableFSM 'PTChoosingTable ->
    PlayerTableFSM 'PTPlacingBet
confirmTableChoice PTChoosingTableFSM = PTPlacingBetFSM

placeBet ::
    (ValidPlayerTableTransition 'PTPlacingBet 'PTPlayingHand ~ 'True) =>
    PlayerTableFSM 'PTPlacingBet ->
    PlayerTableFSM 'PTPlayingHand
placeBet PTPlacingBetFSM = PTPlayingHandFSM

startObservation ::
    (ValidPlayerTableTransition 'PTPlayingHand 'PTObserving ~ 'True) =>
    PlayerTableFSM 'PTPlayingHand ->
    PlayerTableFSM 'PTObserving
startObservation PTPlayingHandFSM = PTObservingFSM

completeSession ::
    (ValidPlayerTableTransition 'PTObserving 'PTDone ~ 'True) =>
    PlayerTableFSM 'PTObserving ->
    PlayerTableFSM 'PTDone
completeSession PTObservingFSM = PTDoneFSM
