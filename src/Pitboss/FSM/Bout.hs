{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.Bout (
    module Pitboss.FSM.Bout.FSM,
    module Pitboss.FSM.Bout.Phase,
    module Pitboss.FSM.Bout.Transition,
    SomeBoutFSM (..),
) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:), (.=), Value (..))
import Pitboss.FSM.Bout.FSM
import Pitboss.FSM.Bout.Phase
import Pitboss.FSM.Bout.Transition
import Pitboss.FSM.Types.Transitionable

data SomeBoutFSM = forall p. SomeBoutFSM (BoutFSM p)

instance Show SomeBoutFSM where
    show (SomeBoutFSM fsm) = show fsm

instance Eq SomeBoutFSM where
    SomeBoutFSM f1 == SomeBoutFSM f2 = case (f1, f2) of
        (AwaitingFirstCardFSM, AwaitingFirstCardFSM) -> True
        (AwaitingSecondCardFSM, AwaitingSecondCardFSM) -> True
        (PlayerTurnFSM, PlayerTurnFSM) -> True
        (DealerTurnFSM, DealerTurnFSM) -> True
        (SettlementFSM, SettlementFSM) -> True
        (DoneFSM, DoneFSM) -> True
        _ -> False

instance Transitionable SomeBoutFSM where
    transitionType (SomeBoutFSM fsm) = transitionType fsm

instance ToJSON SomeBoutFSM where
    toJSON (SomeBoutFSM fsm) = case fsm of
        AwaitingFirstCardFSM -> object ["tag" .= String "AwaitingFirstCard"]
        AwaitingSecondCardFSM -> object ["tag" .= String "AwaitingSecondCard"]
        PlayerTurnFSM -> object ["tag" .= String "PlayerTurn"]
        DealerTurnFSM -> object ["tag" .= String "DealerTurn"]
        SettlementFSM -> object ["tag" .= String "Settlement"]
        DoneFSM -> object ["tag" .= String "Done"]

instance FromJSON SomeBoutFSM where
    parseJSON = withObject "SomeBoutFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "AwaitingFirstCard" -> pure $ SomeBoutFSM AwaitingFirstCardFSM
            "AwaitingSecondCard" -> pure $ SomeBoutFSM AwaitingSecondCardFSM
            "PlayerTurn" -> pure $ SomeBoutFSM PlayerTurnFSM
            "DealerTurn" -> pure $ SomeBoutFSM DealerTurnFSM
            "Settlement" -> pure $ SomeBoutFSM SettlementFSM
            "Done" -> pure $ SomeBoutFSM DoneFSM
            _ -> fail $ "Unknown BoutFSM tag: " ++ tag
