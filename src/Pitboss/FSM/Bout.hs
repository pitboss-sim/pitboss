{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.Bout (
    module Pitboss.FSM.Bout.FSM,
    module Pitboss.FSM.Bout.Phase,
    module Pitboss.FSM.Bout.Transition,
    SomeBoutFSM (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.=))
import Pitboss.FSM.Bout.FSM
import Pitboss.FSM.Bout.Phase
import Pitboss.FSM.Bout.Transition
import Pitboss.FSM.Types.Transitionable

data SomeBoutFSM = forall p. SomeBoutFSM (BoutFSM p)

instance Show SomeBoutFSM where
    show (SomeBoutFSM fsm) = show fsm

instance Eq SomeBoutFSM where
    SomeBoutFSM f1 == SomeBoutFSM f2 = case (f1, f2) of
        (BAwaitingFirstCardFSM, BAwaitingFirstCardFSM) -> True
        (BAwaitingSecondCardFSM, BAwaitingSecondCardFSM) -> True
        (BPlayerTurnFSM, BPlayerTurnFSM) -> True
        (BDealerTurnFSM, BDealerTurnFSM) -> True
        (BSettlementFSM, BSettlementFSM) -> True
        (BDoneFSM, BDoneFSM) -> True
        _ -> False

instance Transitionable SomeBoutFSM where
    transitionType (SomeBoutFSM fsm) = transitionType fsm

instance ToJSON SomeBoutFSM where
    toJSON (SomeBoutFSM fsm) = case fsm of
        BAwaitingFirstCardFSM -> object ["tag" .= String "AwaitingFirstCard"]
        BAwaitingSecondCardFSM -> object ["tag" .= String "AwaitingSecondCard"]
        BPlayerTurnFSM -> object ["tag" .= String "PlayerTurn"]
        BDealerTurnFSM -> object ["tag" .= String "DealerTurn"]
        BSettlementFSM -> object ["tag" .= String "Settlement"]
        BDoneFSM -> object ["tag" .= String "Done"]

instance FromJSON SomeBoutFSM where
    parseJSON = withObject "SomeBoutFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "AwaitingFirstCard" -> pure $ SomeBoutFSM BAwaitingFirstCardFSM
            "AwaitingSecondCard" -> pure $ SomeBoutFSM BAwaitingSecondCardFSM
            "PlayerTurn" -> pure $ SomeBoutFSM BPlayerTurnFSM
            "DealerTurn" -> pure $ SomeBoutFSM BDealerTurnFSM
            "Settlement" -> pure $ SomeBoutFSM BSettlementFSM
            "Done" -> pure $ SomeBoutFSM BDoneFSM
            _ -> fail $ "Unknown BoutFSM tag: " ++ tag
