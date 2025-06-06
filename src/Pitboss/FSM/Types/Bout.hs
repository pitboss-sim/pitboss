{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.Types.Bout where

import Data.Aeson.Types
import Pitboss.FSM.Types.Core

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
