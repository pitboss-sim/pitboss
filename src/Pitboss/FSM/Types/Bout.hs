{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.Types.Bout where

import Data.Aeson.Types
import Pitboss.FSM.Transitionable
import Pitboss.FSM.Types.Core

data BoutPhase
    = BAwaitingFirstCard
    | BAwaitingSecondCard
    | BPlayerTurn
    | BDealerTurn
    | BSettlement
    | BDone
    deriving (Eq, Show)

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

instance Transitionable SomeBoutFSM where
    transitionType (SomeBoutFSM fsm) = transitionType fsm

data BoutFSM (p :: BoutPhase) where
    BAwaitingFirstCardFSM :: BoutFSM 'BAwaitingFirstCard
    BAwaitingSecondCardFSM :: BoutFSM 'BAwaitingSecondCard
    BPlayerTurnFSM :: BoutFSM 'BPlayerTurn
    BDealerTurnFSM :: BoutFSM 'BDealerTurn
    BSettlementFSM :: BoutFSM 'BSettlement
    BDoneFSM :: BoutFSM 'BDone

deriving instance Show (BoutFSM p)
deriving instance Eq (BoutFSM p)

instance Transitionable (BoutFSM p) where
    transitionType = \case
        BAwaitingFirstCardFSM -> AwaitInput
        BAwaitingSecondCardFSM -> AwaitInput
        BPlayerTurnFSM -> AwaitInput
        BDealerTurnFSM -> AutoAdvance
        BSettlementFSM -> AutoAdvance
        BDoneFSM -> TerminalPhase
