{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.DealerRound.Peek (
    module Pitboss.FSM.DealerRound.Peek.FSM,
    module Pitboss.FSM.DealerRound.Peek.Phase,
    module Pitboss.FSM.DealerRound.Peek.Transition,
    SomePeekFSM (..),
)
where

import Data.Aeson
import Pitboss.FSM.DealerRound.Peek.FSM
import Pitboss.FSM.DealerRound.Peek.Phase
import Pitboss.FSM.DealerRound.Peek.Transition
import Pitboss.FSM.Types.Transitionable

data SomePeekFSM = forall p. SomePeekFSM (PeekFSM p)

instance Eq SomePeekFSM where
    SomePeekFSM a == SomePeekFSM b = case (a, b) of
        (PeekAwaitingFSM, PeekAwaitingFSM) -> True
        (PeekBetsFSM, PeekBetsFSM) -> True
        (PeekDealFSM, PeekDealFSM) -> True
        (PeekEarlySurrenderFSM, PeekEarlySurrenderFSM) -> True
        (PeekPeekFSM, PeekPeekFSM) -> True
        (PeekInsuranceDecisionFSM, PeekInsuranceDecisionFSM) -> True
        (PeekInsuranceSettledFSM, PeekInsuranceSettledFSM) -> True
        (PeekPlayersFSM, PeekPlayersFSM) -> True
        (PeekDealingFSM, PeekDealingFSM) -> True
        (PeekSettleFSM, PeekSettleFSM) -> True
        (PeekCompleteFSM, PeekCompleteFSM) -> True
        _ -> False

instance Transitionable SomePeekFSM where
    transitionType (SomePeekFSM fsm) = transitionType fsm

instance Show SomePeekFSM where
    show (SomePeekFSM fsm) = show fsm

instance ToJSON SomePeekFSM where
    toJSON (SomePeekFSM fsm) = case fsm of
        PeekAwaitingFSM -> object ["tag" .= String "PeekAwaiting"]
        PeekBetsFSM -> object ["tag" .= String "PeekBets"]
        PeekDealFSM -> object ["tag" .= String "PeekDeal"]
        PeekEarlySurrenderFSM -> object ["tag" .= String "PeekEarlySurrender"]
        PeekPeekFSM -> object ["tag" .= String "PeekPeek"]
        PeekInsuranceDecisionFSM -> object ["tag" .= String "PeekInsuranceDecision"]
        PeekInsuranceSettledFSM -> object ["tag" .= String "PeekInsuranceSettled"]
        PeekPlayersFSM -> object ["tag" .= String "PeekPlayers"]
        PeekDealingFSM -> object ["tag" .= String "PeekDealing"]
        PeekSettleFSM -> object ["tag" .= String "PeekSettle"]
        PeekCompleteFSM -> object ["tag" .= String "PeekComplete"]
        PeekInterruptedFSM _ -> object ["tag" .= String "PeekComplete"]

instance FromJSON SomePeekFSM where
    parseJSON = withObject "SomePeekFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "PeekAwaiting" -> pure $ SomePeekFSM PeekAwaitingFSM
            "PeekBets" -> pure $ SomePeekFSM PeekBetsFSM
            "PeekDeal" -> pure $ SomePeekFSM PeekDealFSM
            "PeekEarlySurrender" -> pure $ SomePeekFSM PeekEarlySurrenderFSM
            "PeekPeek" -> pure $ SomePeekFSM PeekPeekFSM
            "PeekInsuranceDecision" -> pure $ SomePeekFSM PeekInsuranceDecisionFSM
            "PeekInsuranceSettled" -> pure $ SomePeekFSM PeekInsuranceSettledFSM
            "PeekPlayers" -> pure $ SomePeekFSM PeekPlayersFSM
            "PeekDealing" -> pure $ SomePeekFSM PeekDealingFSM
            "PeekSettle" -> pure $ SomePeekFSM PeekSettleFSM
            "PeekComplete" -> pure $ SomePeekFSM PeekCompleteFSM
            _ -> fail $ "Unknown PeekFSM tag: " ++ tag
