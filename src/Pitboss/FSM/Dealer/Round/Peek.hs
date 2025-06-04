{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Dealer.Round.Peek (
    SomePeekFSM (..),
    PeekPhase (..),
    PeekFSM (..),
    beginPeek,
    betsPlacedPeek,
    dealCardsPeek,
    earlySurrenderBlackjackPeek,
    resolveEarlySurrenderPeek,
    dealerBlackjackPeek,
    dealerNoBlackjackPeek,
    maybeEnterEarlySurrenderPeek,
    insuranceDecidedPeek,
    proceedToPlayersPeek,
    finishPlayersPeek,
    finishDealerPeek,
    resolvePayoutsPeek,
)
where

import Data.Aeson
import Pitboss.Blackjack
import Pitboss.FSM.Types

data PeekPhase
    = PeekAwaiting
    | PeekBets
    | PeekDeal
    | PeekEarlySurrender
    | PeekPeek
    | PeekInsuranceDecision
    | PeekInsuranceSettled
    | PeekPlayers
    | PeekDealing
    | PeekSettle
    | PeekComplete
    | PeekInterrupted

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

data PeekFSM (p :: PeekPhase) where
    PeekAwaitingFSM :: PeekFSM 'PeekAwaiting
    PeekBetsFSM :: PeekFSM 'PeekBets
    PeekDealFSM :: PeekFSM 'PeekDeal
    PeekEarlySurrenderFSM :: PeekFSM 'PeekEarlySurrender
    PeekPeekFSM :: PeekFSM 'PeekPeek
    PeekInsuranceDecisionFSM :: PeekFSM 'PeekInsuranceDecision
    PeekInsuranceSettledFSM :: PeekFSM 'PeekInsuranceSettled
    PeekPlayersFSM :: PeekFSM 'PeekPlayers
    PeekDealingFSM :: PeekFSM 'PeekDealing
    PeekSettleFSM :: PeekFSM 'PeekSettle
    PeekCompleteFSM :: PeekFSM 'PeekComplete
    PeekInterruptedFSM :: InterruptReason -> PeekFSM 'PeekInterrupted

deriving instance Eq (PeekFSM p)
deriving instance Show (PeekFSM p)

type family ValidPeekTransition (from :: PeekPhase) (to :: PeekPhase) :: Bool where
    ValidPeekTransition 'PeekAwaiting 'PeekBets = 'True
    ValidPeekTransition 'PeekBets 'PeekDeal = 'True
    ValidPeekTransition 'PeekDeal 'PeekEarlySurrender = 'True
    ValidPeekTransition 'PeekDeal 'PeekPeek = 'True
    ValidPeekTransition 'PeekEarlySurrender 'PeekPeek = 'True
    ValidPeekTransition 'PeekEarlySurrender 'PeekComplete = 'True
    ValidPeekTransition 'PeekPeek 'PeekInsuranceDecision = 'True
    ValidPeekTransition 'PeekPeek 'PeekComplete = 'True
    ValidPeekTransition 'PeekInsuranceDecision 'PeekInsuranceSettled = 'True
    ValidPeekTransition 'PeekInsuranceSettled 'PeekPlayers = 'True
    ValidPeekTransition 'PeekPlayers 'PeekDealing = 'True
    ValidPeekTransition 'PeekDealing 'PeekSettle = 'True
    ValidPeekTransition 'PeekSettle 'PeekComplete = 'True
    ValidPeekTransition p 'PeekInterrupted = 'True
    ValidPeekTransition 'PeekInterrupted p = 'True
    ValidPeekTransition _ _ = 'False

beginPeek ::
    (ValidPeekTransition 'PeekAwaiting 'PeekBets ~ 'True) =>
    PeekFSM 'PeekAwaiting ->
    PeekFSM 'PeekBets
beginPeek PeekAwaitingFSM = PeekBetsFSM

betsPlacedPeek ::
    (ValidPeekTransition 'PeekBets 'PeekDeal ~ 'True) =>
    PeekFSM 'PeekBets ->
    PeekFSM 'PeekDeal
betsPlacedPeek PeekBetsFSM = PeekDealFSM

dealCardsPeek ::
    (ValidPeekTransition 'PeekDeal 'PeekEarlySurrender ~ 'True) =>
    PeekFSM 'PeekDeal ->
    PeekFSM 'PeekEarlySurrender
dealCardsPeek PeekDealFSM = PeekEarlySurrenderFSM

earlySurrenderBlackjackPeek ::
    (ValidPeekTransition 'PeekEarlySurrender 'PeekComplete ~ 'True) =>
    PeekFSM 'PeekEarlySurrender ->
    PeekFSM 'PeekComplete
earlySurrenderBlackjackPeek PeekEarlySurrenderFSM = PeekCompleteFSM

resolveEarlySurrenderPeek ::
    (ValidPeekTransition 'PeekEarlySurrender 'PeekPeek ~ 'True) =>
    PeekFSM 'PeekEarlySurrender ->
    PeekFSM 'PeekPeek
resolveEarlySurrenderPeek PeekEarlySurrenderFSM = PeekPeekFSM

dealerBlackjackPeek ::
    (ValidPeekTransition 'PeekPeek 'PeekComplete ~ 'True) =>
    PeekFSM 'PeekPeek ->
    PeekFSM 'PeekComplete
dealerBlackjackPeek PeekPeekFSM = PeekCompleteFSM

dealerNoBlackjackPeek ::
    (ValidPeekTransition 'PeekPeek 'PeekInsuranceDecision ~ 'True) =>
    PeekFSM 'PeekPeek ->
    PeekFSM 'PeekInsuranceDecision
dealerNoBlackjackPeek PeekPeekFSM = PeekInsuranceDecisionFSM

maybeEnterEarlySurrenderPeek ::
    GameRuleSet ->
    PeekFSM 'PeekDeal ->
    Either (PeekFSM 'PeekPeek) (PeekFSM 'PeekEarlySurrender)
maybeEnterEarlySurrenderPeek rules fsm =
    case surrender rules of
        Early -> Right (dealCardsPeek fsm)
        _ -> Left PeekPeekFSM

insuranceDecidedPeek ::
    (ValidPeekTransition 'PeekInsuranceDecision 'PeekInsuranceSettled ~ 'True) =>
    PeekFSM 'PeekInsuranceDecision ->
    PeekFSM 'PeekInsuranceSettled
insuranceDecidedPeek PeekInsuranceDecisionFSM = PeekInsuranceSettledFSM

proceedToPlayersPeek ::
    (ValidPeekTransition 'PeekInsuranceSettled 'PeekPlayers ~ 'True) =>
    PeekFSM 'PeekInsuranceSettled ->
    PeekFSM 'PeekPlayers
proceedToPlayersPeek PeekInsuranceSettledFSM = PeekPlayersFSM

finishPlayersPeek ::
    (ValidPeekTransition 'PeekPlayers 'PeekDealing ~ 'True) =>
    PeekFSM 'PeekPlayers ->
    PeekFSM 'PeekDealing
finishPlayersPeek PeekPlayersFSM = PeekDealingFSM

finishDealerPeek ::
    (ValidPeekTransition 'PeekDealing 'PeekSettle ~ 'True) =>
    PeekFSM 'PeekDealing ->
    PeekFSM 'PeekSettle
finishDealerPeek PeekDealingFSM = PeekSettleFSM

resolvePayoutsPeek ::
    (ValidPeekTransition 'PeekSettle 'PeekComplete ~ 'True) =>
    PeekFSM 'PeekSettle ->
    PeekFSM 'PeekComplete
resolvePayoutsPeek PeekSettleFSM = PeekCompleteFSM
