{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Dealer.Round.ENHC (
    SomeENHCFSM (..),
    ENHCPhase (..),
    ENHCFSM (..),
    beginENHC,
    betsPlacedENHC,
    dealCardsENHC,
    maybeEnterEarlySurrenderENHC,
    insuranceDecidedENHC,
    finishPlayersENHC,
    finishDealerENHC,
    resolvePayoutsENHC,
)
where

import Data.Aeson
import Pitboss.Blackjack
import Pitboss.FSM.Transitionable
import Pitboss.FSM.Types

data ENHCPhase
    = ENHCAwaiting
    | ENHCBets
    | ENHCDeal
    | ENHCEarlySurrender
    | ENHCPlayers
    | ENHCDealing
    | ENHCSettle
    | ENHCComplete
    | ENHCInterrupted

data SomeENHCFSM = forall p. SomeENHCFSM (ENHCFSM p)

instance Show SomeENHCFSM where
    show (SomeENHCFSM fsm) = show fsm

instance Eq SomeENHCFSM where
    SomeENHCFSM a == SomeENHCFSM b = case (a, b) of
        (ENHCAwaitingFSM, ENHCAwaitingFSM) -> True
        (ENHCBetsFSM, ENHCBetsFSM) -> True
        (ENHCDealFSM, ENHCDealFSM) -> True
        (ENHCEarlySurrenderFSM, ENHCEarlySurrenderFSM) -> True
        (ENHCPlayersFSM, ENHCPlayersFSM) -> True
        (ENHCDealingFSM, ENHCDealingFSM) -> True
        (ENHCSettleFSM, ENHCSettleFSM) -> True
        (ENHCCompleteFSM, ENHCCompleteFSM) -> True
        _ -> False

instance ToJSON SomeENHCFSM where
    toJSON (SomeENHCFSM fsm) = case fsm of
        ENHCAwaitingFSM -> object ["tag" .= String "ENHCAwaiting"]
        ENHCBetsFSM -> object ["tag" .= String "ENHCBets"]
        ENHCDealFSM -> object ["tag" .= String "ENHCDeal"]
        ENHCEarlySurrenderFSM -> object ["tag" .= String "ENHCEarlySurrender"]
        ENHCPlayersFSM -> object ["tag" .= String "ENHCPlayers"]
        ENHCDealingFSM -> object ["tag" .= String "ENHCDealing"]
        ENHCSettleFSM -> object ["tag" .= String "ENHCSettle"]
        ENHCCompleteFSM -> object ["tag" .= String "ENHCComplete"]
        ENHCInterruptedFSM _ -> object ["tag" .= String "ENHCInterrupted"]

instance FromJSON SomeENHCFSM where
    parseJSON = withObject "SomeENHCFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "ENHCAwaiting" -> pure $ SomeENHCFSM ENHCAwaitingFSM
            "ENHCBets" -> pure $ SomeENHCFSM ENHCBetsFSM
            "ENHCDeal" -> pure $ SomeENHCFSM ENHCDealFSM
            "ENHCEarlySurrender" -> pure $ SomeENHCFSM ENHCEarlySurrenderFSM
            "ENHCPlayers" -> pure $ SomeENHCFSM ENHCPlayersFSM
            "ENHCDealing" -> pure $ SomeENHCFSM ENHCDealingFSM
            "ENHCSettle" -> pure $ SomeENHCFSM ENHCSettleFSM
            "ENHCComplete" -> pure $ SomeENHCFSM ENHCCompleteFSM
            _ -> fail $ "Unknown ENHCFSM tag: " ++ tag

instance Transitionable SomeENHCFSM where
    transitionType (SomeENHCFSM fsm) = transitionType fsm

data ENHCFSM (p :: ENHCPhase) where
    ENHCAwaitingFSM :: ENHCFSM 'ENHCAwaiting
    ENHCBetsFSM :: ENHCFSM 'ENHCBets
    ENHCDealFSM :: ENHCFSM 'ENHCDeal
    ENHCEarlySurrenderFSM :: ENHCFSM 'ENHCEarlySurrender
    ENHCPlayersFSM :: ENHCFSM 'ENHCPlayers
    ENHCDealingFSM :: ENHCFSM 'ENHCDealing
    ENHCSettleFSM :: ENHCFSM 'ENHCSettle
    ENHCCompleteFSM :: ENHCFSM 'ENHCComplete
    ENHCInterruptedFSM :: InterruptReason -> ENHCFSM 'ENHCInterrupted

deriving instance Eq (ENHCFSM p)
deriving instance Show (ENHCFSM p)

instance Transitionable (ENHCFSM p) where
    transitionType = \case
        ENHCAwaitingFSM -> AwaitInput
        ENHCBetsFSM -> AwaitInput
        ENHCDealFSM -> AutoAdvance
        ENHCEarlySurrenderFSM -> AwaitInput
        ENHCPlayersFSM -> AwaitInput
        ENHCDealingFSM -> AutoAdvance
        ENHCSettleFSM -> AutoAdvance
        ENHCCompleteFSM -> TerminalPhase
        ENHCInterruptedFSM _ -> AwaitInput

type family ValidENHCTransition (from :: ENHCPhase) (to :: ENHCPhase) :: Bool where
    ValidENHCTransition 'ENHCAwaiting 'ENHCBets = 'True
    ValidENHCTransition 'ENHCBets 'ENHCDeal = 'True
    ValidENHCTransition 'ENHCDeal 'ENHCEarlySurrender = 'True
    ValidENHCTransition 'ENHCDeal 'ENHCPlayers = 'True
    ValidENHCTransition 'ENHCEarlySurrender 'ENHCPlayers = 'True
    ValidENHCTransition 'ENHCPlayers 'ENHCDealing = 'True
    ValidENHCTransition 'ENHCDealing 'ENHCSettle = 'True
    ValidENHCTransition 'ENHCSettle 'ENHCComplete = 'True
    ValidENHCTransition p 'ENHCInterrupted = 'True
    ValidENHCTransition 'ENHCInterrupted p = 'True
    ValidENHCTransition _ _ = 'False

beginENHC ::
    (ValidENHCTransition 'ENHCAwaiting 'ENHCBets ~ 'True) =>
    ENHCFSM 'ENHCAwaiting ->
    ENHCFSM 'ENHCBets
beginENHC ENHCAwaitingFSM = ENHCBetsFSM

betsPlacedENHC ::
    (ValidENHCTransition 'ENHCBets 'ENHCDeal ~ 'True) =>
    ENHCFSM 'ENHCBets ->
    ENHCFSM 'ENHCDeal
betsPlacedENHC ENHCBetsFSM = ENHCDealFSM

dealCardsENHC ::
    (ValidENHCTransition 'ENHCDeal 'ENHCEarlySurrender ~ 'True) =>
    ENHCFSM 'ENHCDeal ->
    ENHCFSM 'ENHCEarlySurrender
dealCardsENHC ENHCDealFSM = ENHCEarlySurrenderFSM

maybeEnterEarlySurrenderENHC ::
    GameRuleSet ->
    ENHCFSM 'ENHCDeal ->
    Either (ENHCFSM 'ENHCPlayers) (ENHCFSM 'ENHCEarlySurrender)
maybeEnterEarlySurrenderENHC rules fsm =
    case surrender rules of
        Early -> Right (dealCardsENHC fsm)
        _ -> Left ENHCPlayersFSM

insuranceDecidedENHC ::
    (ValidENHCTransition 'ENHCEarlySurrender 'ENHCPlayers ~ 'True) =>
    ENHCFSM 'ENHCEarlySurrender ->
    ENHCFSM 'ENHCPlayers
insuranceDecidedENHC = resolveEarlySurrenderENHC

resolveEarlySurrenderENHC ::
    (ValidENHCTransition 'ENHCEarlySurrender 'ENHCPlayers ~ 'True) =>
    ENHCFSM 'ENHCEarlySurrender ->
    ENHCFSM 'ENHCPlayers
resolveEarlySurrenderENHC ENHCEarlySurrenderFSM = ENHCPlayersFSM

finishPlayersENHC ::
    (ValidENHCTransition 'ENHCPlayers 'ENHCDealing ~ 'True) =>
    ENHCFSM 'ENHCPlayers ->
    ENHCFSM 'ENHCDealing
finishPlayersENHC ENHCPlayersFSM = ENHCDealingFSM

finishDealerENHC ::
    (ValidENHCTransition 'ENHCDealing 'ENHCSettle ~ 'True) =>
    ENHCFSM 'ENHCDealing ->
    ENHCFSM 'ENHCSettle
finishDealerENHC ENHCDealingFSM = ENHCSettleFSM

resolvePayoutsENHC ::
    (ValidENHCTransition 'ENHCSettle 'ENHCComplete ~ 'True) =>
    ENHCFSM 'ENHCSettle ->
    ENHCFSM 'ENHCComplete
resolvePayoutsENHC ENHCSettleFSM = ENHCCompleteFSM
