{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.FSM.Dealer.Round.ENHC.Transition where

import Pitboss.Blackjack
import Pitboss.FSM.Dealer.Round.ENHC.FSM
import Pitboss.FSM.Dealer.Round.ENHC.Phase

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
