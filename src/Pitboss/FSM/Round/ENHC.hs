{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Round.ENHC where

import Pitboss.Blackjack
import Pitboss.FSM.Types

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
    ENHCFSM 'ENHCAwaiting ->
    ENHCFSM 'ENHCBets
beginENHC ENHCAwaitingFSM = ENHCBetsFSM

betsPlacedENHC ::
    ENHCFSM 'ENHCBets ->
    ENHCFSM 'ENHCDeal
betsPlacedENHC ENHCBetsFSM = ENHCDealFSM

dealCardsENHC ::
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
    ENHCFSM 'ENHCEarlySurrender ->
    ENHCFSM 'ENHCPlayers
insuranceDecidedENHC = resolveEarlySurrenderENHC

resolveEarlySurrenderENHC ::
    ENHCFSM 'ENHCEarlySurrender ->
    ENHCFSM 'ENHCPlayers
resolveEarlySurrenderENHC ENHCEarlySurrenderFSM = ENHCPlayersFSM

finishPlayersENHC ::
    ENHCFSM 'ENHCPlayers ->
    ENHCFSM 'ENHCDealing
finishPlayersENHC ENHCPlayersFSM = ENHCDealingFSM

finishDealerENHC ::
    ENHCFSM 'ENHCDealing ->
    ENHCFSM 'ENHCSettle
finishDealerENHC ENHCDealingFSM = ENHCSettleFSM

resolvePayoutsENHC ::
    ENHCFSM 'ENHCSettle ->
    ENHCFSM 'ENHCComplete
resolvePayoutsENHC ENHCSettleFSM = ENHCCompleteFSM
