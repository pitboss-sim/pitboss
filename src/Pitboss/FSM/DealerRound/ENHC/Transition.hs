{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.DealerRound.ENHC.Transition where

import Pitboss.Blackjack.Offering.RuleSet
import Pitboss.FSM.DealerRound.ENHC.FSM
import Pitboss.FSM.DealerRound.ENHC.Phase

beginENHC :: ENHCFSM 'ENHCAwaiting -> ENHCFSM 'ENHCBets
beginENHC ENHCAwaitingFSM = ENHCBetsFSM

betsPlacedENHC :: ENHCFSM 'ENHCBets -> ENHCFSM 'ENHCDeal
betsPlacedENHC ENHCBetsFSM = ENHCDealFSM

dealCardsENHC :: ENHCFSM 'ENHCDeal -> ENHCFSM 'ENHCEarlySurrender
dealCardsENHC ENHCDealFSM = ENHCEarlySurrenderFSM

maybeEnterEarlySurrenderENHC ::
  RuleSet ->
  ENHCFSM 'ENHCDeal ->
  Either (ENHCFSM 'ENHCPlayers) (ENHCFSM 'ENHCEarlySurrender)
maybeEnterEarlySurrenderENHC rules fsm =
  case surrender rules of
    Early -> Right (dealCardsENHC fsm)
    _ -> Left ENHCPlayersFSM

insuranceDecidedENHC :: ENHCFSM 'ENHCEarlySurrender -> ENHCFSM 'ENHCPlayers
insuranceDecidedENHC = resolveEarlySurrenderENHC -- shim, for uniformity

resolveEarlySurrenderENHC :: ENHCFSM 'ENHCEarlySurrender -> ENHCFSM 'ENHCPlayers
resolveEarlySurrenderENHC ENHCEarlySurrenderFSM = ENHCPlayersFSM

finishPlayersENHC :: ENHCFSM 'ENHCPlayers -> ENHCFSM 'ENHCDealer
finishPlayersENHC ENHCPlayersFSM = ENHCDealerFSM

finishDealerENHC :: ENHCFSM 'ENHCDealer -> ENHCFSM 'ENHCSettle
finishDealerENHC ENHCDealerFSM = ENHCSettleFSM

resolvePayoutsENHC :: ENHCFSM 'ENHCSettle -> ENHCFSM 'ENHCComplete
resolvePayoutsENHC ENHCSettleFSM = ENHCCompleteFSM
