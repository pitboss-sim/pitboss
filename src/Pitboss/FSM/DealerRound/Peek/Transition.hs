{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.DealerRound.Peek.Transition where

import Pitboss.Blackjack.Offering.RuleSet
import Pitboss.FSM.DealerRound.Peek.FSM
import Pitboss.FSM.DealerRound.Peek.Phase

beginPeek :: PeekFSM 'PeekAwaiting -> PeekFSM 'PeekBets
beginPeek PeekAwaitingFSM = PeekBetsFSM

betsPlacedPeek :: PeekFSM 'PeekBets -> PeekFSM 'PeekDeal
betsPlacedPeek PeekBetsFSM = PeekDealFSM

dealCardsPeek :: PeekFSM 'PeekDeal -> PeekFSM 'PeekEarlySurrender
dealCardsPeek PeekDealFSM = PeekEarlySurrenderFSM

earlySurrenderBlackjackPeek :: PeekFSM 'PeekEarlySurrender -> PeekFSM 'PeekComplete
earlySurrenderBlackjackPeek PeekEarlySurrenderFSM = PeekCompleteFSM

resolveEarlySurrenderPeek :: PeekFSM 'PeekEarlySurrender -> PeekFSM 'PeekPeek
resolveEarlySurrenderPeek PeekEarlySurrenderFSM = PeekPeekFSM

dealerBlackjackPeek :: PeekFSM 'PeekPeek -> PeekFSM 'PeekComplete
dealerBlackjackPeek PeekPeekFSM = PeekCompleteFSM

dealerNoBlackjackPeek :: PeekFSM 'PeekPeek -> PeekFSM 'PeekInsuranceDecision
dealerNoBlackjackPeek PeekPeekFSM = PeekInsuranceDecisionFSM

maybeEnterEarlySurrenderPeek ::
    RuleSet ->
    PeekFSM 'PeekDeal ->
    Either (PeekFSM 'PeekPeek) (PeekFSM 'PeekEarlySurrender)
maybeEnterEarlySurrenderPeek rules fsm =
    case surrender rules of
        Early -> Right (dealCardsPeek fsm)
        _ -> Left PeekPeekFSM

insuranceDecidedPeek :: PeekFSM 'PeekInsuranceDecision -> PeekFSM 'PeekInsuranceSettled
insuranceDecidedPeek PeekInsuranceDecisionFSM = PeekInsuranceSettledFSM

proceedToPlayersPeek :: PeekFSM 'PeekInsuranceSettled -> PeekFSM 'PeekPlayers
proceedToPlayersPeek PeekInsuranceSettledFSM = PeekPlayersFSM

finishPlayersPeek :: PeekFSM 'PeekPlayers -> PeekFSM 'PeekDealing
finishPlayersPeek PeekPlayersFSM = PeekDealingFSM

finishDealerPeek :: PeekFSM 'PeekDealing -> PeekFSM 'PeekSettle
finishDealerPeek PeekDealingFSM = PeekSettleFSM

resolvePayoutsPeek :: PeekFSM 'PeekSettle -> PeekFSM 'PeekComplete
resolvePayoutsPeek PeekSettleFSM = PeekCompleteFSM
