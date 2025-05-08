{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.PlayerHand.Transition where

import Pitboss.Blackjack.Card
import Pitboss.Blackjack.Hand
import Pitboss.Blackjack.Hand.Category qualified as HC
import Pitboss.FSM.PlayerHand.FSM
import Pitboss.FSM.PlayerHand.Phase

initialDecision :: PlayerHandFSM 'Decision 'OKHit 'OKDbl 'OKSpl
initialDecision = DecisionFSM

resolveSurrender :: PlayerHandFSM 'Decision h d s -> PlayerHandFSM ('Resolved 'Surrendered) 'NoHit 'NoDbl 'NoSpl
resolveSurrender DecisionFSM = ResolvedFSM Surrendered

toOneCardDrawFromDecision :: OneCardDrawReason -> PlayerHandFSM 'Decision 'OKHit d s -> PlayerHandFSM ('OneCardDraw reason) 'NoHit 'NoDbl 'NoSpl
toOneCardDrawFromDecision reason DecisionFSM = OneCardDrawFSM reason

toHitting :: PlayerHandFSM 'Decision 'OKHit d s -> PlayerHandFSM 'Hitting 'OKHit d s
toHitting DecisionFSM = HittingFSM

continueHitting :: PlayerHandFSM 'Hitting h d s -> PlayerHandFSM 'Hitting h d s
continueHitting HittingFSM = HittingFSM

resolveStand :: PlayerHandFSM 'Decision h d s -> PlayerHandFSM ('Resolved 'Stand) 'NoHit 'NoDbl 'NoSpl
resolveStand DecisionFSM = ResolvedFSM Stand

resolveBust :: PlayerHandFSM 'Hitting h d s -> PlayerHandFSM ('Resolved 'Bust) 'NoHit 'NoDbl 'NoSpl
resolveBust HittingFSM = ResolvedFSM Bust

resolveOneCardDraw :: PlayerHandResolution -> PlayerHandFSM ('OneCardDraw reason) 'NoHit 'NoDbl 'NoSpl -> PlayerHandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl
resolveOneCardDraw res (OneCardDrawFSM _) = ResolvedFSM res

resolveSplit :: Hand -> PlayerHandFSM 'Decision h d s -> PlayerHandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl
resolveSplit hand DecisionFSM =
    case HC.categorize (unHand hand) of
        HC.Pair Ace -> ResolvedFSM SplitAces
        HC.Pair _ -> ResolvedFSM SplitNonAces
        _ -> error "resolveSplit: not a pair"

resolvePush, resolveDealerBlackjack :: PlayerHandFSM p h d s -> PlayerHandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl
resolvePush _ = ResolvedFSM Push
resolveDealerBlackjack _ = ResolvedFSM DealerBlackjack

resolveVoid :: BankrollImpact -> PlayerHandFSM p h d s -> PlayerHandFSM ('Resolved ('Void impact)) 'NoHit 'NoDbl 'NoSpl
resolveVoid impact _ = ResolvedFSM (Void impact)
