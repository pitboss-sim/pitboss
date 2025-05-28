{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.PlayerHand.Transition where

import Pitboss.Blackjack.Card (Rank (..))
import Pitboss.Blackjack.Hand
import Pitboss.FSM.PlayerHand.FSM
import Pitboss.FSM.PlayerHand.Phase
import Pitboss.Blackjack.Play (canSplitHand, SomeLifecycleHand (..), LifecycleHand (..), fromSomeHand)
import Pitboss.Blackjack.Offering hiding (SplitAces)

initialDecisionTyped :: SomeHand -> PlayerHandFSM 'Decision 'OKHit 'OKDbl 'OKSpl
initialDecisionTyped _hand = DecisionFSM

resolveSurrenderTyped :: SomeHand -> PlayerHandFSM 'Decision h d s -> PlayerHandFSM ('Resolved 'Surrendered) 'NoHit 'NoDbl 'NoSpl
resolveSurrenderTyped _hand DecisionFSM = ResolvedFSM Surrendered

toOneCardDrawFromDecisionTyped :: OneCardDrawReason -> SomeHand -> PlayerHandFSM 'Decision 'OKHit d s -> PlayerHandFSM ('OneCardDraw reason) 'NoHit 'NoDbl 'NoSpl
toOneCardDrawFromDecisionTyped reason _hand DecisionFSM = OneCardDrawFSM reason

toHittingTyped :: SomeHand -> PlayerHandFSM 'Decision 'OKHit d s -> PlayerHandFSM 'Hitting 'OKHit d s
toHittingTyped _hand DecisionFSM = HittingFSM

continueHittingTyped :: SomeHand -> PlayerHandFSM 'Hitting h d s -> PlayerHandFSM 'Hitting h d s
continueHittingTyped _hand HittingFSM = HittingFSM

resolveStandTyped :: SomeHand -> PlayerHandFSM 'Decision h d s -> PlayerHandFSM ('Resolved 'Stand) 'NoHit 'NoDbl 'NoSpl
resolveStandTyped _hand DecisionFSM = ResolvedFSM Stand

resolveBustTyped :: SomeHand -> PlayerHandFSM 'Hitting h d s -> PlayerHandFSM ('Resolved 'Bust) 'NoHit 'NoDbl 'NoSpl
resolveBustTyped (SomeHand hand) HittingFSM = case witness hand of
    BustWitness -> ResolvedFSM Bust
    _ -> error "resolveBustTyped: hand is not busted"

resolveOneCardDrawTyped :: SomeHand -> PlayerHandResolution -> PlayerHandFSM ('OneCardDraw reason) 'NoHit 'NoDbl 'NoSpl -> PlayerHandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl
resolveOneCardDrawTyped (SomeHand hand) res (OneCardDrawFSM _) = case (witness hand, res) of
    (BustWitness, Bust) -> ResolvedFSM res
    (BlackjackWitness, Blackjack) -> ResolvedFSM res
    (_, Stand) | handScore (SomeHand hand) <= 21 -> ResolvedFSM res
    _ -> error "resolveOneCardDrawTyped: invalid resolution for hand"

resolveSplitTyped :: SomeHand -> PlayerHandFSM 'Decision h d s -> Offering -> PlayerHandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl
resolveSplitTyped someHand DecisionFSM offering =
    case fromSomeHand someHand of
        SomeLifecycleHand fullHand@(FullLifecycleHand _) ->
            if canSplitHand fullHand 0 offering -- you'll need offering here too
            then case extractPairRank someHand of
                Just Ace -> ResolvedFSM SplitAces
                Just _ -> ResolvedFSM SplitNonAces
                Nothing -> error "resolveSplitTyped: not a pair"
            else error "resolveSplitTyped: hand cannot be split"
        _ -> error "resolveSplitTyped: not a full hand"

resolvePushTyped :: SomeHand -> PlayerHandFSM p h d s -> PlayerHandFSM ('Resolved 'Push) 'NoHit 'NoDbl 'NoSpl
resolvePushTyped _hand _ = ResolvedFSM Push

resolveDealerBlackjackTyped :: SomeHand -> PlayerHandFSM p h d s -> PlayerHandFSM ('Resolved 'DealerBlackjack) 'NoHit 'NoDbl 'NoSpl
resolveDealerBlackjackTyped _hand _ = ResolvedFSM DealerBlackjack

resolveVoidTyped :: SomeHand -> BankrollImpact -> PlayerHandFSM p h d s -> PlayerHandFSM ('Resolved ('Void impact)) 'NoHit 'NoDbl 'NoSpl
resolveVoidTyped _hand impact _ = ResolvedFSM (Void impact)
