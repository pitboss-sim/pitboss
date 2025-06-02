{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.FSM.PlayerHand.Transition where

import Pitboss.Blackjack hiding (SplitAces, Push, Stand, HandPhase)
import Pitboss.FSM.PlayerHand.FSM
import Pitboss.FSM.PlayerHand.Phase

type family ValidPlayerHandTransition (from :: HandPhase) (to :: HandPhase) :: Bool where
    ValidPlayerHandTransition 'Decision 'Hitting = 'True
    ValidPlayerHandTransition 'Decision ('OneCardDraw reason) = 'True
    ValidPlayerHandTransition 'Decision ('Resolved res) = 'True
    ValidPlayerHandTransition 'Hitting 'Hitting = 'True
    ValidPlayerHandTransition 'Hitting ('Resolved 'Bust) = 'True
    ValidPlayerHandTransition 'Hitting ('Resolved 'Stand) = 'True
    ValidPlayerHandTransition ('OneCardDraw reason) ('Resolved res) = 'True
    ValidPlayerHandTransition _ _ = 'False

initialDecisionTyped :: SomeHand -> PlayerHandFSM 'Decision 'OKHit 'OKDbl 'OKSpl
initialDecisionTyped _hand = DecisionFSM

resolveSurrenderTyped ::
    (ValidPlayerHandTransition 'Decision ('Resolved 'Surrendered) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'Decision h d s ->
    PlayerHandFSM ('Resolved 'Surrendered) 'NoHit 'NoDbl 'NoSpl
resolveSurrenderTyped _hand DecisionFSM = ResolvedFSM Surrendered

toOneCardDrawFromDecisionTyped ::
    (ValidPlayerHandTransition 'Decision ('OneCardDraw reason) ~ 'True) =>
    OneCardDrawReason ->
    SomeHand ->
    PlayerHandFSM 'Decision 'OKHit d s ->
    PlayerHandFSM ('OneCardDraw reason) 'NoHit 'NoDbl 'NoSpl
toOneCardDrawFromDecisionTyped reason _hand DecisionFSM = OneCardDrawFSM reason

toHittingTyped ::
    (ValidPlayerHandTransition 'Decision 'Hitting ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'Decision 'OKHit d s ->
    PlayerHandFSM 'Hitting 'OKHit d s
toHittingTyped _hand DecisionFSM = HittingFSM

continueHittingTyped ::
    (ValidPlayerHandTransition 'Hitting 'Hitting ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'Hitting h d s ->
    PlayerHandFSM 'Hitting h d s
continueHittingTyped _hand HittingFSM = HittingFSM

resolveStandTyped ::
    (ValidPlayerHandTransition 'Decision ('Resolved 'Stand) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'Decision h d s ->
    PlayerHandFSM ('Resolved 'Stand) 'NoHit 'NoDbl 'NoSpl
resolveStandTyped _hand DecisionFSM = ResolvedFSM Stand

resolveBustTyped ::
    (ValidPlayerHandTransition 'Hitting ('Resolved 'Bust) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'Hitting h d s ->
    PlayerHandFSM ('Resolved 'Bust) 'NoHit 'NoDbl 'NoSpl
resolveBustTyped (SomeHand hand) HittingFSM = case witness hand of
    BustWitness -> ResolvedFSM Bust
    _ -> error "resolveBustTyped: hand is not busted"

resolveOneCardDrawTyped ::
    (ValidPlayerHandTransition ('OneCardDraw reason) ('Resolved res) ~ 'True) =>
    SomeHand ->
    PlayerHandResolution ->
    PlayerHandFSM ('OneCardDraw reason) 'NoHit 'NoDbl 'NoSpl ->
    PlayerHandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl
resolveOneCardDrawTyped (SomeHand hand) res (OneCardDrawFSM _) = case (witness hand, res) of
    (BustWitness, Bust) -> ResolvedFSM res
    (BlackjackWitness, Blackjack) -> ResolvedFSM res
    (_, Stand) | handScore (SomeHand hand) <= 21 -> ResolvedFSM res
    _ -> error "resolveOneCardDrawTyped: invalid resolution for hand"

resolveSplitTyped ::
    (ValidPlayerHandTransition 'Decision ('Resolved res) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'Decision h d s ->
    Offering ->
    PlayerHandFSM ('Resolved res) 'NoHit 'NoDbl 'NoSpl
resolveSplitTyped someHand DecisionFSM offering =
    case fromSomeHand someHand of
        SomeLifecycleHand fullHand@(FullLifecycleHand _) ->
            if canSplitHand fullHand 0 offering
                then case extractPairRank someHand of
                    Just Ace -> ResolvedFSM SplitAces
                    Just _ -> ResolvedFSM SplitNonAces
                    Nothing -> error "resolveSplitTyped: not a pair"
                else error "resolveSplitTyped: hand cannot be split"
        _ -> error "resolveSplitTyped: not a full hand"

resolvePushTyped ::
    (ValidPlayerHandTransition from ('Resolved 'Push) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM from h d s ->
    PlayerHandFSM ('Resolved 'Push) 'NoHit 'NoDbl 'NoSpl
resolvePushTyped _hand _ = ResolvedFSM Push

resolveDealerBlackjackTyped ::
    (ValidPlayerHandTransition from ('Resolved 'DealerBlackjack) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM from h d s ->
    PlayerHandFSM ('Resolved 'DealerBlackjack) 'NoHit 'NoDbl 'NoSpl
resolveDealerBlackjackTyped _hand _ = ResolvedFSM DealerBlackjack

resolveVoidTyped ::
    (ValidPlayerHandTransition from ('Resolved ('Void impact)) ~ 'True) =>
    SomeHand ->
    BankrollImpact ->
    PlayerHandFSM from h d s ->
    PlayerHandFSM ('Resolved ('Void impact)) 'NoHit 'NoDbl 'NoSpl
resolveVoidTyped _hand impact _ = ResolvedFSM (Void impact)
