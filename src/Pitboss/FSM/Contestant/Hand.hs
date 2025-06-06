{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Contestant.Hand where

import Pitboss.Blackjack
import Pitboss.FSM.Types

resolutionImpact :: ContestantHandResolution -> Maybe BankrollImpact
resolutionImpact = \case
    CSurrendered -> Just Refund
    CPush -> Just Refund
    CBust -> Just Loss
    CDealerBlackjack -> Just Loss
    CVoid i -> Just i
    _ -> Nothing

type family ValidContestantHandTransition (from :: HandPhase) (to :: HandPhase) :: Bool where
    ValidContestantHandTransition 'CHDecision 'CHHitting = 'True
    ValidContestantHandTransition 'CHDecision ('CHAwaitingOneCard reason) = 'True
    ValidContestantHandTransition 'CHDecision ('CHResolved res) = 'True
    ValidContestantHandTransition 'CHHitting 'CHHitting = 'True
    ValidContestantHandTransition 'CHHitting ('CHResolved 'CBust) = 'True
    ValidContestantHandTransition 'CHHitting ('CHResolved 'CStand) = 'True
    ValidContestantHandTransition ('CHAwaitingOneCard reason) ('CHResolved res) = 'True
    ValidContestantHandTransition _ _ = 'False

initialDecision :: SomeHand -> ContestantHandFSM 'CHDecision 'OKHit 'OKDbl 'OKSpl
initialDecision _hand = CHDecisionFSM

resolveSurrender ::
    (ValidContestantHandTransition 'CHDecision ('CHResolved 'CSurrendered) ~ 'True) =>
    SomeHand ->
    GameRuleSet ->
    ContestantHandFSM 'CHDecision h d s ->
    Either String (ContestantHandFSM ('CHResolved 'CSurrendered) 'NoHit 'NoDbl 'NoSpl)
resolveSurrender hand rules CHDecisionFSM = do
    validateMoveInContext MSurrender hand rules 0
    Right (CHResolvedFSM CSurrendered)

toOneCardDrawFromDecision ::
    (ValidContestantHandTransition 'CHDecision ('CHAwaitingOneCard reason) ~ 'True) =>
    OneCardDrawReason ->
    SomeHand ->
    GameRuleSet ->
    Int ->
    ContestantHandFSM 'CHDecision 'OKHit d s ->
    Either String (ContestantHandFSM ('CHAwaitingOneCard reason) 'NoHit 'NoDbl 'NoSpl)
toOneCardDrawFromDecision reason hand rules splitCount CHDecisionFSM = do
    let move = case reason of
            OCDouble -> MDouble
            OCSplitAce -> MSplit
            OCSplitNonAce -> MSplit
    validateMoveInContext move hand rules splitCount
    Right (CHAwaitingOneCardFSM reason)

toHitting ::
    (ValidContestantHandTransition 'CHDecision 'CHHitting ~ 'True) =>
    SomeHand ->
    ContestantHandFSM 'CHDecision 'OKHit d s ->
    Either String (ContestantHandFSM 'CHHitting 'OKHit d s)
toHitting hand CHDecisionFSM = do
    if isLegalMove MHit hand undefined
        then Right CHHittingFSM
        else Left "Cannot hit: hand cannot take another card"

continueHitting ::
    (ValidContestantHandTransition 'CHHitting 'CHHitting ~ 'True) =>
    SomeHand ->
    ContestantHandFSM 'CHHitting h d s ->
    Either String (ContestantHandFSM 'CHHitting h d s)
continueHitting hand CHHittingFSM = do
    if isLegalMove MHit hand undefined
        then Right CHHittingFSM
        else Left "Cannot continue hitting: hand cannot take another card"

resolveStand ::
    (ValidContestantHandTransition 'CHDecision ('CHResolved 'CStand) ~ 'True) =>
    SomeHand ->
    ContestantHandFSM 'CHDecision h d s ->
    Either String (ContestantHandFSM ('CHResolved 'CStand) 'NoHit 'NoDbl 'NoSpl)
resolveStand _hand CHDecisionFSM = Right (CHResolvedFSM CStand)

resolveBust ::
    (ValidContestantHandTransition 'CHHitting ('CHResolved 'CBust) ~ 'True) =>
    SomeHand ->
    ContestantHandFSM 'CHHitting h d s ->
    Either String (ContestantHandFSM ('CHResolved 'CBust) 'NoHit 'NoDbl 'NoSpl)
resolveBust hand CHHittingFSM =
    let witness = handWitness hand
     in if valueType witness == BustWitness
            then Right (CHResolvedFSM CBust)
            else Left "Cannot resolve as bust: hand is not busted"

resolveOneCardDraw ::
    (ValidContestantHandTransition ('CHAwaitingOneCard reason) ('CHResolved res) ~ 'True) =>
    SomeHand ->
    ContestantHandResolution ->
    ContestantHandFSM ('CHAwaitingOneCard reason) 'NoHit 'NoDbl 'NoSpl ->
    Either String (ContestantHandFSM ('CHResolved res) 'NoHit 'NoDbl 'NoSpl)
resolveOneCardDraw hand res (CHAwaitingOneCardFSM _) =
    let witness = handWitness hand
        actualResolution = case valueType witness of
            BustWitness -> CBust
            BlackjackWitness -> CBlackjack
            _ -> CStand
     in if actualResolution == res
            then Right (CHResolvedFSM res)
            else Left $ "Resolution mismatch: expected " ++ show res ++ ", hand shows " ++ show actualResolution

resolveSplit ::
    (ValidContestantHandTransition 'CHDecision ('CHResolved res) ~ 'True) =>
    SomeHand ->
    GameRuleSet ->
    Int ->
    ContestantHandFSM 'CHDecision h d s ->
    Either String (ContestantHandFSM ('CHResolved res) 'NoHit 'NoDbl 'NoSpl)
resolveSplit hand rules splitCount CHDecisionFSM = do
    validateMoveInContext MSplit hand rules splitCount
    let witness = handWitness hand
    case structure witness of
        PairWitness Ace -> Right (CHResolvedFSM CSplitAces)
        PairWitness _ -> Right (CHResolvedFSM CSplitNonAces)
        _ -> Left "Cannot split: hand is not a pair"

resolvePush ::
    (ValidContestantHandTransition from ('CHResolved 'CPush) ~ 'True) =>
    SomeHand ->
    ContestantHandFSM from h d s ->
    ContestantHandFSM ('CHResolved 'CPush) 'NoHit 'NoDbl 'NoSpl
resolvePush _hand _ = CHResolvedFSM CPush

resolveDealerBlackjack ::
    (ValidContestantHandTransition from ('CHResolved 'CDealerBlackjack) ~ 'True) =>
    SomeHand ->
    ContestantHandFSM from h d s ->
    ContestantHandFSM ('CHResolved 'CDealerBlackjack) 'NoHit 'NoDbl 'NoSpl
resolveDealerBlackjack _hand _ = CHResolvedFSM CDealerBlackjack

resolveVoid ::
    (ValidContestantHandTransition from ('CHResolved ('CVoid impact)) ~ 'True) =>
    SomeHand ->
    BankrollImpact ->
    ContestantHandFSM from h d s ->
    ContestantHandFSM ('CHResolved ('CVoid impact)) 'NoHit 'NoDbl 'NoSpl
resolveVoid _hand impact _ = CHResolvedFSM (CVoid impact)
