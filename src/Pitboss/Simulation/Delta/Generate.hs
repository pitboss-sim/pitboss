{-# LANGUAGE GADTs #-}

module Pitboss.Simulation.Delta.Generate where

import Control.Monad.Except
import Control.Monad.Reader
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Blackjack.Rules.Hand qualified as Hand
import Pitboss.Causality
import Pitboss.Causality.Types.Core qualified as Core
import Pitboss.FSM
import Pitboss.Simulation.Delta.Bout
import Pitboss.Simulation.Delta.Dealer qualified as Dealer
import Pitboss.Simulation.Delta.Economy
import Pitboss.Simulation.Event qualified as Event

generateDeltas :: Event.SimulationEvent -> CausalHistory -> Reader TickCacheContext (Either ValidationErrors [TraceOp])
generateDeltas event history = runExceptT $ do
    case event of
        Event.Game blackjackEvent -> generateBlackjackDeltas blackjackEvent history
        Event.Economy economyEvent -> generateEconomyDeltas economyEvent history
        Event.Lifecycle lifecycleEvent -> generateLifecycleDeltas lifecycleEvent history

generateBlackjackDeltas :: Event.BlackjackEvent -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateBlackjackDeltas event history = do
    case event of
        Event.BoutPlayerStood boutPlayerId ->
            generateBoutPlayerStandDeltas boutPlayerId history
        Event.BoutPlayerHit boutPlayerId ->
            generateBoutPlayerHitDeltas boutPlayerId history
        Event.BoutPlayerDoubledDown boutPlayerId ->
            generateBoutPlayerDoubleDeltas boutPlayerId history
        Event.BoutPlayerSplit boutPlayerId ->
            generateBoutPlayerSplitDeltas boutPlayerId history
        Event.BoutPlayerSurrendered boutPlayerId ->
            generateBoutPlayerSurrenderDeltas boutPlayerId history
        Event.CardDrawn dealerId shoeId card ->
            Dealer.generateCardDrawnDeltas dealerId shoeId card history
        Event.BoutPlayerCardDealt card boutPlayerId ->
            generateBoutPlayerCardDealtDeltas card boutPlayerId history
        Event.BoutDealerCardDealt card boutDealerId ->
            generateBoutDealerCardDealtDeltas card boutDealerId history
        Event.BoutPlayerHandSet hand boutPlayerId ->
            generateBoutPlayerHandSetDeltas hand boutPlayerId history
        Event.BoutDealerHandSet hand boutDealerId ->
            generateBoutDealerHandSetDeltas hand boutDealerId history
        Event.BoutSettled boutId outcome ->
            generateBoutSettledDeltas boutId outcome history
        Event.InsuranceSettled playerIds ->
            generateInsuranceSettledDeltas playerIds history
        Event.BoutDealerRevealed boutDealerId ->
            Dealer.generateBoutDealerRevealDeltas boutDealerId history
        Event.BoutDealerHit boutDealerId ->
            Dealer.generateBoutDealerHitDeltas boutDealerId history
        Event.BoutDealerStood boutDealerId ->
            Dealer.generateBoutDealerStandDeltas boutDealerId history

generateLifecycleDeltas :: Event.LifecycleEvent -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateLifecycleDeltas event _history = do
    case event of
        Event.EntityCreated _witness eid state ->
            pure [Event.bear eid state]
        Event.EntityMutated witness eid someDelta ->
            pure [Event.mutate witness eid someDelta]
        Event.EntityRetired witness eid reason ->
            pure [Event.bury witness eid reason]

generateEconomyDeltas :: Event.EconomyEvent -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateEconomyDeltas event history = do
    case event of
        Event.BankrollCredit playerId chips ->
            generateBankrollCreditDeltas playerId chips history
        Event.BankrollDebit playerId chips ->
            generateBankrollDebitDeltas playerId chips history

localValidationToEither :: Validation e a -> Either e a
localValidationToEither (Success a) = Right a
localValidationToEither (Failure e) = Left e

generateBoutPlayerCardDealtDeltas :: Card -> BoutId -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateBoutPlayerCardDealtDeltas card boutId history = do
    bout <- ExceptT $ Dealer.validationToEither <$> derefV boutId
    let currentHand = _bAttrsPlayerHand (_bAttrs bout)
        SomeHand cards _witness = currentHand
        newCards = cards ++ [card]
        newHand = characterize newCards
        oldHand = currentHand
        attrDelta = AttrsDelta history (DBoutSetPlayerHand newHand oldHand)
        SomeHand _ newWitness = newHand
        deltas = [MutationOp Core.BoutWitness boutId attrDelta]
    if isBust newWitness
        then do
            let currentFSM = _bModesPlayerHandFSM (_bModes bout)
                newFSM = SomePlayerHandFSM (PHResolvedFSM PHBust)
                fsmDelta = ModesDelta history (DBoutSetPlayerHandFSM newFSM currentFSM)
            pure $ deltas ++ [MutationOp Core.BoutWitness boutId fsmDelta]
        else pure deltas

generateBoutDealerCardDealtDeltas :: Card -> BoutId -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateBoutDealerCardDealtDeltas card boutId history = do
    bout <- ExceptT $ Dealer.validationToEither <$> derefV boutId
    let currentHand = _bAttrsDealerHand (_bAttrs bout)
        SomeHand cards _witness = currentHand
        newCards = cards ++ [card]
        newHand = characterize newCards
        oldHand = currentHand
        attrDelta = AttrsDelta history (DBoutSetDealerHand newHand oldHand)

        SomeHand _ newWitness = newHand
        deltas = [MutationOp Core.BoutWitness boutId attrDelta]

    if isBust newWitness
        then do
            let currentFSM = _bModesDealerHandFSM (_bModes bout)
                newFSM = SomeDealerHandFSM (DHResolvedFSM DHDealerBust)
                fsmDelta = ModesDelta history (DBoutSetDealerHandFSM newFSM currentFSM)
            pure $ deltas ++ [MutationOp Core.BoutWitness boutId fsmDelta]
        else pure deltas

createBoutPlayerHandUpdate :: BoutId -> SomeHand -> SomePlayerHandFSM -> CausalHistory -> [TraceOp]
createBoutPlayerHandUpdate boutId newHand newFSM history =
    let oldHand = characterize []
        oldFSM = SomePlayerHandFSM PHDecisionFSM
        attrDelta = AttrsDelta history (DBoutSetPlayerHand newHand oldHand)
        modeDelta = ModesDelta history (DBoutSetPlayerHandFSM newFSM oldFSM)
     in [ MutationOp Core.BoutWitness boutId attrDelta
        , MutationOp Core.BoutWitness boutId modeDelta
        ]

createBoutPlayerFSMUpdate :: BoutId -> SomePlayerHandFSM -> CausalHistory -> TraceOp
createBoutPlayerFSMUpdate boutId newFSM history =
    let oldFSM = SomePlayerHandFSM PHDecisionFSM
        delta = ModesDelta history (DBoutSetPlayerHandFSM newFSM oldFSM)
     in MutationOp Core.BoutWitness boutId delta

validateBoutPlayerAction :: BoutId -> Move -> ExceptT ValidationErrors (Reader TickCacheContext) ()
validateBoutPlayerAction boutId move = do
    bout <- ExceptT $ Dealer.validationToEither <$> derefV boutId
    let someHand = _bAttrsPlayerHand (_bAttrs bout)
        handFSM = _bModesPlayerHandFSM (_bModes bout)
        handIx = _bAttrsActiveHandIx (_bAttrs bout)
        bouts' = _bRelsPlayerBouts (_bRels bout)

    case lookupFiniteMap handIx bouts' of
        Just (Present activeBoutId) -> do
            activeBout <- ExceptT $ Dealer.validationToEither <$> derefV activeBoutId
            let tableId = _bRelsTable (_bRels activeBout)
            table <- ExceptT $ Dealer.validationToEither <$> derefV tableId
            let rules = gameRuleSet (_tAttrsOffering (_tAttrs table))

            case move of
                MSurrender -> case handFSM of
                    SomePlayerHandFSM PHDecisionFSM -> pure ()
                    SomePlayerHandFSM PHHittingFSM ->
                        throwError $ ValidationErrors [EntityNotFound "Cannot surrender after hitting"]
                    SomePlayerHandFSM (PHAwaitingOneCardFSM _) ->
                        throwError $ ValidationErrors [EntityNotFound "Cannot surrender after hitting"]
                    SomePlayerHandFSM (PHResolvedFSM _) ->
                        throwError $ ValidationErrors [EntityNotFound "Cannot surrender after hitting"]
                    SomePlayerHandFSM PHAwaitingFirstCardFSM ->
                        throwError $ ValidationErrors [EntityNotFound "Cannot surrender after hitting"]
                    SomePlayerHandFSM PHAwaitingSecondCardFSM ->
                        throwError $ ValidationErrors [EntityNotFound "Cannot surrender after hitting"]
                    SomePlayerHandFSM (PHAbandonedFSM _) ->
                        throwError $ ValidationErrors [EntityNotFound "Cannot surrender after hitting"]
                _ -> pure ()

            case Hand.validateMoveInContext move someHand rules 0 of
                Right _ -> pure ()
                Left err -> throwError $ ValidationErrors [EntityNotFound err]
        _ -> throwError $ ValidationErrors [EntityNotFound "No active bout found"]

generateBoutPlayerStandDeltas :: BoutId -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateBoutPlayerStandDeltas boutId history = do
    validateBoutPlayerAction boutId MStand
    pure [createBoutPlayerFSMUpdate boutId (SomePlayerHandFSM (PHResolvedFSM PHStand)) history]

generateBoutPlayerHitDeltas :: BoutId -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateBoutPlayerHitDeltas boutId history = do
    validateBoutPlayerAction boutId MHit
    pure [createBoutPlayerFSMUpdate boutId (SomePlayerHandFSM PHHittingFSM) history]

generateBoutPlayerDoubleDeltas :: BoutId -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateBoutPlayerDoubleDeltas boutId history = do
    validateBoutPlayerAction boutId MDouble
    pure [createBoutPlayerFSMUpdate boutId (SomePlayerHandFSM (PHAwaitingOneCardFSM OCDouble)) history]

generateBoutPlayerSplitDeltas :: BoutId -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateBoutPlayerSplitDeltas boutId history = do
    validateBoutPlayerAction boutId MSplit
    bout <- ExceptT $ Dealer.validationToEither <$> derefV boutId
    let currentHand = _bAttrsPlayerHand (_bAttrs bout)
        SomeHand cards _ = currentHand
        activeHandIx = _bAttrsActiveHandIx (_bAttrs bout)
        currentBouts = _bRelsPlayerBouts (_bRels bout)
    case cards of
        [card1, card2] -> do
            let hand1 = characterize [card1]

                handUpdate = AttrsDelta history (DBoutSetPlayerHand hand1 currentHand)

                currentBoutId = case lookupFiniteMap activeHandIx currentBouts of
                    Just (Present currentBid) -> currentBid
                    _ -> error "No bout found for active hand"

                nextHandIx = case activeHandIx of
                    Hand1 -> Hand2
                    Hand2 -> Hand3
                    Hand3 -> Hand4
                    Hand4 -> error "Cannot split beyond Hand4"

                boutEntryUpdate = RelsDelta history (DBoutSetPlayerBoutEntry nextHandIx (Present currentBoutId) Absent)

                isAces = rank card1 == Ace && rank card2 == Ace
                newFSM =
                    if isAces
                        then SomePlayerHandFSM (PHAwaitingOneCardFSM OCSplitAce)
                        else SomePlayerHandFSM PHAwaitingSecondCardFSM

                fsmUpdate = ModesDelta history (DBoutSetPlayerHandFSM newFSM (SomePlayerHandFSM PHDecisionFSM))

            pure
                [ MutationOp Core.BoutWitness boutId handUpdate
                , MutationOp Core.BoutWitness boutId boutEntryUpdate
                , MutationOp Core.BoutWitness boutId fsmUpdate
                ]
        _ -> throwError $ ValidationErrors [EntityNotFound "Cannot split: invalid hand structure"]

generateBoutPlayerSurrenderDeltas :: BoutId -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateBoutPlayerSurrenderDeltas boutId history = do
    validateBoutPlayerAction boutId MSurrender
    pure [createBoutPlayerFSMUpdate boutId (SomePlayerHandFSM (PHResolvedFSM PHSurrendered)) history]

generateBoutPlayerHandSetDeltas :: SomeHand -> BoutId -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateBoutPlayerHandSetDeltas newHand boutId history = do
    bout <- ExceptT $ Dealer.validationToEither <$> derefV boutId
    let currentHand = _bAttrsPlayerHand (_bAttrs bout)
        attrDelta = AttrsDelta history (DBoutSetPlayerHand newHand currentHand)
        SomeHand _ newWitness = newHand
        deltas = [MutationOp Core.BoutWitness boutId attrDelta]
    if isBust newWitness
        then do
            let currentFSM = _bModesPlayerHandFSM (_bModes bout)
                newFSM = SomePlayerHandFSM (PHResolvedFSM PHBust)
                fsmDelta = ModesDelta history (DBoutSetPlayerHandFSM newFSM currentFSM)
            pure $ deltas ++ [MutationOp Core.BoutWitness boutId fsmDelta]
        else pure deltas

generateBoutDealerHandSetDeltas :: SomeHand -> BoutId -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateBoutDealerHandSetDeltas newHand boutId history = do
    bout <- ExceptT $ Dealer.validationToEither <$> derefV boutId
    let currentHand = _bAttrsDealerHand (_bAttrs bout)
        attrDelta = AttrsDelta history (DBoutSetDealerHand newHand currentHand)
        SomeHand _ newWitness = newHand
        deltas = [MutationOp Core.BoutWitness boutId attrDelta]
    if isBust newWitness
        then do
            let currentFSM = _bModesDealerHandFSM (_bModes bout)
                newFSM = SomeDealerHandFSM (DHResolvedFSM DHDealerBust)
                fsmDelta = ModesDelta history (DBoutSetDealerHandFSM newFSM currentFSM)
            pure $ deltas ++ [MutationOp Core.BoutWitness boutId fsmDelta]
        else pure deltas
