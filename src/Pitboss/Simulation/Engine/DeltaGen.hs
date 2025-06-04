{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Simulation.Engine.DeltaGen where

import Control.Monad.Except
import Control.Monad.Reader
import Pitboss.Blackjack hiding (Hand, HandWitness)
import Pitboss.Blackjack.Rules.Hand qualified as Hand
import Pitboss.Causality
import Pitboss.Causality.Validate
import Pitboss.FSM
import Pitboss.Simulation.Engine.Runtime
import Pitboss.Simulation.Event
import Pitboss.Simulation.Intent.Generate
import System.Random (StdGen, mkStdGen, randomR)

determineSplitFSMStates :: SomeHand -> (SomeContestantHandFSM, SomeContestantHandFSM)
determineSplitFSMStates hand =
    if isPairOfAces hand
        then
            ( SomeContestantHandFSM (CHAwaitingOneCardFSM OCSplitAce)
            , SomeContestantHandFSM (CHAwaitingOneCardFSM OCSplitAce)
            )
        else
            ( SomeContestantHandFSM CHAwaitingSecondCardFSM
            , SomeContestantHandFSM CHAwaitingSecondCardFSM
            )

validateSplitEntitiesWithBout :: ContestantId -> HandId -> Reader TickCacheContext (Validation ValidationErrors (EntityState 'Hand, EntityState 'Contestant, EntityState 'Bout))
validateSplitEntitiesWithBout contestantId handId = do
    handV <- derefV handId
    contestantV <- derefV contestantId

    case handV of
        Success hand -> do
            let boutId = _hRelsBout (_hRels hand)
            boutV <- derefV boutId
            pure $ validateSplitComplete contestantId <$> Success hand <*> contestantV <*> boutV
        Failure errors ->
            pure $ Failure errors
  where
    validateSplitComplete cid hand contestant bout = case _hRelsOwner (_hRels hand) of
        ContestantOwner ownerId | ownerId == cid -> (hand, contestant, bout)
        ContestantOwner ownerId -> error $ "Hand owned by different contestant: " ++ show ownerId
        DealerOwner _ -> error "Hand owned by dealer, not contestant"

executeSplit ::
    ContestantId ->
    HandId ->
    BoutId ->
    EntityState 'Hand ->
    EntityState 'Contestant ->
    EntityState 'Bout ->
    CausalHistory ->
    StdGen ->
    Either String ([TraceOp], StdGen)
executeSplit contestantId originalHandId _originalBoutId originalHand contestant originalBout history gen = do
    let originalSomeHand = _hAttrsHand (_hAttrs originalHand)
        currentSplitCount = countExistingSplits contestant
        offering = vegas6
        rules = gameRuleSet offering

    Hand.validateSplitEligibility originalSomeHand rules currentSplitCount

    (firstHand, secondHand) <- Hand.splitIntoHands originalSomeHand

    let (newHandId, gen1) = generateNewHandId gen
        (newBoutId, gen2) = generateNewBoutId gen1
        nextHandIx = findNextAvailableHandIx contestant
        (originalFSM, newFSM) = determineSplitFSMStates originalSomeHand

    case nextHandIx of
        Nothing -> Left "Cannot split: no available hand slots"
        Just handIx -> do
            let allOps =
                    concat
                        [ createSplitHandEntity newHandId secondHand contestant originalBout newFSM history
                        , createSplitBoutEntity newBoutId newHandId originalBout history
                        , updateOriginalHandForSplit originalHandId firstHand originalFSM history
                        , updateContestantForSplit contestantId handIx newBoutId history
                        ]
            Right (allOps, gen2)

countExistingSplits :: EntityState 'Contestant -> Int
countExistingSplits contestant =
    let bouts' = _cRelsBouts (_cRels contestant)
        allHandIxs = [Hand1, Hand2, Hand3, Hand4]
        occupiedCount = length [ix | ix <- allHandIxs, case lookupFiniteMap ix bouts' of Just (Present _) -> True; _ -> False]
     in max 0 (occupiedCount - 1)

findNextAvailableHandIx :: EntityState 'Contestant -> Maybe HandIx
findNextAvailableHandIx contestant =
    let bouts' = _cRelsBouts (_cRels contestant)
        allHandIxs = [Hand1, Hand2, Hand3, Hand4]
     in case filter (\ix -> lookupFiniteMap ix bouts' == Just Absent) allHandIxs of
            (ix : _) -> Just ix
            [] -> Nothing

createSplitHandEntity :: HandId -> SomeHand -> EntityState 'Contestant -> EntityState 'Bout -> SomeContestantHandFSM -> CausalHistory -> [TraceOp]
createSplitHandEntity newHandId newHand _contestant originalBout fsm _history =
    let contestantId = ContestantOwner (ContestantId 0)
        shoeId = _bRelsShoe (_bRels originalBout)
        tempBoutId = BoutId 0

        handAttrs = HandAttrs newHand
        handModes = HandModes (ContestantHandFSM fsm)
        handRels = HandRels contestantId tempBoutId shoeId
        newHandEntity = EHand handAttrs handModes handRels
     in [createBirth newHandId newHandEntity]

createSplitBoutEntity :: BoutId -> HandId -> EntityState 'Bout -> CausalHistory -> [TraceOp]
createSplitBoutEntity newBoutId newHandId originalBout _history =
    let dealerHandId = _bRelsDealerHand (_bRels originalBout)
        roundId = _bRelsRound (_bRels originalBout)
        shoeId = _bRelsShoe (_bRels originalBout)
        tableId = _bRelsTable (_bRels originalBout)

        boutAttrs = BoutAttrs Absent
        boutModes = BoutModes (SomeBoutFSM BAwaitingSecondCardFSM)
        boutRels = BoutRels newHandId dealerHandId roundId shoeId tableId
        newBoutEntity = EBout boutAttrs boutModes boutRels
     in [createBirth newBoutId newBoutEntity]

updateOriginalHandForSplit :: HandId -> SomeHand -> SomeContestantHandFSM -> CausalHistory -> [TraceOp]
updateOriginalHandForSplit handId newHand newFSM history =
    let oldHand = characterize []
        oldFSM = ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM)

        attrDelta = AttrsDelta history (DHandSetHand newHand oldHand)
        modeDelta = ModesDelta history (DHandSetHandFSM (ContestantHandFSM newFSM) oldFSM)

        attrOp = MutationOp HandWitness handId attrDelta
        modeOp = MutationOp HandWitness handId modeDelta
     in [attrOp, modeOp]

updateContestantForSplit :: ContestantId -> HandIx -> BoutId -> CausalHistory -> [TraceOp]
updateContestantForSplit contestantId handIx newBoutId history =
    let delta = RelsDelta history (DContestantInsertBoutEntry handIx (Present newBoutId))
        mutationOp = MutationOp ContestantWitness contestantId delta
     in [mutationOp]

generateNewHandId :: StdGen -> (HandId, StdGen)
generateNewHandId gen =
    let (w, gen') = randomR (0, maxBound) gen
     in (HandId w, gen')

generateNewBoutId :: StdGen -> (BoutId, StdGen)
generateNewBoutId gen =
    let (w, gen') = randomR (0, maxBound) gen
     in (BoutId w, gen')

generateDeltas :: BlackjackEvent -> CausalHistory -> Reader TickCacheContext (Either ValidationErrors [TraceOp])
generateDeltas event history = runExceptT $ do
    case event of
        ContestantStood _playerId handId ->
            withContestantHand handId $ \hand -> do
                validateHandAction hand MStand
                pure [createHandModeChange handId hand (ContestantHandFSM (SomeContestantHandFSM (CHResolvedFSM CStand))) history]
        ContestantHit _playerId handId ->
            withContestantHand handId $ \hand -> do
                validateHandAction hand MHit
                let oldFSM = _hModesHandFSM (_hModes hand)
                    newFSM = case oldFSM of
                        ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM) ->
                            ContestantHandFSM (SomeContestantHandFSM CHHittingFSM)
                        other -> other
                pure [createHandModeChange handId hand newFSM history]
        CardDealt card handId ->
            withHand handId $ \hand -> do
                let oldHand = _hAttrsHand (_hAttrs hand)
                    oldCards = handCards oldHand
                    newCards = oldCards ++ [card]
                    newHand = characterize newCards
                    attrDelta = AttrsDelta history (DHandSetHand newHand oldHand)
                    attrMutation = createMutation handId hand attrDelta

                ownerOps <- case _hRelsOwner (_hRels hand) of
                    ContestantOwner _ -> generateContestantCardDealtOps handId hand newHand newCards history
                    DealerOwner _ -> generateDealerCardDealtOps handId hand newHand history

                pure $ attrMutation : ownerOps
        ContestantSplit contestantId handId -> do
            hand <- ExceptT $ validationToEither <$> derefV handId
            contestant <- ExceptT $ validationToEither <$> derefV contestantId
            case _hRelsOwner (_hRels hand) of
                ContestantOwner cid | cid == contestantId -> do
                    let boutId = _hRelsBout (_hRels hand)
                    bout <- ExceptT $ validationToEither <$> derefV boutId
                    validateHandAction hand MSplit
                    context <- ask
                    let currentTick = _cacheTick (_ctxTickCache context)
                        gen = mkStdGen (fromIntegral (unTick currentTick))
                    case executeSplit contestantId handId boutId hand contestant bout history gen of
                        Right (ops, _) -> pure ops
                        Left err -> throwError $ ValidationErrors [EntityNotFound err]
                _ -> pure []
        BoutSettled boutId detailedOutcome ->
            withBout boutId $ \bout -> do
                let oldOutcome = _bAttrsOutcome (_bAttrs bout)
                    newOutcome = Present detailedOutcome
                    delta = AttrsDelta history (DBoutSetOutcome newOutcome oldOutcome)
                pure [createMutation boutId bout delta]
        DealerRevealed _dealerId handId ->
            withDealerHand handId $ \hand ->
                pure [createHandModeChange handId hand (DealerHandFSM (SomeDealerHandFSM DHEvaluatingFSM)) history]
        ContestantDoubledDown _playerId handId ->
            withContestantHand handId $ \hand -> do
                validateHandAction hand MDouble
                pure [createHandModeChange handId hand (ContestantHandFSM (SomeContestantHandFSM (CHAwaitingOneCardFSM OCDouble))) history]
        ContestantSurrendered _playerId handId ->
            withContestantHand handId $ \hand -> do
                validateHandAction hand MSurrender
                pure [createHandModeChange handId hand (ContestantHandFSM (SomeContestantHandFSM (CHResolvedFSM CSurrendered))) history]
        DealerHit _dealerId handId ->
            withHand handId $ \_ -> pure []
        DealerStood _dealerId handId ->
            withDealerHand handId $ \hand ->
                pure [createHandModeChange handId hand (DealerHandFSM (SomeDealerHandFSM (DHResolvedFSM DDealerStand))) history]

validateHandAction :: EntityState 'Hand -> Move -> ExceptT ValidationErrors (Reader TickCacheContext) ()
validateHandAction hand move = do
    let someHand = _hAttrsHand (_hAttrs hand)
        rules = gameRuleSet vegas6
    case validateMoveInContext move someHand rules 0 of
        Right () -> pure ()
        Left err -> throwError $ ValidationErrors [EntityNotFound err]

withContestantHand :: HandId -> (EntityState 'Hand -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]) -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
withContestantHand handId action = do
    hand <- ExceptT $ validationToEither <$> derefV handId
    case _hRelsOwner (_hRels hand) of
        ContestantOwner _ -> action hand
        DealerOwner _ -> pure []

withDealerHand :: HandId -> (EntityState 'Hand -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]) -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
withDealerHand handId action = do
    hand <- ExceptT $ validationToEither <$> derefV handId
    case _hRelsOwner (_hRels hand) of
        DealerOwner _ -> action hand
        ContestantOwner _ -> pure []

withHand :: HandId -> (EntityState 'Hand -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]) -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
withHand handId action = do
    hand <- ExceptT $ validationToEither <$> derefV handId
    action hand

withBout :: BoutId -> (EntityState 'Bout -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]) -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
withBout boutId action = do
    bout <- ExceptT $ validationToEither <$> derefV boutId
    action bout

createHandModeChange :: HandId -> EntityState 'Hand -> SomeHandFSM -> CausalHistory -> TraceOp
createHandModeChange handId hand newFSM history =
    let oldFSM = _hModesHandFSM (_hModes hand)
        delta = ModesDelta history (DHandSetHandFSM newFSM oldFSM)
     in createMutation handId hand delta

generateContestantCardDealtOps :: HandId -> EntityState 'Hand -> SomeHand -> [Card] -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateContestantCardDealtOps handId hand newHand newCards history = do
    let oldHandFSM = _hModesHandFSM (_hModes hand)
        witness = handWitness newHand
    case (oldHandFSM, valueType witness, length newCards) of
        (ContestantHandFSM (SomeContestantHandFSM CHAwaitingFirstCardFSM), _, 1) -> do
            let newFSM = ContestantHandFSM (SomeContestantHandFSM CHAwaitingSecondCardFSM)
            pure [createHandModeChange handId hand newFSM history]
        (ContestantHandFSM (SomeContestantHandFSM CHAwaitingSecondCardFSM), BlackjackWitness, 2) -> do
            let newFSM = ContestantHandFSM (SomeContestantHandFSM (CHResolvedFSM CBlackjack))
            pure [createHandModeChange handId hand newFSM history]
        (ContestantHandFSM (SomeContestantHandFSM CHAwaitingSecondCardFSM), _, 2) -> do
            let newFSM = ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM)
            pure [createHandModeChange handId hand newFSM history]
        (ContestantHandFSM (SomeContestantHandFSM CHHittingFSM), BustWitness, _) -> do
            let newFSM = ContestantHandFSM (SomeContestantHandFSM (CHResolvedFSM CBust))
            pure [createHandModeChange handId hand newFSM history]
        (ContestantHandFSM (SomeContestantHandFSM (CHAwaitingOneCardFSM OCDouble)), valueWit, _) ->
            case valueWit of
                BlackjackWitness -> do
                    let newFSM = ContestantHandFSM (SomeContestantHandFSM (CHResolvedFSM CBlackjack))
                    pure [createHandModeChange handId hand newFSM history]
                BustWitness -> do
                    let newFSM = ContestantHandFSM (SomeContestantHandFSM (CHResolvedFSM CBust))
                    pure [createHandModeChange handId hand newFSM history]
                _ -> do
                    let newFSM = ContestantHandFSM (SomeContestantHandFSM (CHResolvedFSM CStand))
                    pure [createHandModeChange handId hand newFSM history]
        _ -> pure []

generateDealerCardDealtOps :: HandId -> EntityState 'Hand -> SomeHand -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateDealerCardDealtOps handId hand newHand history = do
    let oldFSM = _hModesHandFSM (_hModes hand)
        witness = handWitness newHand
    case (oldFSM, valueType witness) of
        (DealerHandFSM (SomeDealerHandFSM DHDealingFSM), BlackjackWitness) -> do
            let newFSM = DealerHandFSM (SomeDealerHandFSM (DHResolvedFSM DDealerBlackjack))
            pure [createHandModeChange handId hand newFSM history]
        (DealerHandFSM (SomeDealerHandFSM DHDealingFSM), BustWitness) -> do
            let newFSM = DealerHandFSM (SomeDealerHandFSM (DHResolvedFSM DDealerBust))
            pure [createHandModeChange handId hand newFSM history]
        _ -> pure []
