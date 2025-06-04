{-# LANGUAGE DataKinds #-}

module Pitboss.Simulation.Delta.Dealer where

import Control.Monad.Except
import Control.Monad.Reader
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.Causality.Types.Core qualified as Core
import Pitboss.FSM

generateBoutDealerRevealDeltas :: BoutId -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateBoutDealerRevealDeltas boutDealerId history = do
    _boutDealer <- ExceptT $ validationToEither <$> derefV boutDealerId
    pure [createBoutDealerHandFSMChange boutDealerId (SomeDealerHandFSM DHEvaluatingFSM) history]

generateBoutDealerHitDeltas :: BoutId -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateBoutDealerHitDeltas _dealerId _history = do
    pure []

generateBoutDealerStandDeltas :: BoutId -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateBoutDealerStandDeltas boutDealerId history = do
    _boutDealer <- ExceptT $ validationToEither <$> derefV boutDealerId
    pure [createBoutDealerHandFSMChange boutDealerId (SomeDealerHandFSM (DHResolvedFSM DHDealerStand)) history]

generateCardDrawnDeltas :: DealerId -> ShoeId -> Card -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateCardDrawnDeltas _dealerId _shoeId _card _history = do
    -- TODO: Implement card staging in dealer and shoe pointer advancement
    -- This requires adding DBoutDealerSetStagedCard and shoe delta constructors
    pure []

createBoutDealerHandUpdate :: BoutId -> SomeHand -> SomeDealerHandFSM -> CausalHistory -> [TraceOp]
createBoutDealerHandUpdate boutDealerId newHand newFSM history =
    let oldHand = characterize []
        oldFSM = SomeDealerHandFSM DHAwaitingFirstCardFSM
        attrDelta = AttrsDelta history (DBoutSetDealerHand newHand oldHand)
        modeDelta = ModesDelta history (DBoutSetDealerHandFSM newFSM oldFSM)
     in [ MutationOp Core.BoutWitness boutDealerId attrDelta
        , MutationOp Core.BoutWitness boutDealerId modeDelta
        ]

createBoutDealerHandFSMChange :: BoutId -> SomeDealerHandFSM -> CausalHistory -> TraceOp
createBoutDealerHandFSMChange boutDealerId newFSM history =
    let oldFSM = SomeDealerHandFSM DHAwaitingFirstCardFSM
        delta = ModesDelta history (DBoutSetDealerHandFSM newFSM oldFSM)
     in MutationOp Core.BoutWitness boutDealerId delta

validationToEither :: Validation e a -> Either e a
validationToEither (Success a) = Right a
validationToEither (Failure e) = Left e
