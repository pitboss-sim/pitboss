{-# LANGUAGE DataKinds #-}

module Pitboss.Simulation.Delta.Bout where

import Control.Monad.Except
import Control.Monad.Reader
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.Simulation.Event

generateBoutSettledDeltas :: BoutId -> DetailedOutcome -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateBoutSettledDeltas boutId detailedOutcome history = do
    bout <- ExceptT $ validationToEither <$> derefV boutId
    let oldOutcome = _bAttrsOutcome (_bAttrs bout)
        newOutcome = Present detailedOutcome
        delta = AttrsDelta history (DBoutSetOutcome newOutcome oldOutcome)
    pure [mutate BoutWitness boutId delta]

validationToEither :: Validation e a -> Either e a
validationToEither (Success a) = Right a
validationToEither (Failure e) = Left e
