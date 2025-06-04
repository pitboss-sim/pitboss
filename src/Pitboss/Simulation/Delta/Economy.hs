{-# LANGUAGE DataKinds #-}

module Pitboss.Simulation.Delta.Economy where

import Control.Monad.Except
import Control.Monad.Reader
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.Simulation.Event

generateBankrollCreditDeltas :: PlayerId -> Chips -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateBankrollCreditDeltas playerId chips history = do
    player <- ExceptT $ validationToEither <$> derefV playerId
    let oldBankroll = _pAttrsBankroll (_pAttrs player)
        newBankroll = oldBankroll + chips
        delta = AttrsDelta history (DPlayerSetBankroll newBankroll oldBankroll)
    pure [mutate PlayerWitness playerId delta]

generateBankrollDebitDeltas :: PlayerId -> Chips -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateBankrollDebitDeltas playerId chips history = do
    player <- ExceptT $ validationToEither <$> derefV playerId
    let oldBankroll = _pAttrsBankroll (_pAttrs player)
        newBankroll = oldBankroll - chips
        delta = AttrsDelta history (DPlayerSetBankroll newBankroll oldBankroll)
    pure [mutate PlayerWitness playerId delta]

generateInsuranceSettledDeltas :: [PlayerId] -> CausalHistory -> ExceptT ValidationErrors (Reader TickCacheContext) [TraceOp]
generateInsuranceSettledDeltas _ _ = do
    -- TODO: Implement insurance settlement logic
    -- This should handle insurance payouts based on dealer blackjack
    -- For now, return empty list as placeholder
    pure []

validationToEither :: Validation e a -> Either e a
validationToEither (Success a) = Right a
validationToEither (Failure e) = Left e
