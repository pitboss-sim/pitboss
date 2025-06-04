{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Simulation.Intent.Validate where

import Pitboss.Blackjack
import Pitboss.Blackjack.Rules.Hand qualified as Hand
import Pitboss.Causality
import Pitboss.Causality.Validate
import Pitboss.Simulation.Intent.Types
import Pitboss.Simulation.Intent.ValidationContext

class Validateable (k :: IntentKind) where
    validate :: IntentCtx k -> ValidatedReader Bool

instance Validateable 'IPlayerHit where
    validate ctx = do
        result <- validateIntent ctx
        case result of
            Success _ -> pure (Success True)
            Failure errors -> pure (Failure errors)

instance Validateable 'IPlayerStand where
    validate ctx = do
        result <- validateIntent ctx
        case result of
            Success _ -> pure (Success True)
            Failure errors -> pure (Failure errors)

instance Validateable 'IPlayerDouble where
    validate ctx = do
        result <- validateIntent ctx
        case result of
            Success _ -> pure (Success True)
            Failure errors -> pure (Failure errors)

instance Validateable 'IPlayerSplit where
    validate ctx = do
        result <- validateIntent ctx
        case result of
            Success (_, _, _, _, hand, bankroll, currentSplitCount) -> do
                let rules = gameRuleSet vegas6
                case Hand.validateSplitEligibility hand rules currentSplitCount of
                    Right _ ->
                        if bankroll >= minBetAmount
                            then pure (Success True)
                            else pure (Failure (ValidationErrors [EntityNotFound "Insufficient bankroll for split bet"]))
                    Left err -> pure (Failure (ValidationErrors [EntityNotFound err]))
            Failure errors -> pure (Failure errors)
      where
        minBetAmount = Chips 25

instance Validateable 'IPlayerSurrender where
    validate _ = pure (Failure (ValidationErrors [EntityNotFound "Player surrender not yet implemented"]))

instance Validateable 'IDealerHit where
    validate _ = pure (Success True)

instance Validateable 'IDealerStand where
    validate _ = pure (Success True)
