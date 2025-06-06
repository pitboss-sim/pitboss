{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Simulation.Agents.Dealer where

import Pitboss.Blackjack
import Pitboss.Simulation.Agents.Types

data DealerRequiredAction
    = MustHit
    | MustStand
    | MustDeal
    | MustSettle

determineRequiredDealerAction :: BoutContext -> SomeHand -> DealerRequiredAction
determineRequiredDealerAction ctx dealerHand =
    let rules = gameRuleSet (_contextOffering ctx)
     in if dealerShouldHit rules dealerHand
            then MustHit
            else MustStand

applyDealerArchetypeVariation :: SomeDealerArchetype -> DealerRequiredAction -> IO ()
applyDealerArchetypeVariation = \case
    SomeDealerByTheBook _ -> \_ -> pure ()
    SomeDealerRookie (RookieDealerArchetype config _) -> \case
        MustDeal -> applyRookieErrors (rkErrors config)
        _ -> pure ()
    SomeDealerVeteran (VeteranDealerArchetype config _) -> \case
        MustHit -> possiblyGiveTell (vtTellProfile config)
        _ -> pure ()

applyRookieErrors :: ErrorProfile -> IO ()
applyRookieErrors _ = pure ()

possiblyGiveTell :: TellProfile -> IO ()
possiblyGiveTell _ = pure ()
