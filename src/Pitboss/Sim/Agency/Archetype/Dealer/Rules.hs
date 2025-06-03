{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Sim.Agency.Archetype.Dealer.Rules where

import Pitboss.Blackjack
import Pitboss.Blackjack.Materia.Instances.Witnessable
import Pitboss.Sim.Agency.Archetype.Types
import Pitboss.Sim.Agency.Types

data DealerRequiredAction
    = MustHit
    | MustStand
    | MustDeal
    | MustSettle

determineRequiredDealerAction :: GameContext -> SomeHand -> DealerRequiredAction
determineRequiredDealerAction ctx dealerHand =
    let rules = gameRuleSet (_contextOffering ctx)
     in if dealerShouldHit rules dealerHand
            then MustHit
            else MustStand

dealerShouldHit :: GameRuleSet -> SomeHand -> Bool
dealerShouldHit ruleset (SomeHand hand) = case witness hand of
    BlackjackWitness -> False
    TwentyOneWitness -> False
    BustWitness -> False
    HardWitness -> handScore (SomeHand hand) < 17
    SoftWitness ->
        let score = handScore (SomeHand hand)
         in score < 17 || (score == 17 && isH17 ruleset)
    PairWitness -> handScore (SomeHand hand) < 17

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
