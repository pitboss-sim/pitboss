module Pitboss.FSM.DealerHandFSM.Transitions where

import Pitboss.Blackjack.Hand
import Pitboss.Blackjack.Hand.Score qualified as HS
import Pitboss.Blackjack.Offering.RuleSet
import Pitboss.FSM.DealerHandFSM.Types

dealerShouldHit :: RuleSet -> Hand -> Bool
dealerShouldHit ruleset hand = case HS.dealerHandTotal hand of
  Nothing -> False -- busted
  Just HS.DealerBlackjack -> False
  Just (HS.Total n soft) -> n < 17 || (n == 17 && soft && isH17 ruleset)

resolveDealerHand :: Hand -> DealerHandResolution
resolveDealerHand hand = case HS.dealerHandTotal hand of
  Nothing -> DealerBust
  Just HS.DealerBlackjack -> DealerBlackjack
  Just _ -> DealerStand
