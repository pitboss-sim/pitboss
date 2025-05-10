module Pitboss.Blackjack.Offering where

import Pitboss.Blackjack.Offering.Matter (Matter)
import Pitboss.Blackjack.Offering.RuleSet (RuleSet)

data Offering = Offering
  { matter :: Matter,
    ruleSet :: RuleSet
  }
  deriving (Eq, Show)

mkOffering :: Matter -> RuleSet -> Offering
mkOffering m r = Offering {matter = m, ruleSet = r}
