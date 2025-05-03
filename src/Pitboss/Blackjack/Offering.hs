module Pitboss.Blackjack.Offering
  ( mkOffering,
  )
where

import Pitboss.Blackjack.Offering.Matter (Matter)
import Pitboss.Blackjack.Offering.RuleSet (RuleSet)

data Offering = Offering
  { matter :: Matter,
    ruleset :: RuleSet
  }
  deriving (Show)

mkOffering :: Matter -> RuleSet -> Offering
mkOffering m r = Offering {matter = m, ruleset = r}
