module Pitboss.Blackjack.Offering
  ( mkOffering,
    Offering (..),
    module Pitboss.Blackjack.Offering.Matter,
    module Pitboss.Blackjack.Offering.RuleSet,
  )
where

import Pitboss.Blackjack.Offering.Matter (Matter)
import Pitboss.Blackjack.Offering.RuleSet (RuleSet)

data Offering = Offering
  { matter :: Matter,
    ruleSet :: RuleSet
  }
  deriving (Show)

mkOffering :: Matter -> RuleSet -> Offering
mkOffering m r = Offering {matter = m, ruleSet = r}
