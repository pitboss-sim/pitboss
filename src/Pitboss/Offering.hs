module Pitboss.Offering
  ( mkOffering,
  )
where

import Pitboss.Offering.Matter (Matter)
import Pitboss.Offering.RuleSet (RuleSet)

data Offering = Offering
  { matter :: Matter,
    ruleset :: RuleSet
  }
  deriving (Show)

mkOffering :: Matter -> RuleSet -> Offering
mkOffering m r = Offering {matter = m, ruleset = r}
