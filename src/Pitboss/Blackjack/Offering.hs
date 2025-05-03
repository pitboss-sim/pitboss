module Pitboss.Blackjack.Offering where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Offering.Matter (Matter)
import Pitboss.Blackjack.Offering.RuleSet (RuleSet)

data Offering = Offering
    { matter :: Matter
    , ruleSet :: RuleSet
    }
    deriving (Eq, Show, Generic)

instance ToJSON Offering

instance FromJSON Offering

mkOffering :: Matter -> RuleSet -> Offering
mkOffering m r = Offering{matter = m, ruleSet = r}
