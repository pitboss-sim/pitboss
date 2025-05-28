module Pitboss.Blackjack.Offering (
    module Pitboss.Blackjack.Offering.Matter,
    module Pitboss.Blackjack.Offering.RuleSet,
    Offering (..),
    mkOffering,
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Offering.Matter
import Pitboss.Blackjack.Offering.RuleSet

data Offering = Offering
    { matter :: Matter
    , ruleSet :: RuleSet
    }
    deriving (Eq, Show, Generic)

instance ToJSON Offering
instance FromJSON Offering

mkOffering :: Matter -> RuleSet -> Offering
mkOffering m r = Offering{matter = m, ruleSet = r}
