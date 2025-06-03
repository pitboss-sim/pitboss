module Pitboss.Blackjack.Offering (
    module Pitboss.Blackjack.Offering.Materia,
    module Pitboss.Blackjack.Offering.RuleSet,
    Offering (..),
    mkOffering,
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Offering.Materia
import Pitboss.Blackjack.Offering.RuleSet

data Offering = Offering
    { materia :: Materia
    , gameRuleSet :: GameRuleSet
    , tableRuleSet :: TableRuleSet
    }
    deriving (Eq, Show, Generic)

instance ToJSON Offering
instance FromJSON Offering

mkOffering :: Materia -> GameRuleSet -> TableRuleSet -> Offering
mkOffering m g t = Offering{materia = m, gameRuleSet = g, tableRuleSet = t}
