module Pitboss.Trace.Entity.Offering where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Offering.Matter
import Pitboss.Blackjack.Offering.RuleSet
import Pitboss.Trace.Delta.Types.Clocked

mkOfferingState :: Tick -> Matter -> RuleSet -> OfferingState
mkOfferingState t m r =
  OfferingState
    { _tick = t,
      _offeringMatter = m,
      _offeringRules = r
    }

data OfferingState = OfferingState
  { _tick :: Tick,
    _offeringMatter :: Matter,
    _offeringRules :: RuleSet
  }
  deriving (Eq, Show, Generic)

instance ToJSON OfferingState

instance FromJSON OfferingState

instance Clocked OfferingState where
  tick = _tick
  setTick t os = os {_tick = t}
