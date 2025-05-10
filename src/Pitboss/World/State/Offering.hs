module Pitboss.World.State.Offering where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Offering
import Pitboss.Blackjack.Offering.Matter
import Pitboss.Blackjack.Offering.RuleSet
import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven

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

data OfferingDelta
  = SetMatter Matter
  | SetRules RuleSet
  | ReplaceOffering Offering
  deriving (Eq, Show, Generic)

instance ToJSON OfferingDelta

instance FromJSON OfferingDelta

instance Clocked OfferingState where
  tick = _tick
  setTick t os = os {_tick = t}

instance DeltaDriven OfferingState OfferingDelta where
  applyDelta d os = case d of
    SetMatter m -> os {_offeringMatter = m}
    SetRules r -> os {_offeringRules = r}
    ReplaceOffering (Offering m r) -> os {_offeringMatter = m, _offeringRules = r}

  describeDelta :: OfferingDelta -> entity -> String
  describeDelta d _ = case d of
    SetMatter _ -> "Updated matter config"
    SetRules _ -> "Updated rule set"
    ReplaceOffering _ -> "Replaced full offering"

  previewDelta d os = Just (applyDelta d os)
