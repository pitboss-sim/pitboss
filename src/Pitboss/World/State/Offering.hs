module Pitboss.World.State.Offering where

import Pitboss.Blackjack.Offering
import Pitboss.Blackjack.Offering.Matter
import Pitboss.Blackjack.Offering.RuleSet
import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven

data OfferingState = OfferingState
  { offeringTick :: Tick,
    offeringMatter :: Matter,
    offeringRules :: RuleSet
  }
  deriving (Eq, Show)

data OfferingDelta
  = SetMatter Matter
  | SetRules RuleSet
  | ReplaceOffering Offering
  deriving (Eq, Show)

instance Clocked OfferingState where
  tick = offeringTick
  setTick t os = os {offeringTick = t}

instance DeltaDriven OfferingState OfferingDelta where
  applyDelta d os = case d of
    SetMatter m -> os {offeringMatter = m}
    SetRules r -> os {offeringRules = r}
    ReplaceOffering (Offering m r) -> os {offeringMatter = m, offeringRules = r}

  describeDelta :: OfferingDelta -> entity -> String
  describeDelta d _ = case d of
    SetMatter _ -> "Updated matter config"
    SetRules _ -> "Updated rule set"
    ReplaceOffering _ -> "Replaced full offering"

  previewDelta d os = Just (applyDelta d os)
