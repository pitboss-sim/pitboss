module Pitboss.World.State.Offering where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Offering
import Pitboss.Blackjack.Offering.Matter
import Pitboss.Blackjack.Offering.RuleSet
import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven
import Pitboss.World.State.Types.Snapshot (StateSnapshot (..), defaultSnapshot)

data OfferingState = OfferingState
  { offeringTick :: Tick,
    offeringMatter :: Matter,
    offeringRules :: RuleSet
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

defaultOfferingState :: Tick -> Matter -> RuleSet -> OfferingState
defaultOfferingState t m r =
  OfferingState
    { offeringTick = t,
      offeringMatter = m,
      offeringRules = r
    }

defaultOfferingSnapshot :: Tick -> Matter -> RuleSet -> StateSnapshot OfferingState OfferingDelta
defaultOfferingSnapshot t m r =
  defaultSnapshot (defaultOfferingState t m r)
