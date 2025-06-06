module Pitboss.Blackjack.Types.Core.Bout where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Blackjack.Types.Core.Decision
import Pitboss.Blackjack.Types.Core.Economy

data HandResolution = HODealer DealerHandResolution | HOContestant ContestantHandResolution
    deriving (Eq, Show, Generic)

data DealerHandResolution
    = DDealerBlackjack
    | DDealerStand
    | DDealerBust
    deriving (Eq, Show, Generic)

data ContestantHandResolution
    = CSurrendered
    | CBlackjack
    | CStand
    | CBust
    | CPush
    | CSplitNonAces
    | CSplitAces
    | CDealerBlackjack
    | CVoid BankrollImpact
    deriving (Eq, Show, Generic)

data AbandonedReason
    = CSurrender Surrender
    | CInsurance InsuranceOutcome
    deriving (Eq, Show, Generic)

data OneCardDrawReason = OCDouble | OCSplitAce | OCSplitNonAce
    deriving (Eq, Show, Generic)

instance ToJSON HandResolution
instance FromJSON HandResolution
instance ToJSON DealerHandResolution
instance FromJSON DealerHandResolution
instance ToJSON ContestantHandResolution
instance FromJSON ContestantHandResolution
instance ToJSON AbandonedReason
instance FromJSON AbandonedReason
instance ToJSON OneCardDrawReason
instance FromJSON OneCardDrawReason
