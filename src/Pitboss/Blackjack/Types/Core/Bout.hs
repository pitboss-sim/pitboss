{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Blackjack.Types.Core.Bout where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Blackjack.Types.Core.Decision
import Pitboss.Blackjack.Types.Core.Economy

data HandResolution = HODealer DealerHandResolution | HOBoutPlayer PlayerHandResolution
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DealerHandResolution
    = DHDealerBlackjack
    | DHDealerStand
    | DHDealerBust
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data PlayerHandResolution
    = PHSurrendered
    | PHBlackjack
    | PHStand
    | PHBust
    | PHPush
    | PHSplitNonAces
    | PHSplitAces
    | PHDealerBlackjack
    | PHVoid BankrollImpact
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data AbandonedReason
    = CSurrender Surrender
    | CInsurance InsuranceOutcome
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data OneCardDrawReason = OCDouble | OCSplitAce | OCSplitNonAce
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
