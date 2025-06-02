{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.DealerHand.Phase where

import Data.Aeson.Types
import GHC.Generics
import Pitboss.FSM.Types

data DealerHandResolution
    = DHDealerBlackjack
    | DHDealerStand
    | DHDealerBust
    deriving (Eq, Show, Generic)

data DealerHandPhase
    = DHDealing
    | DHEvaluating
    | DHResolved DealerHandResolution
    | DHInterrupted InterruptReason
    deriving (Eq, Show, Generic)

instance ToJSON DealerHandResolution
instance FromJSON DealerHandResolution
instance ToJSON DealerHandPhase
instance FromJSON DealerHandPhase
