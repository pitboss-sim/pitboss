{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.DealerHand.Phase where

import Data.Aeson.Types
import GHC.Generics
import Pitboss.FSM.DealerRound.Phase

data DealerHandResolution
    = DealerBlackjack
    | DealerStand
    | DealerBust
    deriving (Eq, Show, Generic)

data DealerHandPhase
    = Dealing
    | Evaluating
    | Resolved DealerHandResolution
    | Interrupted InterruptReason
    deriving (Eq, Show, Generic)

instance ToJSON DealerHandResolution

instance FromJSON DealerHandResolution

instance ToJSON DealerHandPhase

instance FromJSON DealerHandPhase
