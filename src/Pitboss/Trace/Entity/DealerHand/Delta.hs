{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.DealerHand.Delta where

import Data.Aeson
import GHC.Generics
import Pitboss.Blackjack.Card
import Pitboss.FSM.DealerHand
import Pitboss.Trace.Entity.Types.EntityId

data DDealerHandAttrs
    = DDealerHandPushCard Card [Card]
    | DDealerHandPopCard Card [Card]
    | DDealerHandSetCards [Card] [Card]
    deriving (Eq, Show, Generic)

data DDealerHandModes
    = DDealerHandSetFSM SomeDealerHandFSM SomeDealerHandFSM
    deriving (Eq, Show, Generic)

data DDealerHandRels
    = DDealerHandSetRound (ClockedRef EDealerRoundId) (ClockedRef EDealerRoundId)
    | DDealerHandSetDealer (ClockedRef EDealerId) (ClockedRef EDealerId)
    deriving (Eq, Show, Generic)

instance ToJSON DDealerHandAttrs
instance FromJSON DDealerHandAttrs

instance ToJSON DDealerHandModes
instance FromJSON DDealerHandModes

instance ToJSON DDealerHandRels
instance FromJSON DDealerHandRels
