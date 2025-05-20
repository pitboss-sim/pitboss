{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.DealerHand.Delta where

import Data.Aeson
import GHC.Generics
import Pitboss.Blackjack.Card
import Pitboss.FSM.DealerHand
import Pitboss.Trace.Entity.Types
import Pitboss.Trace.Entity.Types.EntityId

data DealerHandEntityAttrsDelta
    = AddCard Card
    | RemoveCard Card
    | ReplaceCards [Card] [Card] -- old, new
    deriving (Eq, Show, Generic)

instance ToJSON DealerHandEntityAttrsDelta
instance FromJSON DealerHandEntityAttrsDelta

data DealerHandEntityModesDelta
    = ReplaceFSM SomeDealerHandFSM SomeDealerHandFSM
    deriving (Eq, Show, Generic)

instance ToJSON DealerHandEntityModesDelta
instance FromJSON DealerHandEntityModesDelta

data DealerHandEntityRelsDelta
    = UpdatePlayerSpot (ClockedRef PlayerSpotEntityId) (ClockedRef PlayerSpotEntityId)
    | UpdateDealerRound (ClockedRef DealerRoundEntityId) (ClockedRef DealerRoundEntityId)
    | UpdateDealer (ClockedRef DealerEntityId) (ClockedRef DealerEntityId)
    deriving (Eq, Show, Generic)

instance ToJSON DealerHandEntityRelsDelta
instance FromJSON DealerHandEntityRelsDelta
