{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.DealerHand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.FSM.DealerHand
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkDealerHand :: Meta (EntityRef DealerHandId) -> SomeDealerHandFSM -> DealerHandState -> DealerHandRelations -> DealerHand
mkDealerHand = DealerHand

mkDealerHandState :: [Card] -> DealerHandState
mkDealerHandState = DealerHandState

mkDealerHandRelations :: EntityRef PlayerSpotId -> EntityRef DealerRoundId -> EntityRef DealerId -> DealerHandRelations
mkDealerHandRelations = DealerHandRelations

data DealerHand = DealerHand
  { _dealerHandMeta :: Meta (EntityRef DealerHandId),
    _dealerHandFsm :: SomeDealerHandFSM,
    _dealerHandState :: DealerHandState,
    _dealerHandRels :: DealerHandRelations
  }
  deriving (Eq, Show, Generic)

data DealerHandState = DealerHandState
  { _dealerHandStateHandCards :: [Card]
  }
  deriving (Eq, Show, Generic)

data DealerHandRelations = DealerHandRelations
  { _dealerHandRelsBelongsToPlayerSpot :: EntityRef PlayerSpotId,
    _dealerHandRelsBelongsToRound :: EntityRef DealerRoundId,
    _dealerHandRelsOwnedByDealer :: EntityRef DealerId
  }
  deriving (Eq, Show, Generic)

instance ToJSON DealerHand

instance FromJSON DealerHand

instance ToJSON DealerHandState

instance FromJSON DealerHandState

instance ToJSON DealerHandRelations

instance FromJSON DealerHandRelations
