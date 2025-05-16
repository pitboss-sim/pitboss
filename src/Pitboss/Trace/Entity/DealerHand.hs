{-# LANGUAGE GADTs #-}

module Pitboss.Trace.Entity.DealerHand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.FSM.DealerHandFSM
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.Identifier

mkDealerHand :: Meta DealerHandId -> SomeDealerHandFSM -> DealerHandState -> DealerHandRelations -> DealerHand
mkDealerHand = DealerHand

mkDealerHandState :: [Card] -> Chips -> Int -> Int -> DealerHandState
mkDealerHandState = DealerHandState

mkDealerHandRelations :: PlayerSpotId -> DealerRoundId -> DealerId -> DealerHandRelations
mkDealerHandRelations = DealerHandRelations

data DealerHand = DealerHand
  { _meta :: Meta DealerHandId,
    _fsm :: SomeDealerHandFSM,
    _state :: DealerHandState,
    _rels :: DealerHandRelations
  }
  deriving (Eq, Show, Generic)

data DealerHandState = DealerHandState
  { _handCards :: [Card],
    _originalBet :: Chips,
    _splitDepth :: Int,
    _handIx :: Int
  }
  deriving (Eq, Show, Generic)

data DealerHandRelations = DealerHandRelations
  { _belongsToPlayerSpot :: PlayerSpotId,
    _belongsToRound :: DealerRoundId,
    _ownedByDealer :: DealerId
  }
  deriving (Eq, Show, Generic)

instance ToJSON DealerHand

instance FromJSON DealerHand

instance ToJSON DealerHandState

instance FromJSON DealerHandState

instance ToJSON DealerHandRelations

instance FromJSON DealerHandRelations
