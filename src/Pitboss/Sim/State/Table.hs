{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Pitboss.Sim.State.Table where

import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.FSM.Game.ENHC (ENHCFSM (..), ENHCPhase (..))
import Pitboss.Blackjack.FSM.Game.Peek (PeekFSM (..), PeekPhase (..))
import Pitboss.Sim.State.Shoe (ShoeState (..))
import Pitboss.Sim.State.Spot (SpotState)
import Pitboss.Sim.Types.FiniteMap (FiniteMap, emptyFiniteMap)
import Pitboss.Sim.Types.Occupancy (Occupancy (..))
import Pitboss.Sim.World.Identifier (ActorId, ShoeId)
import Pitboss.Types.BoundedEnum (BoundedEnum)

data TableSpotIx = Spot1 | Spot2 | Spot3 | Spot4 | Spot5 | Spot6
  deriving (Eq, Ord, Enum, Bounded, Show)

instance BoundedEnum TableSpotIx

data TableState = TableState
  { tableDealerId :: Maybe ActorId,
    tableShoeId :: Maybe ShoeId,
    tableRoundId :: Maybe RoundId,
    tableSpotIds :: FiniteMap TableSeatIx (Occupancy SpotState)
  }

instance (Eq fsm) => Eq (TableState fsm) where
  (TableState fsm1 deck1 _ spots1 tdid1 tsid1) == (TableState fsm2 deck2 _ spots2 tdid2 tsid2) =
    fsm1 == fsm2 && deck1 == deck2 && spots1 == spots2 && tdid1 == tdid2 && tsid1 == tsid2

instance (Show fsm) => Show (TableState fsm) where
  show (TableState fsm deck _ spots mDealerId shoeId) =
    "TableState { fsm = "
      ++ show fsm
      ++ ", deck = "
      ++ show deck
      ++ ", playerSpots = "
      ++ show spots
      ++ ", tableDealerId = "
      ++ show mDealerId
      ++ ", tableShoeId = "
      ++ show shoeId
      ++ " }"

data SomeTableState where
  SomeTableState :: (Eq fsm, Show fsm) => TableState fsm -> SomeTableState

instance Show SomeTableState where
  show (SomeTableState t) = "SomeTableState (" ++ show t ++ ")"

instance Eq SomeTableState where
  SomeTableState _ == SomeTableState _ = False -- infeasible

emptyTableState :: fsm -> TableState fsm
emptyTableState fsm =
  TableState
    { gameFSM = fsm,
      deck = [],
      shoeState = ShoeState [] [],
      playerSpots = emptyFiniteMap Absent,
      tableDealerId = Nothing,
      tableShoeId = Nothing
    }

initPeekTableState :: TableState (PeekFSM 'PeekAwaiting)
initPeekTableState = emptyTableState PeekAwaitingFSM

initENHCTableState :: TableState (ENHCFSM 'ENHCAwaiting)
initENHCTableState = emptyTableState ENHCAwaitingFSM
