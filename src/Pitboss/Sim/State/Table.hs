{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Pitboss.Sim.State.Table where

import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.FSM.Game.ENHC (ENHCFSM (..), ENHCPhase (..))
import Pitboss.Blackjack.FSM.Game.Peek (PeekFSM (..), PeekPhase (..))
import Pitboss.Sim.State.Spot (SpotState, emptySpot)
import Pitboss.Sim.Types.FiniteMap (FiniteMap, emptyFiniteMap)
import Pitboss.Sim.Types.Occupancy (Occupancy (..))
import Pitboss.Types.BoundedEnum (BoundedEnum)

data TableSpotIx = Spot1 | Spot2 | Spot3 | Spot4
  deriving (Eq, Ord, Enum, Bounded, Show)

instance BoundedEnum TableSpotIx

data ShoeState
  = ShoeKnown Int
  | ShoeUnknown
  deriving (Eq, Show)

data TableState fsm = TableState
  { gameFSM :: fsm,
    deck :: [Card],
    shoeState :: ShoeState,
    playerSpots :: FiniteMap TableSpotIx (Occupancy SpotState),
    dealerSpot :: SpotState
  }
  deriving (Eq, Show)

data SomeTableState where
  SomeTableState :: TableState fsm -> SomeTableState

emptyTableState :: fsm -> TableState fsm
emptyTableState fsm =
  TableState
    { gameFSM = fsm,
      deck = [],
      shoeState = ShoeUnknown,
      playerSpots = emptyFiniteMap Absent,
      dealerSpot = emptySpot
    }

initPeekTableState :: TableState (PeekFSM 'PeekAwaiting)
initPeekTableState = emptyTableState PeekAwaitingFSM

initENHCTableState :: TableState (ENHCFSM 'ENHCAwaiting)
initENHCTableState = emptyTableState ENHCAwaitingFSM
