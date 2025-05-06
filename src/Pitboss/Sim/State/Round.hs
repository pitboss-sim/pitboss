{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Sim.State.Round where

import Pitboss.Blackjack.FSM.Game.ENHC
import Pitboss.Blackjack.FSM.Game.Peek
import Pitboss.Blackjack.Hand (Hand)
import Pitboss.Blackjack.Table (TableSeatIx)
import Pitboss.Sim.Types.FiniteMap (FiniteMap)
import Pitboss.Sim.Types.Occupancy (Occupancy)
import Pitboss.Sim.World.Identifier (ShoeId, SpotId, TableId)

data SomeRoundState
  = PeekRound (RoundState (PeekFSM 'PeekAwaiting))
  | ENHCRound (RoundState (ENHCFSM 'ENHCAwaiting))

instance Eq SomeRoundState where
  PeekRound r1 == PeekRound r2 = r1 == r2
  ENHCRound r1 == ENHCRound r2 = r1 == r2
  _ == _ = False

instance Show SomeRoundState where
  show (PeekRound r) = "PeekRound " ++ show r
  show (ENHCRound r) = "ENHCRound " ++ show r

data RoundState fsm = RoundState
  { roundGameFSM :: fsm,
    roundDealerHand :: Occupancy Hand,

    roundTableId :: TableId, -- access to spots
    roundShoeId :: ShoeId,

    playerSpots :: FiniteMap TableSeatIx (Occupancy SpotId)
    -- events, possibly
  }
  deriving (Eq, Show)

mapRoundState :: (forall fsm. RoundState fsm -> a) -> SomeRoundState -> a
mapRoundState f = \case
  PeekRound r -> f r
  ENHCRound r -> f r

withRoundFSM :: (forall fsm. fsm -> a) -> SomeRoundState -> a
withRoundFSM f = mapRoundState (f . roundGameFSM)
