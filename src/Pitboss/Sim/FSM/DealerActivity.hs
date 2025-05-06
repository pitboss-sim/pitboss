{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Sim.FSM.DealerActivity where

import GHC.Generics (Generic)

data DealerPhase
  = Idle -- Not assigned to any table
  | Pushing -- In transit / about to start a shift
  | Dealing -- Actively running a game
  | BetweenRounds -- Temporarily paused
  | BetweenRoundsShuffle -- Paused to shuffle
  | Relieved -- Just handed off table duties
  deriving (Eq, Show, Generic)

data DealerActivityFSM (p :: DealerPhase) where
  IdleFSM :: DealerActivityFSM 'Idle
  PushingFSM :: DealerActivityFSM 'Pushing
  DealingFSM :: DealerActivityFSM 'Dealing
  BetweenRoundsFSM :: DealerActivityFSM 'BetweenRounds
  BetweenRoundsShuffleFSM :: DealerActivityFSM 'BetweenRoundsShuffle
  RelievedFSM :: DealerActivityFSM 'Relieved

deriving instance Show (DealerActivityFSM p)

deriving instance Eq (DealerActivityFSM p)

pushTo :: DealerActivityFSM 'Idle -> DealerActivityFSM 'Pushing
pushTo IdleFSM = PushingFSM

beginDealing :: DealerActivityFSM 'Pushing -> DealerActivityFSM 'Dealing
beginDealing PushingFSM = DealingFSM

pauseBetweenRounds :: DealerActivityFSM 'Dealing -> DealerActivityFSM 'BetweenRounds
pauseBetweenRounds DealingFSM = BetweenRoundsFSM

beginShuffling :: DealerActivityFSM 'BetweenRounds -> DealerActivityFSM 'BetweenRoundsShuffle
beginShuffling BetweenRoundsFSM = BetweenRoundsShuffleFSM

endShuffling :: DealerActivityFSM 'BetweenRoundsShuffle -> DealerActivityFSM 'Dealing
endShuffling BetweenRoundsShuffleFSM = DealingFSM

relieveDealer :: DealerActivityFSM 'Dealing -> DealerActivityFSM 'Relieved
relieveDealer DealingFSM = RelievedFSM

goIdle :: DealerActivityFSM 'Relieved -> DealerActivityFSM 'Idle
goIdle RelievedFSM = IdleFSM
