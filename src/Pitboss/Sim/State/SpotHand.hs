{-# LANGUAGE GADTs #-}

module Pitboss.Sim.State.SpotHand where

import Pitboss.Blackjack.FSM.Hand
import Pitboss.Blackjack.Hand (Hand)
import Pitboss.Sim.World.Identifier (SpotId)

data SpotHandState = SpotHandState
  { spotHandFSM :: SomeHandFSM,
    hand :: Hand,
    spotId :: SpotId
  }

mkSpotHandState :: Hand -> SpotId -> SpotHandState
mkSpotHandState = SpotHandState (SomeHandFSM PreDealFSM)

instance Eq SpotHandState where
  SpotHandState fsm1 h1 aid1 == SpotHandState fsm2 h2 aid2 =
    handPhaseOf fsm1 == handPhaseOf fsm2 && h1 == h2 && aid1 == aid2

instance Show SpotHandState where
  show (SpotHandState fsm hand sid) =
    "SpotHandState { spotId = "
      ++ show sid
      ++ ", phase = "
      ++ show (handPhaseOf fsm)
      ++ ", hand = "
      ++ show hand
      ++ " }"

handPhaseOf :: SomeHandFSM -> HandPhase
handPhaseOf (SomeHandFSM fsm) = case fsm of
  PreDealFSM -> PreDeal
  Dealt1FSM -> Dealt1
  Dealt2FSM -> Dealt2
  AwaitingDecisionFSM -> AwaitingDecision
  HittingFSM -> Hitting
  DoublingFSM -> Doubling
  SplitAcesAutoFSM -> SplitAcesAuto
  CompletedFSM _ -> Completed
