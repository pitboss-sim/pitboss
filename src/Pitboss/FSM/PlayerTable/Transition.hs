{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.PlayerTable.Transition where

import Pitboss.FSM.PlayerTable.FSM
import Pitboss.FSM.PlayerTable.Phase

beginChoosingTable :: PlayerTableFSM 'Idle -> PlayerTableFSM 'ChoosingTable
beginChoosingTable IdleFSM = ChoosingTableFSM

confirmTableChoice :: PlayerTableFSM 'ChoosingTable -> PlayerTableFSM 'PlacingBet
confirmTableChoice ChoosingTableFSM = PlacingBetFSM

placeBet :: PlayerTableFSM 'PlacingBet -> PlayerTableFSM 'PlayingHand
placeBet PlacingBetFSM = PlayingHandFSM

startObservation :: PlayerTableFSM 'PlayingHand -> PlayerTableFSM 'Observing
startObservation PlayingHandFSM = ObservingFSM

completeSession :: PlayerTableFSM 'Observing -> PlayerTableFSM 'Done
completeSession ObservingFSM = DoneFSM
