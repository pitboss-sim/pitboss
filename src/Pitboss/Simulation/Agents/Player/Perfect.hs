{-# LANGUAGE DataKinds #-}

module Pitboss.Simulation.Agents.Player.Perfect where

import Control.Monad.State
import Pitboss.Blackjack
import Pitboss.Simulation.Agents.Types
import System.Random

getPerfectMove :: ArchetypeConfig 'Perfect -> BoutContext -> State StdGen Move
getPerfectMove config ctx =
    pure $
        if pcUseEV config
            then calculateEVMove ctx
            else calculateOptimalMove ctx

calculateEVMove :: BoutContext -> Move
calculateEVMove ctx =
    let hand = _contextBoutPlayerHand ctx
        rules = gameRuleSet (_contextOffering ctx)
        availMoves = availableMoves hand rules 0
        score = handScore hand
     in if score < 17 && MHit `elem` availMoves
            then MHit
            else MStand

calculateOptimalMove :: BoutContext -> Move
calculateOptimalMove ctx =
    let hand = _contextBoutPlayerHand ctx
        rules = gameRuleSet (_contextOffering ctx)
        availMoves = availableMoves hand rules 0
        score = handScore hand
     in if score < 17 && MHit `elem` availMoves
            then MHit
            else MStand
