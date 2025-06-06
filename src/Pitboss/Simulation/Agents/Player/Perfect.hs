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
    let score = handScore (_contextPlayerHand ctx)
     in if score < 17 then Hit else Stand

calculateOptimalMove :: BoutContext -> Move
calculateOptimalMove ctx =
    let score = handScore (_contextPlayerHand ctx)
     in if score < 17 then Hit else Stand
