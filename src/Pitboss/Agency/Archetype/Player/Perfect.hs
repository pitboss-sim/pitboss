{-# LANGUAGE DataKinds #-}

module Pitboss.Agency.Archetype.Player.Perfect where

import Control.Monad.State
import Pitboss.Agency.Archetype.Types
import Pitboss.Agency.Types
import Pitboss.Blackjack.Action
import Pitboss.Blackjack.Materia.Hand (handScore)
import System.Random

getPerfectMove :: ArchetypeConfig 'Perfect -> GameContext -> State StdGen Move
getPerfectMove config ctx =
    pure $
        if pcUseEV config
            then calculateEVMove ctx
            else calculateOptimalMove ctx

calculateEVMove :: GameContext -> Move
calculateEVMove ctx =
    let score = handScore (_contextPlayerHand ctx)
     in if score < 17 then Hit else Stand

calculateOptimalMove :: GameContext -> Move
calculateOptimalMove ctx =
    let score = handScore (_contextPlayerHand ctx)
     in if score < 17 then Hit else Stand
