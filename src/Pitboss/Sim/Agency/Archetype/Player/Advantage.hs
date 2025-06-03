{-# LANGUAGE DataKinds #-}

module Pitboss.Sim.Agency.Archetype.Player.Advantage where

import Control.Monad.State
import Data.Map qualified as Map
import Pitboss.Blackjack
import Pitboss.Sim.Agency.Archetype.Types
import Pitboss.Sim.Agency.Types
import System.Random

getAdvantageMove :: ArchetypeConfig 'Advantage -> ArchetypeState 'Advantage -> GameContext -> State StdGen Move
getAdvantageMove config state' ctx = do
    let tc = round (asTrueCount state') :: Int
        baseMove = calculateBaseMove ctx
        deviatedMove = applyDeviations (acDeviationChart config) tc baseMove ctx
    applyCoverPlay (acBettingSpread config) deviatedMove ctx

calculateBaseMove :: GameContext -> Move
calculateBaseMove ctx =
    let score = handScore (_contextPlayerHand ctx)
     in if score < 17 then Hit else Stand

applyDeviations :: DeviationChart -> Int -> Move -> GameContext -> Move
applyDeviations (DeviationChart deviationMap) trueCount baseMove ctx =
    case Map.lookup trueCount deviationMap of
        Nothing -> baseMove
        Just deviations ->
            case findApplicableDeviation deviations ctx of
                Nothing -> baseMove
                Just deviation -> applyDeviation deviation baseMove

findApplicableDeviation :: [Deviation] -> GameContext -> Maybe Deviation
findApplicableDeviation deviations ctx =
    let playerTotal = handScore (_contextPlayerHand ctx)
     in listToMaybe [d | d <- deviations, devPlayerTotal d == playerTotal]
  where
    listToMaybe [] = Nothing
    listToMaybe (x : _) = Just x

applyDeviation :: Deviation -> Move -> Move
applyDeviation (Deviation _ _ action) _ =
    case action of
        DevStandInsteadOfHit -> Stand
        DevHitInsteadOfStand -> Hit
        DevDoubleInsteadOfHit -> Double

applyCoverPlay :: BettingSpread -> Move -> GameContext -> State StdGen Move
applyCoverPlay spread move _ = do
    case spread of
        FlatBetting -> pure move
        SimpleSpread _ _ -> pure move
        TrueCountSpread _ -> pure move
