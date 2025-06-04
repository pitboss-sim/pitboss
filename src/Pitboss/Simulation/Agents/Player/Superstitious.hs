{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Simulation.Agents.Player.Superstitious where

import Control.Monad.State
import Pitboss.Blackjack
import Pitboss.Simulation.Agents.Types
import System.Random

getSuperstitiousMove :: ArchetypeConfig 'Superstitious -> BoutContext -> State StdGen Move
getSuperstitiousMove config ctx =
    case findApplicableSuperstition (scBeliefs config) ctx of
        Just forcedMove ->
            let hand = _contextBoutPlayerHand ctx
                rules = gameRuleSet (_contextOffering ctx)
                availMoves = availableMoves hand rules 0
             in pure $ if forcedMove `elem` availMoves then forcedMove else MStand
        Nothing -> pure $ lookupFallbackStrategy (scFallbackChart config) ctx

findApplicableSuperstition :: [Superstition] -> BoutContext -> Maybe Move
findApplicableSuperstition beliefs ctx =
    let hand = _contextBoutPlayerHand ctx
        dealerCard = _contextDealerUpcard ctx
     in case filter (superstitionApplies hand dealerCard) beliefs of
            (belief : _) -> Just (superstitionMove belief)
            [] -> Nothing

superstitionApplies :: SomeHand -> Card -> Superstition -> Bool
superstitionApplies hand (Card dealerRank _) = \case
    NeverHitOn16 -> handScore hand == 16
    AlwaysStandSoft18 -> handScore hand == 18 && isSoft (handWitness hand)
    NeverSplitTens -> case extractPairRank hand of
        Just Ten -> True
        _ -> False
    AlwaysSplitAces -> case extractPairRank hand of
        Just Ace -> True
        _ -> False
    NeverDoubleDown -> False
    StandOn12VsDealer2Or3 ->
        handScore hand == 12 && (dealerRank == Two || dealerRank == Three)

superstitionMove :: Superstition -> Move
superstitionMove = \case
    NeverHitOn16 -> MStand
    AlwaysStandSoft18 -> MStand
    NeverSplitTens -> MHit
    AlwaysSplitAces -> MSplit
    NeverDoubleDown -> MHit
    StandOn12VsDealer2Or3 -> MStand

lookupFallbackStrategy :: StrategyChart -> BoutContext -> Move
lookupFallbackStrategy chart ctx =
    let hand = _contextBoutPlayerHand ctx
        upcard = rank (_contextDealerUpcard ctx)
        offering = _contextOffering ctx
        decision = safeDecisionLookup chart hand upcard offering
     in decisionToMove decision

decisionToMove :: Decision -> Move
decisionToMove (Always move) = move
decisionToMove (Prefer primary (Else _)) = primary
