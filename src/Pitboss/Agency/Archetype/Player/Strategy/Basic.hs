module Pitboss.Agency.Archetype.Player.Strategy.Basic where

import Control.Lens
import Pitboss.Agency.Archetype.Player.Strategy.Types
import Pitboss.Blackjack.BasicStrategy.Chart.Interpret
import Pitboss.Blackjack.BasicStrategy.Chart.Types
import Pitboss.Blackjack.BasicStrategy.Types
import Pitboss.Blackjack.Materia.Card

lookupBasicStrategy :: GameContext -> Move
lookupBasicStrategy ctx =
    let chart = getBasicStrategyChart
        hand = ctx ^. contextPlayerHand
        upcard = rank (ctx ^. contextDealerUpcard)
        offering = ctx ^. contextOffering
        decision = safeDecisionLookup chart hand upcard offering
     in decisionToMove decision

lookupPerfectStrategy :: GameContext -> Move
lookupPerfectStrategy = lookupBasicStrategy

decisionToMove :: Decision -> Move
decisionToMove (Always move) = move
decisionToMove (Prefer primary (Else _)) = primary

getBasicStrategyChart :: StrategyChart
getBasicStrategyChart = []
