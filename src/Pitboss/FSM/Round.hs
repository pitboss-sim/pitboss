{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.Round (
    module Pitboss.FSM.Round.ENHC,
    module Pitboss.FSM.Round.Peek,
    RoundFSM (..),
    mkENHCRound,
    mkPeekRound,
    abandonHandDueToSurrender,
    abandonHandDueToInsurance,
    atPlayersPhase,
)
where

import Pitboss.Blackjack
import Pitboss.FSM.Round.ENHC
import Pitboss.FSM.Round.Peek
import Pitboss.FSM.Types

abandonHandDueToSurrender :: GameRuleSet -> Bool -> SomePlayerHandFSM
abandonHandDueToSurrender _ early =
    SomePlayerHandFSM $
        if early
            then PHAbandonedFSM (CSurrender Early)
            else PHAbandonedFSM (CSurrender Late)

abandonHandDueToInsurance :: Bool -> SomePlayerHandFSM
abandonHandDueToInsurance evenMoney =
    SomePlayerHandFSM $
        PHAbandonedFSM $
            if evenMoney then CInsurance PaidEvenMoney else CInsurance Paid

atPlayersPhase :: RoundFSM -> Bool
atPlayersPhase = \case
    PeekRound (SomePeekFSM _) -> True
    ENHCRound (SomeENHCFSM _) -> True
