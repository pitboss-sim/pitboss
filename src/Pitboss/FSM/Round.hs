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

abandonHandDueToSurrender :: GameRuleSet -> Bool -> SomeContestantHandFSM
abandonHandDueToSurrender _ early =
    SomeContestantHandFSM $
        if early
            then CHAbandonedFSM (CSurrender Early)
            else CHAbandonedFSM (CSurrender Late)

abandonHandDueToInsurance :: Bool -> SomeContestantHandFSM
abandonHandDueToInsurance evenMoney =
    SomeContestantHandFSM $
        CHAbandonedFSM $
            if evenMoney then CInsurance PaidEvenMoney else CInsurance Paid

atPlayersPhase :: RoundFSM -> Bool
atPlayersPhase = \case
    PeekRound (SomePeekFSM _) -> True
    ENHCRound (SomeENHCFSM _) -> True
