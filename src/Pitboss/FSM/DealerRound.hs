{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.DealerRound (
    module Pitboss.FSM.DealerRound.ENHC,
    module Pitboss.FSM.DealerRound.Peek,
    module Pitboss.FSM.DealerRound.Phase,
    module Pitboss.FSM.DealerRound.Typeclass.AtDecisionPoint,
    module Pitboss.FSM.DealerRound.Typeclass.PhaseTag,
    DealerRoundFSM (..),
    mkENHCDealerRound,
    mkPeekDealerRound,
    abandonHandDueToSurrender,
    abandonHandDueToInsurance,
    atPlayersPhase,
)
where

import Data.Aeson.Types
import Data.Text qualified as T
import Pitboss.Blackjack.Offering.RuleSet
import Pitboss.FSM.DealerRound.ENHC
import Pitboss.FSM.DealerRound.Peek
import Pitboss.FSM.DealerRound.Phase
import Pitboss.FSM.DealerRound.Typeclass.AtDecisionPoint
import Pitboss.FSM.DealerRound.Typeclass.PhaseTag
import Pitboss.FSM.PlayerHand
import Pitboss.FSM.Types
import Pitboss.FSM.Types.Transitionable

data DealerRoundFSM
    = PeekDealerRound SomePeekFSM
    | ENHCDealerRound SomeENHCFSM

mkENHCDealerRound :: ENHCFSM p -> DealerRoundFSM
mkENHCDealerRound = ENHCDealerRound . SomeENHCFSM

mkPeekDealerRound :: PeekFSM p -> DealerRoundFSM
mkPeekDealerRound = PeekDealerRound . SomePeekFSM

instance Eq DealerRoundFSM where
    PeekDealerRound f1 == PeekDealerRound f2 = f1 == f2
    ENHCDealerRound f1 == ENHCDealerRound f2 = f1 == f2
    _ == _ = False

instance Show DealerRoundFSM where
    show = \case
        PeekDealerRound f -> "PeekDealerRound (" ++ show f ++ ")"
        ENHCDealerRound f -> "ENHCDealerRound (" ++ show f ++ ")"

instance Transitionable DealerRoundFSM where
    transitionType = \case
        PeekDealerRound f -> transitionType f
        ENHCDealerRound f -> transitionType f

instance ToJSON DealerRoundFSM where
    toJSON = \case
        PeekDealerRound peekFsm ->
            object ["flavor" .= String "Peek", "state" .= toJSON peekFsm]
        ENHCDealerRound enhcFsm ->
            object ["flavor" .= String "ENHC", "state" .= toJSON enhcFsm]

instance FromJSON DealerRoundFSM where
    parseJSON = withObject "DealerRoundFSM" $ \obj -> do
        flavor <- obj .: "flavor"
        case (flavor :: T.Text) of
            "Peek" -> PeekDealerRound <$> obj .: "state"
            "ENHC" -> ENHCDealerRound <$> obj .: "state"
            other -> fail $ "Unknown flavor for DealerRoundFSM: " ++ T.unpack other

-- helpers

abandonHandDueToSurrender :: GameRuleSet -> Bool -> SomePlayerHandFSM
abandonHandDueToSurrender _ early =
    SomePlayerHandFSM $
        if early
            then AbandonedFSM (Surrender Early)
            else AbandonedFSM (Surrender Late)

abandonHandDueToInsurance :: Bool -> SomePlayerHandFSM
abandonHandDueToInsurance evenMoney =
    SomePlayerHandFSM $
        AbandonedFSM $
            if evenMoney then Insurance PaidEvenMoney else Insurance Paid

atPlayersPhase :: DealerRoundFSM -> Bool
atPlayersPhase = \case
    PeekDealerRound (SomePeekFSM _) -> True
    ENHCDealerRound (SomeENHCFSM _) -> True
