{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.Dealer.Round (
    module Pitboss.FSM.Dealer.Round.ENHC,
    module Pitboss.FSM.Dealer.Round.Peek,
    module Pitboss.FSM.Dealer.Round.Phase,
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
import Pitboss.Blackjack hiding (Surrender)
import Pitboss.FSM.Dealer.Round.ENHC
import Pitboss.FSM.Dealer.Round.Peek
import Pitboss.FSM.Dealer.Round.Phase
import Pitboss.FSM.Player.Hand
import Pitboss.FSM.Transitionable
import Pitboss.FSM.Types

data DealerRoundFSM
    = PeekDealerRound SomePeekFSM
    | ENHCDealerRound SomeENHCFSM

instance Transitionable DealerRoundFSM where
    transitionType = \case
        PeekDealerRound f -> transitionType f
        ENHCDealerRound f -> transitionType f

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

abandonHandDueToSurrender :: GameRuleSet -> Bool -> SomePlayerHandFSM
abandonHandDueToSurrender _ early =
    SomePlayerHandFSM $
        if early
            then PHAbandonedFSM (PHSurrender Early)
            else PHAbandonedFSM (PHSurrender Late)

abandonHandDueToInsurance :: Bool -> SomePlayerHandFSM
abandonHandDueToInsurance evenMoney =
    SomePlayerHandFSM $
        PHAbandonedFSM $
            if evenMoney then PHInsurance PaidEvenMoney else PHInsurance Paid

atPlayersPhase :: DealerRoundFSM -> Bool
atPlayersPhase = \case
    PeekDealerRound (SomePeekFSM _) -> True
    ENHCDealerRound (SomeENHCFSM _) -> True
