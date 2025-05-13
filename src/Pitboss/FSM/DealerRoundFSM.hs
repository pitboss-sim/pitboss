{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.DealerRoundFSM
  ( module Pitboss.FSM.DealerRoundFSM.ENHC,
    module Pitboss.FSM.DealerRoundFSM.Peek,
    module Pitboss.FSM.DealerRoundFSM.Phase,
    module Pitboss.FSM.DealerRoundFSM.Typeclass.AtDecisionPoint,
    module Pitboss.FSM.DealerRoundFSM.Typeclass.PhaseTag,
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
import Pitboss.FSM.DealerRoundFSM.ENHC
import Pitboss.FSM.DealerRoundFSM.Peek
import Pitboss.FSM.DealerRoundFSM.Phase
import Pitboss.FSM.DealerRoundFSM.Typeclass.AtDecisionPoint
import Pitboss.FSM.DealerRoundFSM.Typeclass.PhaseTag
import Pitboss.FSM.PlayerHandFSM
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

abandonHandDueToSurrender :: RuleSet -> Bool -> SomePlayerHandFSM
abandonHandDueToSurrender _ early =
  if early
    then mkPlayerHandFSMAbandoned (Surrender Early)
    else mkPlayerHandFSMAbandoned (Surrender Late)

abandonHandDueToInsurance :: Bool -> SomePlayerHandFSM
abandonHandDueToInsurance evenMoney =
  mkPlayerHandFSMAbandoned $
    if evenMoney then Insurance PaidEvenMoney else Insurance Paid

atPlayersPhase :: DealerRoundFSM -> Bool
atPlayersPhase = \case
  PeekDealerRound (SomePeekFSM _) -> True
  ENHCDealerRound (SomeENHCFSM _) -> True
