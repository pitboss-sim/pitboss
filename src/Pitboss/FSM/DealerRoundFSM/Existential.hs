{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.DealerRoundFSM.Existential where

import Pitboss.FSM.DealerRoundFSM.Types
import Pitboss.FSM.Types.Transitionable

data DealerRoundFSM
  = PeekDealerRound SomePeekFSM
  | ENHCDealerRound SomeENHCFSM

newtype SomeDealerRoundFSM = SomeDealerRoundFSM DealerRoundFSM

instance Eq SomePeekFSM where
  SomePeekFSM a == SomePeekFSM b = case (a, b) of
    (PeekAwaitingFSM, PeekAwaitingFSM) -> True
    (PeekBetsFSM, PeekBetsFSM) -> True
    (PeekDealFSM, PeekDealFSM) -> True
    (PeekEarlySurrenderFSM, PeekEarlySurrenderFSM) -> True
    (PeekPeekFSM, PeekPeekFSM) -> True
    (PeekInsuranceDecisionFSM, PeekInsuranceDecisionFSM) -> True
    (PeekInsuranceSettledFSM, PeekInsuranceSettledFSM) -> True
    (PeekPlayersFSM, PeekPlayersFSM) -> True
    (PeekDealerFSM, PeekDealerFSM) -> True
    (PeekSettleFSM, PeekSettleFSM) -> True
    (PeekCompleteFSM, PeekCompleteFSM) -> True
    _ -> False

instance Eq SomeDealerRoundFSM where
  (SomeDealerRoundFSM f1) == (SomeDealerRoundFSM f2) = f1 == f2

instance Show SomeDealerRoundFSM where
  show (SomeDealerRoundFSM fsm) = "SomeDealerRoundFSM (" ++ show fsm ++ ")"

data SomePeekFSM = forall p. SomePeekFSM (PeekFSM p)

instance Transitionable SomePeekFSM where
  transitionType (SomePeekFSM fsm) = transitionType fsm

instance Show SomePeekFSM where
  show (SomePeekFSM fsm) = show fsm

instance Eq DealerRoundFSM where
  PeekDealerRound f1 == PeekDealerRound f2 = f1 == f2
  ENHCDealerRound f1 == ENHCDealerRound f2 = f1 == f2
  _ == _ = False

instance Show DealerRoundFSM where
  show = \case
    PeekDealerRound f -> "PeekDealerRound (" ++ show f ++ ")"
    ENHCDealerRound f -> "ENHCDealerRound (" ++ show f ++ ")"

instance Transitionable SomeDealerRoundFSM where
  transitionType (SomeDealerRoundFSM fsm) = case fsm of
    PeekDealerRound f -> transitionType f
    ENHCDealerRound f -> transitionType f

data SomeENHCFSM = forall p. SomeENHCFSM (ENHCFSM p)

instance Transitionable SomeENHCFSM where
  transitionType (SomeENHCFSM fsm) = transitionType fsm

instance Show SomeENHCFSM where
  show (SomeENHCFSM fsm) = show fsm

instance Eq SomeENHCFSM where
  SomeENHCFSM a == SomeENHCFSM b = case (a, b) of
    (ENHCAwaitingFSM, ENHCAwaitingFSM) -> True
    (ENHCBetsFSM, ENHCBetsFSM) -> True
    (ENHCDealFSM, ENHCDealFSM) -> True
    (ENHCEarlySurrenderFSM, ENHCEarlySurrenderFSM) -> True
    (ENHCPlayersFSM, ENHCPlayersFSM) -> True
    (ENHCDealerFSM, ENHCDealerFSM) -> True
    (ENHCSettleFSM, ENHCSettleFSM) -> True
    (ENHCCompleteFSM, ENHCCompleteFSM) -> True
    _ -> False

roundFlavor :: SomeDealerRoundFSM -> DealerRoundFlavor
roundFlavor (SomeDealerRoundFSM fsm) = case fsm of
  PeekDealerRound _ -> IsPeek
  ENHCDealerRound _ -> IsENHC
