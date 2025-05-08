{-# LANGUAGE GADTs #-}

module Pitboss.FSM.DealerHandFSM.Existential where

import Pitboss.FSM.DealerHandFSM.Types

data SomeDealerHandFSM = forall p. SomeDealerHandFSM (DealerHandFSM p)

instance Show SomeDealerHandFSM where
  show (SomeDealerHandFSM fsm) = "SomeDealerHandFSM (" ++ show fsm ++ ")"

instance Eq SomeDealerHandFSM where
  (SomeDealerHandFSM f1) == (SomeDealerHandFSM f2) = case (f1, f2) of
    (DealingFSM, DealingFSM) -> True
    (EvaluatingFSM, EvaluatingFSM) -> True
    (ResolvedFSM r1, ResolvedFSM r2) -> r1 == r2
    _ -> False

mkDealerHandFSMResolved :: DealerHandResolution -> SomeDealerHandFSM
mkDealerHandFSMResolved res = SomeDealerHandFSM (ResolvedFSM res)
