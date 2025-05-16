{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.DealerHand.Transition where

import Pitboss.FSM.DealerHand.FSM
import Pitboss.FSM.DealerHand.Phase
import Pitboss.FSM.DealerRound hiding (Interrupted)

beginEvaluation :: DealerHandFSM 'Dealing -> DealerHandFSM 'Evaluating
beginEvaluation DealingFSM = EvaluatingFSM

resolveHand :: DealerHandResolution -> DealerHandFSM 'Evaluating -> DealerHandFSM ('Resolved res)
resolveHand res EvaluatingFSM = ResolvedFSM res

interruptHand :: InterruptReason -> DealerHandFSM p -> DealerHandFSM ('Interrupted r)
interruptHand reason _ = InterruptedFSM reason

resumeFromInterrupt :: DealerHandFSM ('Interrupted r) -> DealerHandFSM 'Dealing
resumeFromInterrupt (InterruptedFSM _) = DealingFSM
