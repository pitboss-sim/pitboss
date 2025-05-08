module Pitboss.FSM.PlayerHandFSM.Existential where

import Pitboss.FSM.PlayerHandFSM.Types

data SomePlayerHandFSM = forall p h d s. SomePlayerHandFSM (PlayerHandFSM p h d s)

mkPlayerHandFSMAbandoned :: AbandonedReason -> SomePlayerHandFSM
mkPlayerHandFSMAbandoned reason = SomePlayerHandFSM (AbandonedFSM reason)
