{-# LANGUAGE DataKinds #-}

module Pitboss.Orchestration.DeltaGen where

import Pitboss.FSM.PlayerHand
import Pitboss.State.Delta.Types
import Pitboss.State.Entity.Types
import Pitboss.State.Types.Core

processEvent :: EntityState 'Event -> EntityState 'Intent -> CausalHistory -> [SomeDelta 'PlayerHand]
processEvent event intent history =
    case (_eventAttrsType (_eventAttrs event), _intentAttrsDetails (_intentAttrs intent)) of
        (IntentValidated, PlayerStandIntent) ->
            [ ModesDelta
                history
                ( DPlayerHandSetPlayerHandFSM
                    (SomePlayerHandFSM DecisionFSM)
                    (SomePlayerHandFSM (ResolvedFSM Stand))
                )
            ]
        _ -> []
