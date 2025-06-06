{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Pitboss.FSM.Transitionable where

import Data.Kind (Type)
import Pitboss.FSM.Types.Core

class Transitionable (fsm :: Type) where
    transitionType :: fsm -> TransitionPhase
