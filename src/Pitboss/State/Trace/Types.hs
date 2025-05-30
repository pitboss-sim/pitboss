{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.State.Trace.Types where

import Pitboss.Blackjack.Play (Outcome)
import Pitboss.FSM.PlayerHand (PlayerHandResolution)
import Pitboss.State.Types.Core

data family DeathReason (k :: EntityKind)

data instance DeathReason 'Bout = BoutComplete Outcome
    deriving (Show, Eq)

data instance DeathReason 'PlayerHand = HandResolved PlayerHandResolution
    deriving (Show, Eq)
