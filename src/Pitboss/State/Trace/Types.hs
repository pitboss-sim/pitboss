{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.State.Trace.Types where

import Pitboss.Blackjack
import Pitboss.FSM.PlayerHand
import Pitboss.State.Types.Core

data family DeathReason (k :: EntityKind)

data instance DeathReason 'Bout = BoutComplete DetailedOutcome
    deriving (Show, Eq)

data instance DeathReason 'PlayerHand = HandResolved PlayerHandResolution
    deriving (Show, Eq)
