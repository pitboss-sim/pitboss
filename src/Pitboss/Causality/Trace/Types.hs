{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Causality.Trace.Types where

import Pitboss.Blackjack
import Pitboss.Causality.Types.Core
import Pitboss.FSM

data family DeathReason (k :: EntityKind)

data instance DeathReason 'Bout = BoutComplete DetailedOutcome
    deriving (Show, Eq)

data instance DeathReason 'PlayerHand = HandResolved PlayerHandResolution
    deriving (Show, Eq)
