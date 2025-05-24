{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Causality.Trace.Types where

import Pitboss.Blackjack
import Pitboss.Causality.Types.Core
import Pitboss.FSM
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data family DeathReason (k :: EntityKind)

data instance DeathReason 'Bout = BoutComplete DetailedOutcome
    deriving (Eq, Show, Generic)

instance ToJSON (DeathReason 'Bout)
instance FromJSON (DeathReason 'Bout)

data instance DeathReason 'PlayerHand = HandResolved PlayerHandResolution
    deriving (Eq, Show, Generic)

instance ToJSON (DeathReason 'PlayerHand)
instance FromJSON (DeathReason 'PlayerHand)
