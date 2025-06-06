{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Blackjack.Types.Core.Hand where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Blackjack.Types.Core.Card

data HandKind
    = BlackjackHand
    | TwentyOneHand
    | SoftHand
    | HardHand
    | PairHand
    | BustHand
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

newtype Hand = Hand [Card]
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
