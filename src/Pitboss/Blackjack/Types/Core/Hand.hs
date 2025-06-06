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
    deriving (Eq, Show, Ord, Generic)

instance ToJSON HandKind
instance FromJSON HandKind

newtype Hand = Hand [Card]
    deriving (Eq, Show, Generic)

instance ToJSON Hand
instance FromJSON Hand
