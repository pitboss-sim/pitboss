{-# LANGUAGE DeriveGeneric #-}

module Pitboss.Blackjack.BasicStrategy.Chart.Types where

import Data.Aeson
import Data.Map.Strict
import GHC.Generics (Generic)
import Pitboss.Blackjack.Materia.Card
import Pitboss.Blackjack.Materia.Hand

data HandPrefix
    = PA
    | PT
    | P Int
    | A Int
    | H Int
    deriving (Eq, Ord, Show)

data MoveCode
    = MoveHit
    | MoveStand
    | MoveDoubleOrHit
    | MoveDoubleOrStand
    | MoveSplit
    | MoveSplitOrHit
    | MoveSurrenderOrStand
    | MoveUndefined
    deriving (Eq, Show, Generic)

data ChartEntry = ChartEntry
    { handKind :: HandKind
    , kindValue :: Maybe Int
    , moves :: Map Rank MoveCode
    }
    deriving (Eq, Show, Generic)

type StrategyChart = [ChartEntry]

handPrefixToKind :: HandPrefix -> (HandKind, Maybe Int)
handPrefixToKind PA = (PairHand, Just 1)
handPrefixToKind PT = (BlackjackHand, Nothing)
handPrefixToKind (P n) = (PairHand, Just n)
handPrefixToKind (A nonAceValue) = (SoftHand, Just (11 + nonAceValue))
handPrefixToKind (H n) = (HardHand, Just n)

kindToHandPrefix :: HandKind -> Maybe Int -> HandPrefix
kindToHandPrefix PairHand (Just 1) = PA
kindToHandPrefix BlackjackHand _ = PT
kindToHandPrefix PairHand (Just n) = P n
kindToHandPrefix SoftHand (Just softTotal) = A (softTotal - 11)
kindToHandPrefix HardHand (Just n) = H n
kindToHandPrefix TwentyOneHand (Just n) = H n
kindToHandPrefix BustHand (Just n) = H n
kindToHandPrefix _ _ = H 0

instance ToJSON MoveCode
instance FromJSON MoveCode

instance ToJSON ChartEntry
instance FromJSON ChartEntry
