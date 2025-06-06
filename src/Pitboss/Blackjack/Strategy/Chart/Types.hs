{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Blackjack.Strategy.Chart.Types where

import Data.Aeson.Types
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Types.Core

data HandPrefix
    = PA
    | PT
    | P Int
    | A Int
    | H Int
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data MoveCode
    = MoveHit
    | MoveStand
    | MoveDoubleOrHit
    | MoveDoubleOrStand
    | MoveSplit
    | MoveSplitOrHit
    | MoveSurrenderOrStand
    | MoveUndefined
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ChartEntry = ChartEntry
    { handKind :: HandKind
    , kindValue :: Maybe Int
    , moves :: Map Rank MoveCode
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

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
