{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Blackjack.Types.GameRuleSet where

import Data.Aeson.Types
import GHC.Generics (Generic)
import GHC.TypeLits (Nat)
import Pitboss.Blackjack.Types.Core

data GameRuleSet = GameRuleSet
    { holeCardRule :: HoleCardRule
    , soft17 :: Soft17Rule
    , das :: DASRule
    , doubling :: DoubleRule
    , splitAcesAllowed :: SplitAcesAllowed
    , resplitAcesAllowed :: ResplitAcesAllowed
    , splitAcesFrozen :: SplitAcesFrozen
    , splitHands :: SplitHands
    , surrender :: Surrender
    , payout :: Payout
    , pen :: Pen
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data HoleCardRule
    = Peek
    | ENHC
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Soft17Rule = StandSoft17 | HitSoft17
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DASRule = DAS | NoDAS
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DoubleRule
    = DoubleAny
    | Double9_10
    | Double9_11
    | Double10_11
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data SplitAcesAllowed = NoSplitAces | SplitAces
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ResplitAcesAllowed = NoResplitAces | ResplitAces
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data SplitAcesFrozen = OneCardOnly | FullPlay
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data SplitHands
    = SP2
    | SP3
    | SP4
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Payout = P3_2 | P6_5
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Pen
    = PenCards Nat
    | PenFrac Nat Nat
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
