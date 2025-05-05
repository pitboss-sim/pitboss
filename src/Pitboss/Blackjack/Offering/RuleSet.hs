module Pitboss.Blackjack.Offering.RuleSet
  ( RuleSet (..),
    Soft17Rule (..),
    DASRule (..),
    DoubleRule (..),
    SplitHands (..),
    SplitAcesAllowed (..),
    ResplitAcesAllowed (..),
    SplitAcesFrozen (..),
    Surrender (..),
    Payout (..),
    Pen (..),
    canDouble,
    maxSplits,
    canSplitAnotherHand,
    isH17,
  )
where

import GHC.TypeLits (Nat)

data Soft17Rule = StandSoft17 | HitSoft17
  deriving (Show, Eq)

data DASRule = DAS | NoDAS
  deriving (Show, Eq)

data DoubleRule
  = DoubleAny
  | Double9_10
  | Double9_11
  | Double10_11
  deriving (Show, Eq)

data SplitAcesAllowed = NoSplitAces | SplitAces
  deriving (Show, Eq)

data ResplitAcesAllowed = NoResplitAces | ResplitAces
  deriving (Show, Eq)

data SplitAcesFrozen = OneCardOnly | FullPlay
  deriving (Show, Eq)

data SplitHands
  = SP2
  | SP3
  | SP4
  deriving (Show, Eq)

data Surrender = Early | Late | NoSurrender
  deriving (Show, Eq)

data Payout = P3_2 | P6_5
  deriving (Show, Eq)

data Pen
  = PenCards Nat
  | PenFrac Nat Nat
  deriving (Show, Eq)

data RuleSet = RuleSet
  { soft17 :: Soft17Rule,
    das :: DASRule,
    doubling :: DoubleRule,
    splitAcesAllowed :: SplitAcesAllowed,
    resplitAcesAllowed :: ResplitAcesAllowed,
    splitAcesFrozen :: SplitAcesFrozen,
    splitHands :: SplitHands,
    surrender :: Surrender,
    payout :: Payout,
    pen :: Pen
  }
  deriving (Show, Eq)

canDouble :: DoubleRule -> Int -> Bool
canDouble DoubleAny _ = True
canDouble Double9_10 t = t == 9 || t == 10
canDouble Double9_11 t = t >= 9 && t <= 11
canDouble Double10_11 t = t == 10 || t == 11

maxSplits :: SplitHands -> Int
maxSplits SP2 = 2
maxSplits SP3 = 3
maxSplits SP4 = 4

canSplitAnotherHand :: SplitHands -> Int -> Bool
canSplitAnotherHand limit current = current < maxSplits limit

isH17 :: RuleSet -> Bool
isH17 rs = case soft17 rs of
  HitSoft17 -> True
  StandSoft17 -> False
