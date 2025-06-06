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
    deriving (Show, Eq, Generic)

data HoleCardRule
    = Peek
    | ENHC
    deriving (Eq, Show, Generic)

data Soft17Rule = StandSoft17 | HitSoft17
    deriving (Show, Eq, Generic)

data DASRule = DAS | NoDAS
    deriving (Show, Eq, Generic)

data DoubleRule
    = DoubleAny
    | Double9_10
    | Double9_11
    | Double10_11
    deriving (Show, Eq, Generic)

data SplitAcesAllowed = NoSplitAces | SplitAces
    deriving (Show, Eq, Generic)

data ResplitAcesAllowed = NoResplitAces | ResplitAces
    deriving (Show, Eq, Generic)

data SplitAcesFrozen = OneCardOnly | FullPlay
    deriving (Show, Eq, Generic)

data SplitHands
    = SP2
    | SP3
    | SP4
    deriving (Show, Eq, Generic)

data Payout = P3_2 | P6_5
    deriving (Show, Eq, Generic)

data Pen
    = PenCards Nat
    | PenFrac Nat Nat
    deriving (Show, Eq, Generic)

instance ToJSON GameRuleSet
instance FromJSON GameRuleSet

instance FromJSON SplitAcesFrozen
instance ToJSON SplitAcesFrozen

instance FromJSON HoleCardRule
instance ToJSON HoleCardRule

instance FromJSON Soft17Rule
instance ToJSON Soft17Rule

instance FromJSON DASRule
instance ToJSON DASRule

instance FromJSON DoubleRule
instance ToJSON DoubleRule

instance FromJSON SplitAcesAllowed
instance ToJSON SplitAcesAllowed

instance FromJSON ResplitAcesAllowed
instance ToJSON ResplitAcesAllowed

instance FromJSON SplitHands
instance ToJSON SplitHands

instance FromJSON Payout
instance ToJSON Payout

instance FromJSON Pen
instance ToJSON Pen
