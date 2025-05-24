module Pitboss.Blackjack.Offering.RuleSet where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import GHC.TypeLits (Nat)

data HoleCardRule
    = Peek
    | ENHC
    deriving (Eq, Show, Generic)

data InsuranceOutcome = Lost | Paid | PaidEvenMoney
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

instance FromJSON SplitAcesFrozen

instance ToJSON SplitAcesFrozen

data SplitHands
    = SP2
    | SP3
    | SP4
    deriving (Show, Eq, Generic)

data Surrender = Early | Late | NoSurrender
    deriving (Show, Eq, Generic)

data Payout = P3_2 | P6_5
    deriving (Show, Eq, Generic)

data Pen
    = PenCards Nat
    | PenFrac Nat Nat
    deriving (Show, Eq, Generic)

data RuleSet = RuleSet
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

instance FromJSON HoleCardRule
instance ToJSON HoleCardRule

instance FromJSON InsuranceOutcome
instance ToJSON InsuranceOutcome

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

instance FromJSON Surrender
instance ToJSON Surrender

instance FromJSON Payout
instance ToJSON Payout

instance FromJSON Pen
instance ToJSON Pen

instance FromJSON RuleSet
instance ToJSON RuleSet

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
