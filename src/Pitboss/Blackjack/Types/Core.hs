{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Blackjack.Types.Core where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import GHC.Generics (Generic)

data HandKindWitness (k :: HandKind) where
    BlackjackWitness :: HandKindWitness 'BlackjackHand
    TwentyOneWitness :: HandKindWitness 'TwentyOneHand
    SoftWitness :: HandKindWitness 'SoftHand
    HardWitness :: HandKindWitness 'HardHand
    PairWitness :: HandKindWitness 'PairHand
    BustWitness :: HandKindWitness 'BustHand

instance Show (HandKindWitness k) where
    show BlackjackWitness = "BlackjackWitness"
    show TwentyOneWitness = "TwentyOneWitness"
    show SoftWitness = "SoftWitness"
    show HardWitness = "HardWitness"
    show PairWitness = "PairWitness"
    show BustWitness = "BustWitness"

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

data Hand (k :: HandKind) where
    Hand :: [Card] -> Hand k

unHand :: Hand (k :: HandKind) -> [Card]
unHand (Hand cards) = cards

data Suit = Hearts | Diamonds | Clubs | Spades
    deriving (Show, Eq, Enum, Generic)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Eq, Enum, Ord, Generic)

data Card = Card {rank :: Rank, suit :: Suit}
    deriving (Show, Eq, Generic)

instance ToJSON Suit
instance FromJSON Suit

instance ToJSON Rank
instance FromJSON Rank
instance ToJSONKey Rank
instance FromJSONKey Rank

instance ToJSON Card
instance FromJSON Card

data HandAnalysis = HandAnalysis
    { _value :: Int
    , _isSoft :: Bool
    , _isBust :: Bool
    , _isPair :: Bool
    , _isBlackjack :: Bool
    }
    deriving (Show)

deriving instance Show (Hand k)
deriving instance Eq (Hand k)

value :: Card -> Int
value (Card Two _) = 2
value (Card Three _) = 3
value (Card Four _) = 4
value (Card Five _) = 5
value (Card Six _) = 6
value (Card Seven _) = 7
value (Card Eight _) = 8
value (Card Nine _) = 9
value (Card Ten _) = 10
value (Card Jack _) = 10
value (Card Queen _) = 10
value (Card King _) = 10
value (Card Ace _) = 11

cardValue :: Card -> Int
cardValue = rankValue . rank

rankValue :: Rank -> Int
rankValue r = case r of
    Two -> 2
    Three -> 3
    Four -> 4
    Five -> 5
    Six -> 6
    Seven -> 7
    Eight -> 8
    Nine -> 9
    Ten -> 10
    Jack -> 10
    Queen -> 10
    King -> 10
    Ace -> 11

newtype Chips = Chips Int
    deriving (Eq, Ord, Show, Num, Generic)

instance ToJSON Chips
instance FromJSON Chips
