{-# LANGUAGE DataKinds #-}

module Pitboss.Blackjack.Materia.Types.Core where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import GHC.Generics (Generic)

data HandKindWitness (k :: HandKind) where
    BlackjackWitness :: HandKindWitness 'BlackjackHand
    TwentyOneWitness :: HandKindWitness 'TwentyOneHand
    SoftWitness :: HandKindWitness 'SoftHand
    HardWitness :: HandKindWitness 'HardHand
    PairWitness :: HandKindWitness 'PairHand
    BustWitness :: HandKindWitness 'BustHand

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
