{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.Blackjack.Types.Hand where

import Control.Monad (guard)
import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Blackjack.Types.Core.Card
import Pitboss.Blackjack.Types.Hand.Witness
import Pitboss.Blackjack.Types.Offering

data SomeHand = SomeHand
    { handCards :: [Card]
    , handWitness :: HandWitness
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON SomeHand where
    toJSON (SomeHand cards witness) =
        object
            [ "cards" .= cards
            , "witness" .= witness
            ]

instance FromJSON SomeHand where
    parseJSON = withObject "SomeHand" $ \obj -> do
        cards <- obj .: "cards"
        witness <- obj .: "witness"
        pure (SomeHand cards witness)

mkValidatedHand :: Materia -> [Card] -> Maybe SomeHand
mkValidatedHand matter cards = do
    guard (length cards <= maxCardsFor (matterDecks matter))
    pure (SomeHand cards (HandWitness HardWitness EmptyWitness NoneWitness [] 0))
  where
    maxCardsFor :: DeckCount -> Int
    maxCardsFor d = case d of
        D1 -> 11
        D2 -> 14
        D6 -> 21
        D8 -> 21

isBust :: HandWitness -> Bool
isBust witness = valueType witness == BustWitness

isBlackjack :: HandWitness -> Bool
isBlackjack witness = valueType witness == BlackjackWitness

isSoft :: HandWitness -> Bool
isSoft witness = valueType witness == SoftWitness

isPairOfAces :: SomeHand -> Bool
isPairOfAces hand = case structure (handWitness hand) of
    PairWitness Ace -> True
    _ -> False

extractPairRank :: SomeHand -> Maybe Rank
extractPairRank hand = case structure (handWitness hand) of
    PairWitness rank -> Just rank
    _ -> Nothing

handScore :: SomeHand -> Int
handScore hand = numericValue (handWitness hand)

handProperties :: SomeHand -> HandWitness
handProperties = handWitness
