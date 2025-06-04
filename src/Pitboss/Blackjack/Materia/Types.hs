{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Pitboss.Blackjack.Materia.Types where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Text hiding (filter, length, map)
import Pitboss.Blackjack.Materia.Card
import Pitboss.Blackjack.Materia.Instances.Witnessable
import Pitboss.Blackjack.Types.Core

data SomeHand where
    SomeHand :: (Witnessable k) => Hand k -> SomeHand

instance Show SomeHand where
    show (SomeHand hand) = "SomeHand (" ++ show hand ++ ")"

instance Eq SomeHand where
    SomeHand (Hand cards1) == SomeHand (Hand cards2) = cards1 == cards2

instance ToJSON SomeHand where
    toJSON (SomeHand hand) =
        let cards = handCards hand
            handKind :: Text
            handKind = case witness hand of
                BlackjackWitness -> "BlackjackHand"
                TwentyOneWitness -> "TwentyOneHand"
                SoftWitness -> "SoftHand"
                HardWitness -> "HardHand"
                PairWitness -> "PairHand"
                BustWitness -> "BustHand"
         in object
                [ "cards" .= cards
                , "kind" .= handKind
                ]

handCards :: Hand k -> [Card]
handCards (Hand cards) = cards

instance FromJSON SomeHand where
    parseJSON = withObject "SomeHand" $ \obj -> do
        cards <- obj .: "cards"
        kindText <- obj .: "kind"

        let reconstructedHand = characterize cards

        case (kindText :: Text) of
            "BlackjackHand" -> validateKind BlackjackWitness reconstructedHand
            "TwentyOneHand" -> validateKind TwentyOneWitness reconstructedHand
            "SoftHand" -> validateKind SoftWitness reconstructedHand
            "HardHand" -> validateKind HardWitness reconstructedHand
            "PairHand" -> validateKind PairWitness reconstructedHand
            "BustHand" -> validateKind BustWitness reconstructedHand
            _ -> fail $ "Unknown hand kind: " ++ show kindText
      where
        validateKind :: HandKindWitness k -> SomeHand -> Parser SomeHand
        validateKind expectedWitness someHand@(SomeHand hand) =
            case witness hand of
                actualWitness | sameWitness expectedWitness actualWitness -> pure someHand
                actualWitness ->
                    fail $
                        "Hand kind mismatch: expected "
                            ++ showWitness expectedWitness
                            ++ " but characterization produced "
                            ++ showWitness actualWitness

        showWitness :: HandKindWitness k -> String
        showWitness BlackjackWitness = "BlackjackHand"
        showWitness TwentyOneWitness = "TwentyOneHand"
        showWitness SoftWitness = "SoftHand"
        showWitness HardWitness = "HardHand"
        showWitness PairWitness = "PairHand"
        showWitness BustWitness = "BustHand"

        sameWitness :: HandKindWitness k1 -> HandKindWitness k2 -> Bool
        sameWitness BlackjackWitness BlackjackWitness = True
        sameWitness TwentyOneWitness TwentyOneWitness = True
        sameWitness SoftWitness SoftWitness = True
        sameWitness HardWitness HardWitness = True
        sameWitness PairWitness PairWitness = True
        sameWitness BustWitness BustWitness = True
        sameWitness _ _ = False

characterize :: [Card] -> SomeHand
characterize cards = case analyzeHand cards of
    HandAnalysis{..}
        | _isBust -> SomeHand (Hand cards :: Hand 'BustHand)
        | _isPair -> SomeHand (Hand cards :: Hand 'PairHand)
        | _isBlackjack -> SomeHand (Hand cards :: Hand 'BlackjackHand)
        | _value == 21 -> SomeHand (Hand cards :: Hand 'TwentyOneHand)
        | _isSoft -> SomeHand (Hand cards :: Hand 'SoftHand)
        | otherwise -> SomeHand (Hand cards :: Hand 'HardHand)

analyzeHand :: [Card] -> HandAnalysis
analyzeHand cards =
    let ranks = map (\(Card rank _) -> rank) cards
        aceCount = length (filter (== Ace) ranks)
        nonAceSum = sum (map rankValue (filter (/= Ace) ranks))

        (finalValue, usingSoftAce) = bestAceValue nonAceSum aceCount

        isPair :: [Rank] -> Bool
        isPair [r1, r2] = r1 == r2
        isPair _ = False

        isBusted = finalValue > 21
        isSoftHand = usingSoftAce && not isBusted
        isPairHand = isPair ranks
        isNaturalBlackjack = finalValue == 21 && length cards == 2 && aceCount == 1
     in HandAnalysis
            { _value = finalValue
            , _isSoft = isSoftHand
            , _isBust = isBusted
            , _isPair = isPairHand
            , _isBlackjack = isNaturalBlackjack
            }

bestAceValue :: Int -> Int -> (Int, Bool)
bestAceValue nonAceSum aceCount
    | aceCount == 0 = (nonAceSum, False)
    | nonAceSum + 11 + (aceCount - 1) <= 21 = (nonAceSum + 11 + (aceCount - 1), True)
    | otherwise = (nonAceSum + aceCount, False)
