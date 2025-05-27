module Pitboss.Blackjack.Hand.Analysis where

import Pitboss.Blackjack.Card (Card (..), Rank (..), rankValue)

data HandAnalysis = HandAnalysis
    { _value :: Int
    , _isSoft :: Bool
    , _isBust :: Bool
    , _isPair :: Bool
    , _isBlackjack :: Bool
    }
    deriving (Show)

analyzeHand :: [Card] -> HandAnalysis
analyzeHand cards =
    let ranks = map cardRank cards
        aceCount = length (filter (== Ace) ranks)
        nonAceSum = sum (map rankValue (filter (/= Ace) ranks))

        (finalValue, usingSoftAce) = bestAceValue nonAceSum aceCount

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

isPair :: [Rank] -> Bool
isPair [r1, r2] = r1 == r2
isPair _ = False

cardRank :: Card -> Rank
cardRank (Card rank _) = rank
