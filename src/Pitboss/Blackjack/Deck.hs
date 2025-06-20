{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Blackjack.Deck where

import Control.Monad.ST (ST, runST)
import Data.Aeson.Types
import Data.Array.ST (STArray, getElems, newListArray, readArray, writeArray)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Types.Core
import Pitboss.Blackjack.Types.Offering
import System.Random (StdGen, randomR)

newtype CutPoint = CutPoint Int
    deriving stock (Eq, Show, Generic)
    deriving newtype (ToJSON, FromJSON)

data Deck = Deck
    { cardsRemaining :: [Card]
    , cutCard :: Maybe CutPoint
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

fullDeck :: [Card]
fullDeck = [Card r s | r <- [Two .. Ace], s <- [Hearts .. Spades]]

mkDeck :: Materia -> Deck
mkDeck (Materia decks _) =
    let copies = case decks of
            D1 -> 1
            D2 -> 2
            D6 -> 6
            D8 -> 8
        allCards = concat $ replicate copies fullDeck
     in Deck{cardsRemaining = allCards, cutCard = Nothing}

drawCard :: Deck -> Either String (Card, Deck)
drawCard (Deck [] _) = Left "Deck exhausted"
drawCard (Deck (c : cs) cut) = Right (c, Deck cs cut)

-- Pure, fast Fisher–Yates shuffle using a random seed
shuffle :: forall a. StdGen -> [a] -> ([a], StdGen)
shuffle gen xs = runST $ do
    let n = length xs
    ar <- newListArray (1, n) xs :: ST s (STArray s Int a)
    finalGen <- shuffleArray gen n ar
    shuffled <- getElems ar
    pure (shuffled, finalGen)

-- In-place shuffle of the array
shuffleArray :: StdGen -> Int -> STArray s Int a -> ST s StdGen
shuffleArray g0 n ar = go g0 1
  where
    go g i
        | i >= n = pure g
        | otherwise = do
            let (j, g') = randomR (i, n) g
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar i vj
            writeArray ar j vi
            go g' (i + 1)
