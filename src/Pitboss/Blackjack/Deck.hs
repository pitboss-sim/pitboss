module Pitboss.Blackjack.Deck
  ( drawCard,
    shuffle,
    fullDeck,
    CutPoint (..),
    Deck (..),
  )
where

import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, getElems, newListArray, readArray, writeArray)
import Pitboss.Blackjack.Card (Card (..), Rank (..), Suit (..))
import System.Random (StdGen, randomR)

newtype CutPoint = CutPoint Int
  deriving (Show, Eq)

data Deck = Deck
  { cardsRemaining :: [Card],
    cutCard :: Maybe CutPoint
  }
  deriving (Show, Eq)

fullDeck :: [Card]
fullDeck = [Card r s | r <- [Two .. Ace], s <- [Hearts .. Spades]]

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
