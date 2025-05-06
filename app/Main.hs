{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens hiding (uncons)
import Data.List (uncons)
import Pitboss.Blackjack.Card
import Pitboss.Blackjack.Deck
import Pitboss.Blackjack.Hand
import Pitboss.Blackjack.Hand.Score (Score (..), isSoft, scoreHand)
import Pitboss.Blackjack.Offering (Offering (..))
import Pitboss.Blackjack.Offering.RuleSet (isH17)
import Pitboss.Blackjack.Offering.WellKnown (vegas6)
import Pitboss.Sim.State.Spot
import Pitboss.Sim.State.Table
import Pitboss.Sim.State.Table.Lens (lensDeck, lensPlayerSpots)
import Pitboss.Sim.State.Table.Mutation (claimSpot)
import Pitboss.Sim.Types.FiniteMap
import Pitboss.Sim.Types.Occupancy
import System.Environment (getArgs)
import System.Random (mkStdGen, randomIO)
import Text.Read (readMaybe)

data RoundOutcome
  = PlayerWins
  | DealerWins
  | Push
  | BlackjackWin
  deriving (Show, Eq)

-- Plays out the dealer hand using remaining cards, returns (final hand, remaining deck)
playDealerHand :: Offering -> Hand -> [Card] -> (Hand, [Card])
playDealerHand offering hand deck =
  case score of
    Busted -> (hand, deck)
    Blackjack -> (hand, deck)
    Valued n
      | n > 17 -> (hand, deck)
      | n == 17 && isH17 ruleSet' && isSoft hand -> drawAndContinue
      | n >= 17 -> (hand, deck)
      | otherwise -> drawAndContinue
  where
    score = scoreHand hand

    ruleSet' = ruleSet offering
    matter' = matter offering

    drawAndContinue = case uncons deck of
      Nothing -> (hand, []) -- deck ran out, shouldn't happen
      Just (c, rest) ->
        case mkHand matter' (c : unHand hand) of
          Just newHand -> playDealerHand offering newHand rest
          Nothing -> (hand, rest) -- fallback if over max hand size

compareHands :: Offering -> Hand -> Hand -> RoundOutcome
compareHands _ player dealer =
  case (scoreHand player, scoreHand dealer) of
    (Busted, _) -> DealerWins
    (_, Busted) -> PlayerWins
    (Blackjack, Blackjack) -> Push
    (Blackjack, _) -> BlackjackWin
    (_, Blackjack) -> DealerWins
    (Valued p, Valued d)
      | p > d -> PlayerWins
      | p < d -> DealerWins
      | otherwise -> Push

getSeedFromArgs :: IO Int
getSeedFromArgs = do
  args <- getArgs
  case args of
    [] -> do
      seed <- randomIO :: IO Int
      putStrLn $ "🌱 Generated random seed: " ++ show seed
      pure seed
    (arg : _) ->
      case readMaybe arg :: Maybe Int of
        Just s -> do
          putStrLn $ "🌱 Provided seed: " ++ show s
          pure s
        Nothing -> do
          putStrLn $ "⚠️  Invalid seed: " ++ arg
          putStrLn "Usage: pitboss [optional-seed]"
          error "Aborting due to invalid seed"

main :: IO ()
main = do
  putStrLn "🎲 Starting single-player round..."

  seed <- getSeedFromArgs
  let rng = mkStdGen seed
  let (shuffled, _) = shuffle rng (cardsRemaining (mkDeck (matter vegas6)))

  let actorId = mkActorId (Uid "AAAAAA-00000000")
  let table0 = initPeekTableState & lensDeck .~ shuffled

-- let table1 = case claimSpot Spot1 actorId table0 of
--   Right t -> t
--   Left err -> error $ "💥 Failed to claim spot: " ++ err

-- case table1 ^. lensDeck of
--   c1 : c2 : dealer1 : dealer2 : rest ->
--     let playerHand = Hand [c1, c2]
--         spotState =
--           SpotState
--             { hands = singletonFiniteMap Hand1 (Present (mkSpotHandState playerHand)) Absent,
--               turn = Playing Hand1
--             }
--
--         table2 =
--           table1
--             & lensDeck .~ rest
--             & lensPlayerSpots %~ insertFiniteMap Spot1 (Present spotState)
--
--         dealerInitial = Hand [dealer1, dealer2]
--         (dealerFinal, _) = playDealerHand vegas6 dealerInitial rest
--         outcome = compareHands vegas6 playerHand dealerFinal
--      in do
--           putStrLn $ "🃏 Player was dealt: " ++ show [c1, c2]
--           putStrLn $ "🪭 Dealer was dealt: " ++ show [dealer1, dealer2]
--           putStrLn $ "🤖 Dealer final hand: " ++ show dealerFinal
--           putStrLn $ "📊 Final Spot State: " ++ show (table2 ^. lensPlayerSpots)
--           putStrLn $ "🎯 Outcome: " ++ show outcome
--           putStrLn "✅ Round complete."
--   _ -> error "⛔ Not enough cards to play a round."
