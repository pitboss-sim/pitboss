{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pitboss.Integration.FullBoutSimulationRoundtripSpec where

import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BSL
import Pitboss.Blackjack hiding (Hand)
import Pitboss.FSM
import Pitboss.Simulation
import System.Directory (createDirectoryIfMissing)
import Test.Hspec
import Test.Pitboss.Matchers
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Complete Game Simulation with JSON Roundtrip" $ describe "Full Bout Lifecycle" $ do
    it "simulates complete blackjack game and roundtrips through JSON" $ do
        let gameEvents = completeBlackjackGameEvents
            originalState = buildSimStateFromMixedEvents gameEvents testTick

        originalState `shouldHavePlayerHandFSM` testBoutId $ PHResolvedFSM PHStand
        originalState `shouldHaveBoutPlayerHandScore` testBoutId $ 20

        let eventJson = encodePretty gameEvents

        withFixtureJsonFile "complete-blackjack-game.json" eventJson $ \filePath -> do
            fileContent <- BSL.readFile filePath

            case decode fileContent of
                Nothing -> expectationFailure "Failed to decode events from JSON"
                Just (restoredEvents :: [SimulationEvent]) -> do
                    let reconstructedState = buildSimStateFromMixedEvents restoredEvents testTick

                    reconstructedState `shouldBe` originalState

    it "simulates dealer blackjack scenario with JSON persistence" $ do
        let gameEvents = dealerBlackjackGameEvents
            _originalState = buildSimStateFromMixedEvents gameEvents testTick

        let eventJson = encodePretty gameEvents

        withFixtureJsonFile "dealer-blackjack-game.json" eventJson $ \filePath -> do
            fileContent <- BSL.readFile filePath
            case decode fileContent of
                Nothing -> expectationFailure "Failed to decode dealer blackjack events"
                Just (restoredEvents :: [SimulationEvent]) -> do
                    let reconstructedState = buildSimStateFromMixedEvents restoredEvents testTick

                    verifyDealerBlackjackState reconstructedState

    it "simulates player bust scenario with event persistence" $ do
        let gameEvents = playerBustGameEvents
            originalState = buildSimStateFromMixedEvents gameEvents testTick

        originalState `shouldHavePlayerHandFSM` testBoutId $ PHResolvedFSM PHBust

        let eventJson = encodePretty gameEvents

        withFixtureJsonFile "player-bust-game.json" eventJson $ \filePath -> do
            fileContent <- BSL.readFile filePath
            case decode fileContent of
                Nothing -> expectationFailure "Failed to decode bust events"
                Just (restoredEvents :: [SimulationEvent]) -> do
                    let reconstructedState = buildSimStateFromMixedEvents restoredEvents testTick

                    reconstructedState `shouldHavePlayerHandFSM` testBoutId $ PHResolvedFSM PHBust

completeBlackjackGameEvents :: [SimulationEvent]
completeBlackjackGameEvents =
    basicGameSetupMixedEvents
        ++ [ playerDealtCard (Card Ten Hearts)
           , dealerDealtCard (Card Six Spades)
           , playerDealtCard (Card Queen Diamonds)
           , dealerDealtCard (Card King Clubs)
           , playerStands
           , gameEvent (BoutDealerRevealed testBoutId)
           , gameEvent (BoutDealerHit testBoutId)
           , dealerDealtCard (Card Four Hearts)
           , gameEvent (BoutDealerStood testBoutId)
           , gameEvent (BoutSettled testBoutId pushOutcome)
           ]

dealerBlackjackGameEvents :: [SimulationEvent]
dealerBlackjackGameEvents =
    emptyHandsGameSetupMixedEvents
        ++ [ playerDealtCard (Card Ten Hearts)
           , dealerDealtCard (Card Ace Spades)
           , playerDealtCard (Card Nine Diamonds)
           , dealerDealtCard (Card King Clubs)
           , gameEvent (BoutDealerRevealed testBoutId)
           , gameEvent (BoutSettled testBoutId dealerWinsBlackjack)
           ]

playerBustGameEvents :: [SimulationEvent]
playerBustGameEvents =
    basicGameSetupMixedEvents
        ++ [ playerDealtCard (Card Ten Hearts)
           , dealerDealtCard (Card Six Spades)
           , playerDealtCard (Card Eight Diamonds)
           , dealerDealtCard (Card King Clubs)
           , playerHits
           , playerDealtCard (Card Seven Hearts)
           , gameEvent (BoutSettled testBoutId dealerWinsBoutPlayerBust)
           ]

withFixtureJsonFile :: String -> BSL.ByteString -> (FilePath -> IO a) -> IO a
withFixtureJsonFile filename content action = do
    let fixturesDir = "test/fixtures"
        filePath = fixturesDir ++ "/" ++ filename
    createDirectoryIfMissing True fixturesDir
    BSL.writeFile filePath content
    action filePath

verifyDealerBlackjackState :: SimState -> Expectation
verifyDealerBlackjackState state = do
    shouldHaveDealerHandScore state testBoutId 21
    shouldHaveDealerBlackjack state testBoutId
