{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Pitboss.Integration.SplitHandSpec where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.FSM
import Pitboss.Simulation
import Test.Hspec
import Test.Pitboss.Matchers
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Split Hand Integration" $ do
    describe "Pair Splitting" $ do
        it "splits eights into two separate hands" $ do
            let state = mkPairSplitState [Card Eight Hearts, Card Eight Spades]
                splitState = runEvent (BoutPlayerSplit testBoutId) state

            splitState `shouldHaveActiveSplitHands` 2
            splitState `shouldHaveOriginalHandWith` [Card Eight Hearts]
            shouldHaveBoutPlayerInSplitMode splitState

        it "transitions split hands to awaiting second card" $ do
            let state = mkPairSplitState [Card Eight Hearts, Card Eight Spades]
                splitState = runEvent (BoutPlayerSplit testBoutId) state

            splitState `shouldHavePlayerHandFSM` testBoutId $ PHAwaitingSecondCardFSM
            shouldHaveSecondSplitHandInAwaitingState splitState

        it "handles split aces with one card only rule" $ do
            let state = mkPairSplitStateWithOneCardDraw [Card Ace Hearts, Card Ace Spades]
                splitState = runEvent (BoutPlayerSplit testBoutId) state

            splitState `shouldHaveActiveSplitHands` 2
            splitState `shouldHavePlayerHandFSM` testBoutId $ PHAwaitingOneCardFSM OCSplitAce

        it "rejects split of non-pair" $ do
            let state = mkNonPairState [Card Ten Hearts, Card Nine Spades]
                result = runEventStrict (BoutPlayerSplit testBoutId) state

            result `shouldFailValidationWith` "Cannot split: hand is not a pair"

    describe "Split Hand Progression" $ do
        it "deals cards to active split hand" $ do
            let state = mkPairSplitState [Card Eight Hearts, Card Eight Spades]
                splitState = runEvent (BoutPlayerSplit testBoutId) state
                cardState = runEvent (BoutPlayerCardDealt (Card King Clubs) testBoutId) splitState

            cardState `shouldHaveBoutPlayerHandScore` testBoutId $ 18

        it "advances to next split hand after completion" $ do
            let state = mkPairSplitState [Card Eight Hearts, Card Eight Spades]
                splitState = runEvent (BoutPlayerSplit testBoutId) state
                cardState = runEvent (BoutPlayerCardDealt (Card King Clubs) testBoutId) splitState
                standState = runEvent (BoutPlayerStood testBoutId) cardState

            shouldHaveAdvancedToNextSplitHand standState

        it "completes all split hands when finished" $ do
            let state = mkPairSplitState [Card Eight Hearts, Card Eight Spades]
                splitState = runEvent (BoutPlayerSplit testBoutId) state
                firstCardState = runEvent (BoutPlayerCardDealt (Card King Clubs) testBoutId) splitState
                firstStandState = runEvent (BoutPlayerStood testBoutId) firstCardState
                secondCardState = runEvent (BoutPlayerCardDealt (Card Queen Spades) testBoutId) firstStandState
                finalState = runEvent (BoutPlayerStood testBoutId) secondCardState

            shouldHaveCompletedAllSplitHands finalState

mkPairSplitState :: [Card] -> SimState
mkPairSplitState = mkNonPairState

mkPairSplitStateWithOneCardDraw :: [Card] -> SimState
mkPairSplitStateWithOneCardDraw = mkNonPairState

mkNonPairState :: [Card] -> SimState
mkNonPairState cards = buildSimStateFromMixedEvents (mkNonPairStateEvents cards) testTick

mkNonPairStateEvents :: [Card] -> [SimulationEvent]
mkNonPairStateEvents playerCards =
    [ lifecycleEvent (playerCreated testPlayerId "Test Player")
    , lifecycleEvent (dealerCreated testDealerId "Test Dealer")
    , lifecycleEvent (tableCreated testTableId "Test Table" vegas6)
    , lifecycleEvent (roundCreated testRoundId testShoeId testTableId)
    , lifecycleEvent (shoeCreated testShoeId testTableId [Card Ten Hearts, Card Six Diamonds, Card Seven Spades, Card King Clubs, Card Four Hearts])
    , lifecycleEvent (boutCreated testBoutId testPlayerId testDealerId testRoundId testTableId)
    , playerHandSet playerCards
    , dealerHandSet [Card Six Hearts, Card Queen Clubs]
    ]

mkInitialSimState :: SimState
mkInitialSimState = buildSimStateFromEvents basicGameSetupEvents testTick

mkInitialSimStateWithOneCardDraw :: SimState
mkInitialSimStateWithOneCardDraw = buildSimStateFromEvents basicGameSetupEvents testTick
