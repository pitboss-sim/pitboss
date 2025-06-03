{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Pitboss.Integration.DealerBlackjackSpec where

import Control.Lens ((&))
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Test.Hspec
import Test.Pitboss.TestUtils

startTick :: Tick
startTick = Tick 1000

mkBlackjackScenario :: Offering -> [Card] -> [Card] -> SimState
mkBlackjackScenario offering contestantCards dealerCards =
    let trace = mkInitialTrace startTick
        contestantHand =
            EHand
                (HandAttrs (characterize contestantCards))
                (HandModes (ContestantHandFSM (SomeContestantHandFSM CHDecisionFSM)))
                (HandRels (ContestantOwner testContestantId) testBoutId testShoeId)
        dealerHand =
            EHand
                (HandAttrs (characterize dealerCards))
                (HandModes (DealerHandFSM (SomeDealerHandFSM DHDealingFSM)))
                (HandRels (DealerOwner testDealerId) testBoutId testShoeId)
        table =
            ETable
                (TableAttrs "Test Table" offering (emptyFiniteMap Absent))
                (TableModes (SomeTableFSM TRoundInProgressFSM))
                (TableRels (Present testDealerId) (Present testRoundId))

        updatedTrace =
            trace
                & applyTraceOp
                    ( createMutation
                        testHandId
                        contestantHand
                        ( AttrsDelta
                            (CausalHistory Nothing Nothing)
                            (DHandSetHand (characterize contestantCards) (characterize []))
                        )
                    )
                    startTick
                & applyTraceOp
                    ( createMutation
                        testDealerHandId
                        dealerHand
                        ( AttrsDelta
                            (CausalHistory Nothing Nothing)
                            (DHandSetHand (characterize dealerCards) (characterize [Card Ten Diamonds]))
                        )
                    )
                    startTick
                & applyTraceOp
                    ( createMutation
                        testTableId
                        table
                        ( AttrsDelta
                            (CausalHistory Nothing Nothing)
                            (DTableSetOffering offering vegas6)
                        )
                    )
                    startTick
     in SimState updatedTrace (EventLog mempty) (IntentLog mempty) startTick

spec :: Spec
spec = describe "Dealer Blackjack Flow" $ do
    describe "Peek Rules" $ do
        it "dealer has blackjack - player loses immediately" $ do
            let state = mkBlackjackScenario vegas6 [Card Ten Hearts, Card Nine Spades] [Card Ace Diamonds, Card King Clubs]
                cache = mkCacheFromTrace (simTrace state) startTick

            let dealerHand = withTickCache cache $ deref testDealerHandId
            case dealerHand of
                Just hand -> case _hAttrsHand (_hAttrs hand) of
                    someHand -> case valueType (handWitness someHand) of
                        BlackjackWitness -> pure ()
                        other -> expectationFailure $ "Expected dealer blackjack but got: " ++ show other
                Nothing -> expectationFailure "Dealer hand not found"

            let outcome = dealerWinsBlackjack
                state1 = runEvent (BoutSettled testBoutId outcome) state
                bout = withSimCache state1 $ deref testBoutId

            case bout of
                Just b -> _bAttrsOutcome (_bAttrs b) `shouldBe` Present outcome
                Nothing -> expectationFailure "Bout not found"

        it "dealer shows ace but no blackjack - continues play" $ do
            let state = mkBlackjackScenario vegas6 [Card Ten Hearts, Card Nine Spades] [Card Ace Diamonds, Card Seven Clubs]
                cache = mkCacheFromTrace (simTrace state) startTick

            let dealerHand = withTickCache cache $ deref testDealerHandId
            case dealerHand of
                Just hand -> do
                    let hand' = _hAttrsHand (_hAttrs hand)
                    dealerUpcard hand' `shouldBe` Just (Card Ace Diamonds)
                    dealerHasBlackjack hand' `shouldBe` False
                Nothing -> expectationFailure "Dealer hand not found"

        it "player has blackjack vs dealer ace" $ do
            let state = mkBlackjackScenario vegas6 [Card Ace Hearts, Card King Spades] [Card Ace Diamonds, Card Seven Clubs]
                cache = mkCacheFromTrace (simTrace state) startTick

            let contestantHand = withTickCache cache $ deref testHandId
            case contestantHand of
                Just hand -> case _hAttrsHand (_hAttrs hand) of
                    someHand -> case valueType (handWitness someHand) of
                        BlackjackWitness -> pure ()
                        other -> expectationFailure $ "Expected player blackjack but got: " ++ show other
                Nothing -> expectationFailure "Contestant hand not found"

    describe "ENHC vs Peek" $ do
        it "ENHC rules prevent peeking" $ do
            let enhcOffering = vegas6{gameRuleSet = (gameRuleSet vegas6){holeCardRule = ENHC}}
            dealerShouldPeek (gameRuleSet enhcOffering) (Card Ace Diamonds) `shouldBe` False

        it "Peek rules allow peeking on ace" $ do
            dealerShouldPeek (gameRuleSet vegas6) (Card Ace Diamonds) `shouldBe` True

        it "Peek rules allow peeking on ten" $ do
            dealerShouldPeek (gameRuleSet vegas6) (Card Ten Diamonds) `shouldBe` True

        it "No peek needed on low cards" $ do
            dealerShouldPeek (gameRuleSet vegas6) (Card Seven Diamonds) `shouldBe` False

    describe "Bout Resolution" $ do
        it "dealer blackjack beats player 19" $ do
            let state = mkBlackjackScenario vegas6 [Card Ten Hearts, Card Nine Spades] [Card Ace Diamonds, Card King Clubs]
                outcome = dealerWinsBlackjack
                state1 = runEvent (BoutSettled testBoutId outcome) state
                bout = withSimCache state1 $ deref testBoutId

            case bout of
                Just b -> case _bAttrsOutcome (_bAttrs b) of
                    Present (DetailedOutcome DealerWin (Just NaturalBlackjack)) -> pure ()
                    other -> expectationFailure $ "Expected dealer wins with blackjack but got: " ++ show other
                Nothing -> expectationFailure "Bout not found"

        it "player blackjack vs dealer non-blackjack" $ do
            let state = mkBlackjackScenario vegas6 [Card Ace Hearts, Card King Spades] [Card Ten Diamonds, Card Seven Clubs]
                outcome = contestantWinsBlackjack
                state1 = runEvent (BoutSettled testBoutId outcome) state
                bout = withSimCache state1 $ deref testBoutId

            case bout of
                Just b -> case _bAttrsOutcome (_bAttrs b) of
                    Present (DetailedOutcome ContestantWin (Just NaturalBlackjack)) -> pure ()
                    other -> expectationFailure $ "Expected player wins with blackjack but got: " ++ show other
                Nothing -> expectationFailure "Bout not found"
