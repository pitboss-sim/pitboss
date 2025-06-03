{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Pitboss.Integration.SplitHandSpec where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.Causality.Validate
import Pitboss.FSM
import Pitboss.Simulation
import Test.Hspec
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Split Hand Integration" $ do
    describe "Pair Splitting" $ do
        it "splits eights into two separate hands" $ do
            let state = mkPairSplitState [Card Eight Hearts, Card Eight Spades]
                splitState = runEvent (ContestantSplit testContestantId testHandId) state

            splitState `shouldHaveActiveSplitHands` 2
            splitState `shouldHaveOriginalHandWith` [Card Eight Hearts]
            shouldHaveContestantInSplitMode splitState

        it "transitions split hands to awaiting second card" $ do
            let state = mkPairSplitState [Card Eight Hearts, Card Eight Spades]
                splitState = runEvent (ContestantSplit testContestantId testHandId) state

            splitState `shouldHaveHandFSM` testHandId $ CHAwaitingSecondCardFSM
            shouldHaveSecondSplitHandInAwaitingState splitState

        it "handles split aces with one card only rule" $ do
            let state = mkPairSplitState [Card Ace Hearts, Card Ace Spades]
                splitState = runEvent (ContestantSplit testContestantId testHandId) state

            splitState `shouldHaveActiveSplitHands` 2
            splitState `shouldHaveHandFSM` testHandId $ CHAwaitingOneCardFSM OCSplitAce

        it "rejects split of non-pair" $ do
            let state = mkNonPairState [Card Ten Hearts, Card Nine Spades]
                result = runEventStrict (ContestantSplit testContestantId testHandId) state

            result `shouldFailValidationWith` "Cannot split: hand is not a pair"

    describe "Split Hand Progression" $ do
        it "deals cards to active split hand" $ do
            let state = mkPairSplitState [Card Eight Hearts, Card Eight Spades]
                splitState = runEvent (ContestantSplit testContestantId testHandId) state
                cardState = runEvent (CardDealt (Card King Clubs) testHandId) splitState

            cardState `shouldHaveHandScore` testHandId $ 18
            cardState `shouldHaveHandFSM` testHandId $ CHDecisionFSM

        it "resolves split hand on bust" $ do
            let state = mkPairSplitState [Card Eight Hearts, Card Eight Spades]
                splitState = runEvent (ContestantSplit testContestantId testHandId) state
                card1State = runEvent (CardDealt (Card King Clubs) testHandId) splitState
                hitState = runEvent (ContestantHit testContestantId testHandId) card1State
                bustState = runEvent (CardDealt (Card Queen Diamonds) testHandId) hitState

            bustState `shouldHaveHandFSM` testHandId $ CHResolvedFSM CBust

        it "resolves split hand on stand" $ do
            let state = mkPairSplitState [Card Eight Hearts, Card Eight Spades]
                splitState = runEvent (ContestantSplit testContestantId testHandId) state
                card1State = runEvent (CardDealt (Card Three Clubs) testHandId) splitState
                standState = runEvent (ContestantStood testContestantId testHandId) card1State

            standState `shouldHaveHandFSM` testHandId $ CHResolvedFSM CStand
            standState `shouldHaveHandScore` testHandId $ 11

    describe "Split Hand Orchestration (pending orchestration layer)" $ do
        it "advances to next hand after completing split hand" $
            pendingWith "Requires orchestration layer for hand advancement"

        it "processes all split hands sequentially" $
            pendingWith "Requires orchestration layer for hand advancement"

        it "handles multiple splits with proper sequencing" $
            pendingWith "Requires orchestration layer for hand advancement"

-- Test setup helpers using existing TestUtils
mkPairSplitState :: [Card] -> SimState
mkPairSplitState cards =
    let baseState = SimState (mkInitialTrace testTick) (EventLog mempty) (IntentLog mempty) testTick
        cardEvents = [CardDealt card testHandId | card <- cards]
     in foldl (flip runEvent) baseState cardEvents

mkNonPairState :: [Card] -> SimState
mkNonPairState = mkPairSplitState

-- Custom assertions
shouldHaveActiveSplitHands :: SimState -> Int -> Expectation
shouldHaveActiveSplitHands state expectedCount =
    case withSimCache state $ deref testContestantId of
        Just contestant ->
            let bouts' = _cRelsBouts (_cRels contestant)
                activeCount =
                    length
                        [ ix | ix <- [Hand1, Hand2, Hand3, Hand4], case lookupFiniteMap ix bouts' of Just (Present _) -> True; _ -> False
                        ]
             in activeCount `shouldBe` expectedCount
        Nothing -> expectationFailure "Contestant not found"

shouldHaveOriginalHandWith :: SimState -> [Card] -> Expectation
shouldHaveOriginalHandWith state expectedCards =
    case withSimCache state $ deref testHandId of
        Just hand ->
            case _hAttrsHand (_hAttrs hand) of
                h -> handCards h `shouldBe` expectedCards
        Nothing -> expectationFailure "Original hand not found"

shouldHaveContestantInSplitMode :: SimState -> Expectation
shouldHaveContestantInSplitMode state = shouldHaveActiveSplitHands state 2

shouldHaveHandFSM :: SimState -> HandId -> ContestantHandFSM p h d s -> Expectation
shouldHaveHandFSM state handId expectedFSM =
    case withSimCache state $ deref handId of
        Just hand ->
            case _hModesHandFSM (_hModes hand) of
                ContestantHandFSM (SomeContestantHandFSM actualFSM) ->
                    case (expectedFSM, actualFSM) of
                        (CHDecisionFSM, CHDecisionFSM) -> pure ()
                        (CHAwaitingSecondCardFSM, CHAwaitingSecondCardFSM) -> pure ()
                        (CHAwaitingOneCardFSM OCSplitAce, CHAwaitingOneCardFSM OCSplitAce) -> pure ()
                        (CHResolvedFSM CBust, CHResolvedFSM CBust) -> pure ()
                        (CHResolvedFSM CStand, CHResolvedFSM CStand) -> pure ()
                        _ -> expectationFailure $ "Expected " ++ show expectedFSM ++ " but got " ++ show actualFSM
                other -> expectationFailure $ "Expected contestant hand FSM but got: " ++ show other
        Nothing -> expectationFailure $ "Hand " ++ show handId ++ " not found"

shouldHaveSecondSplitHandInAwaitingState :: SimState -> Expectation
shouldHaveSecondSplitHandInAwaitingState state =
    case withSimCache state $ deref testContestantId of
        Just contestant ->
            case lookupFiniteMap Hand2 (_cRelsBouts (_cRels contestant)) of
                Just (Present boutId) -> do
                    case withSimCache state $ deref boutId of
                        Just bout -> do
                            let handId = _bRelsPlayerHand (_bRels bout)
                            shouldHaveHandFSM state handId CHAwaitingSecondCardFSM
                        Nothing -> expectationFailure "Second bout not found"
                _ -> expectationFailure "Second hand not found in contestant"
        Nothing -> expectationFailure "Contestant not found"

shouldFailValidationWith :: Either ValidationErrors SimState -> String -> Expectation
shouldFailValidationWith (Left (ValidationErrors errors)) expectedError =
    show errors `shouldContain` expectedError
shouldFailValidationWith (Right _) expectedError =
    expectationFailure $ "Expected validation error containing '" ++ expectedError ++ "' but operation succeeded"

shouldHaveHandScore :: SimState -> HandId -> Int -> Expectation
shouldHaveHandScore state handId expectedScore =
    case withSimCache state $ deref handId of
        Just hand -> handScore (_hAttrsHand (_hAttrs hand)) `shouldBe` expectedScore
        Nothing -> expectationFailure $ "Hand " ++ show handId ++ " not found"
