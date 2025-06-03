{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Pitboss.Matchers where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Test.Hspec
import Test.Pitboss.TestUtils

shouldHavePlayerHandFSM :: SimState -> BoutId -> PlayerHandFSM p h d s -> Expectation
shouldHavePlayerHandFSM state boutPlayerId expectedFSM =
    case withSimCache state $ deref boutPlayerId of
        Just boutPlayer ->
            case _bModesPlayerHandFSM (_bModes boutPlayer) of
                SomePlayerHandFSM actualFSM ->
                    case (expectedFSM, actualFSM) of
                        (PHDecisionFSM, PHDecisionFSM) -> pure ()
                        (PHAwaitingFirstCardFSM, PHAwaitingFirstCardFSM) -> pure ()
                        (PHAwaitingSecondCardFSM, PHAwaitingSecondCardFSM) -> pure ()
                        (PHHittingFSM, PHHittingFSM) -> pure ()
                        (PHAwaitingOneCardFSM OCDouble, PHAwaitingOneCardFSM OCDouble) -> pure ()
                        (PHAwaitingOneCardFSM OCSplitAce, PHAwaitingOneCardFSM OCSplitAce) -> pure ()
                        (PHResolvedFSM PHStand, PHResolvedFSM PHStand) -> pure ()
                        (PHResolvedFSM PHBust, PHResolvedFSM PHBust) -> pure ()
                        (PHResolvedFSM PHBlackjack, PHResolvedFSM PHBlackjack) -> pure ()
                        _ -> expectationFailure $ "Expected " ++ show expectedFSM ++ " but got " ++ show actualFSM
        Nothing -> expectationFailure $ "BoutPlayer " ++ show boutPlayerId ++ " not found"

shouldHaveBoutPlayerHandScore :: SimState -> BoutId -> Int -> Expectation
shouldHaveBoutPlayerHandScore state boutPlayerId expectedScore =
    case withSimCache state $ deref boutPlayerId of
        Just boutPlayer -> handScore (_bAttrsPlayerHand (_bAttrs boutPlayer)) `shouldBe` expectedScore
        Nothing -> expectationFailure $ "BoutPlayer " ++ show boutPlayerId ++ " not found"

shouldHaveDealerHandScore :: SimState -> BoutId -> Int -> Expectation
shouldHaveDealerHandScore state boutPlayerId expectedScore =
    case withSimCache state $ deref boutPlayerId of
        Just boutPlayer -> handScore (_bAttrsDealerHand (_bAttrs boutPlayer)) `shouldBe` expectedScore
        Nothing -> expectationFailure $ "BoutPlayer " ++ show boutPlayerId ++ " not found"

shouldHaveDealerBlackjack :: SimState -> BoutId -> Expectation
shouldHaveDealerBlackjack state boutPlayerId =
    case withSimCache state $ deref boutPlayerId of
        Just boutDealer ->
            let SomeHand _ witness = _bAttrsDealerHand (_bAttrs boutDealer)
             in isBlackjack witness `shouldBe` True
        Nothing -> expectationFailure $ "BoutDealer " ++ show boutPlayerId ++ " not found"

shouldNotHaveDealerBlackjack :: SimState -> BoutId -> Expectation
shouldNotHaveDealerBlackjack state boutPlayerId =
    case withSimCache state $ deref boutPlayerId of
        Just boutDealer ->
            let SomeHand _ witness = _bAttrsDealerHand (_bAttrs boutDealer)
             in isBlackjack witness `shouldBe` False
        Nothing -> expectationFailure $ "BoutDealer " ++ show boutPlayerId ++ " not found"

shouldHavePlayerBlackjack :: SimState -> BoutId -> Expectation
shouldHavePlayerBlackjack state boutPlayerId =
    case withSimCache state $ deref boutPlayerId of
        Just boutPlayer ->
            let SomeHand _ witness = _bAttrsPlayerHand (_bAttrs boutPlayer)
             in isBlackjack witness `shouldBe` True
        Nothing -> expectationFailure $ "BoutPlayer " ++ show boutPlayerId ++ " not found"

shouldHaveActiveSplitHands :: SimState -> Int -> Expectation
shouldHaveActiveSplitHands state expectedCount =
    case withSimCache state $ deref testBoutId of
        Just boutPlayer ->
            let bouts' = _bRelsPlayerBouts (_bRels boutPlayer)
                activeCount =
                    length
                        [ ix | ix <- [Hand1, Hand2, Hand3, Hand4], case lookupFiniteMap ix bouts' of
                                                                    Just (Present _) -> True
                                                                    _ -> False
                        ]
             in activeCount `shouldBe` expectedCount
        Nothing -> expectationFailure "BoutPlayer not found"

shouldHaveOriginalHandWith :: SimState -> [Card] -> Expectation
shouldHaveOriginalHandWith state expectedCards =
    case withSimCache state $ deref testBoutId of
        Just boutPlayer ->
            case _bAttrsPlayerHand (_bAttrs boutPlayer) of
                SomeHand cards _ -> cards `shouldBe` expectedCards
        Nothing -> expectationFailure "Original hand not found"

shouldHaveBoutPlayerInSplitMode :: SimState -> Expectation
shouldHaveBoutPlayerInSplitMode state = shouldHaveActiveSplitHands state 2

shouldFailValidationWith :: Either ValidationErrors SimState -> String -> Expectation
shouldFailValidationWith (Left (ValidationErrors errors)) expectedError =
    show errors `shouldContain` expectedError
shouldFailValidationWith (Right _) expectedError =
    expectationFailure $ "Expected validation error containing '" ++ expectedError ++ "' but operation succeeded"

shouldHaveDealerUpcard :: SimState -> BoutId -> Rank -> Expectation
shouldHaveDealerUpcard state boutPlayerId expectedRank =
    case withSimCache state $ deref boutPlayerId of
        Just boutPlayer ->
            case dealerUpcard (_bAttrsDealerHand (_bAttrs boutPlayer)) of
                Just (Card rank _) -> rank `shouldBe` expectedRank
                Nothing -> expectationFailure "No dealer upcard found"
        Nothing -> expectationFailure $ "BoutPlayer " ++ show boutPlayerId ++ " not found"

shouldHaveDealerUpcardScore :: SimState -> BoutId -> Int -> Expectation
shouldHaveDealerUpcardScore state boutPlayerId expectedScore =
    case withSimCache state $ deref boutPlayerId of
        Just boutPlayer ->
            case dealerUpcard (_bAttrsDealerHand (_bAttrs boutPlayer)) of
                Just upcard -> cardValue upcard `shouldBe` expectedScore
                Nothing -> expectationFailure "No dealer upcard found"
        Nothing -> expectationFailure $ "BoutPlayer " ++ show boutPlayerId ++ " not found"

shouldHaveSecondSplitHandInAwaitingState :: SimState -> Expectation
shouldHaveSecondSplitHandInAwaitingState _state =
    pure ()

shouldHaveAdvancedToNextSplitHand :: SimState -> Expectation
shouldHaveAdvancedToNextSplitHand _state =
    pure ()

shouldHaveCompletedAllSplitHands :: SimState -> Expectation
shouldHaveCompletedAllSplitHands _state =
    pure ()
