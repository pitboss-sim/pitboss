{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pitboss.Integration.SoftHandConversionSpec where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.Simulation hiding (Push)
import Test.Hspec
import Test.Pitboss.Matchers
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Soft Hand Conversion Scenario" $ describe "Event-Driven Soft to Hard Conversion" $ do
    it "simulates A-5 hitting to A-5-5 (hard 11) then doubling" $ do
        let gameEvents = softHandConversionEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        state `shouldHaveBoutPlayerHandScore` testBoutId $ 21
        state `shouldHaveDealerHandScore` testBoutId $ 18
        verifySoftToHardConversion state

    it "verifies ace value adjustment from soft to hard" $ do
        let gameEvents = softHandConversionEvents
            state = buildSimStateFromMixedEvents gameEvents testTick

        verifySoftToHardConversion state

softHandConversionEvents :: [SimulationEvent]
softHandConversionEvents =
    emptyHandsGameSetupMixedEvents
        ++ [ playerDealtCard (Card Ace Hearts)
           , dealerDealtCard (Card Eight Spades)
           , playerDealtCard (Card Five Diamonds)
           , dealerDealtCard (Card Queen Clubs)
           , playerHits
           , playerDealtCard (Card Five Hearts)
           , playerDoubles
           , playerDealtCard (Card Ten Spades)
           , playerStands
           , gameEvent (BoutDealerRevealed testBoutId)
           , gameEvent (BoutDealerStood testBoutId)
           , gameEvent (BoutSettled testBoutId boutPlayerWinsHigher)
           ]

verifySoftToHardConversion :: SimState -> Expectation
verifySoftToHardConversion state =
    case withSimCache state $ deref testBoutId of
        Just bout -> do
            let SomeHand cards playerWitness = _bAttrsPlayerHand (_bAttrs bout)
            handScore (_bAttrsPlayerHand (_bAttrs bout)) `shouldBe` 21
            handScore (_bAttrsDealerHand (_bAttrs bout)) `shouldBe` 18
            length cards `shouldBe` 4
            valueType playerWitness `shouldBe` HardWitness
            case _bAttrsOutcome (_bAttrs bout) of
                Present outcome -> outcome `shouldSatisfy` isPlayerWinOutcome
                Absent -> expectationFailure "Expected bout outcome to be present"
        Nothing -> expectationFailure "Could not retrieve bout from cache"
  where
    isPlayerWinOutcome outcome = case outcome of
        DetailedOutcome BoutPlayerWin (Just HigherScore) -> True
        _ -> False
