{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Pitboss.Integration.BoutPlayerDecisionFlowSpec where

import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Test.Hspec
import Test.Pitboss.TestUtils

mkTestTable :: Offering -> EntityState 'Table
mkTestTable offering =
    ETable
        (TableAttrs "Test Table" offering (emptyFiniteMap Absent))
        (TableModes (SomeTableFSM TRoundInProgressFSM))
        (TableRels Absent (Present testRoundId))

spec :: Spec
spec = describe "BoutPlayer Decision Flow Integration" $ do
    describe "Basic Decision Making" $ do
        it "processes hit event correctly" $ do
            let state =
                    buildSimStateFromMixedEvents
                        [ lifecycleEvent (playerCreated testPlayerId "Test Player")
                        , lifecycleEvent (dealerCreated testDealerId "Test Dealer")
                        , lifecycleEvent (tableCreated testTableId "Test Table" vegas6)
                        , lifecycleEvent (roundCreated testRoundId testShoeId testTableId)
                        , lifecycleEvent (shoeCreated testShoeId testTableId [Card Ten Hearts, Card Six Diamonds, Card Seven Spades, Card King Clubs, Card Four Hearts])
                        , lifecycleEvent (boutCreated testBoutId testPlayerId testDealerId testRoundId testTableId)
                        , playerHandSet [Card Two Hearts, Card Three Spades]
                        , dealerHandSet [Card Six Hearts, Card Ten Clubs]
                        , gameEvent (BoutPlayerHit testBoutId)
                        ]
                        testTick

            case withSimCache state $ deref testBoutId of
                Just bout -> do
                    case _bModesPlayerHandFSM (_bModes bout) of
                        SomePlayerHandFSM PHHittingFSM -> pure ()
                        other -> expectationFailure $ "Expected PHHittingFSM but got: " ++ show other
                Nothing -> expectationFailure "Bout not found in cache"

        it "processes stand event correctly" $ do
            let state =
                    buildSimStateFromMixedEvents
                        [ lifecycleEvent (playerCreated testPlayerId "Test Player")
                        , lifecycleEvent (dealerCreated testDealerId "Test Dealer")
                        , lifecycleEvent (tableCreated testTableId "Test Table" vegas6)
                        , lifecycleEvent (roundCreated testRoundId testShoeId testTableId)
                        , lifecycleEvent (shoeCreated testShoeId testTableId [Card Ten Hearts, Card Six Diamonds, Card Seven Spades, Card King Clubs, Card Four Hearts])
                        , lifecycleEvent (boutCreated testBoutId testPlayerId testDealerId testRoundId testTableId)
                        , playerHandSet [Card Ten Hearts, Card Nine Spades]
                        , dealerHandSet [Card Six Hearts, Card Ten Clubs]
                        , gameEvent (BoutPlayerStood testBoutId)
                        ]
                        testTick

            case withSimCache state $ deref testBoutId of
                Just bout -> do
                    case _bModesPlayerHandFSM (_bModes bout) of
                        SomePlayerHandFSM (PHResolvedFSM PHStand) -> pure ()
                        other -> expectationFailure $ "Expected PHResolvedFSM PHStand but got: " ++ show other
                Nothing -> expectationFailure "Bout not found in cache"

        it "processes double down event correctly" $ do
            let state =
                    buildSimStateFromMixedEvents
                        [ lifecycleEvent (playerCreated testPlayerId "Test Player")
                        , lifecycleEvent (dealerCreated testDealerId "Test Dealer")
                        , lifecycleEvent (tableCreated testTableId "Test Table" vegas6)
                        , lifecycleEvent (roundCreated testRoundId testShoeId testTableId)
                        , lifecycleEvent (shoeCreated testShoeId testTableId [Card Ten Hearts, Card Six Diamonds, Card Seven Spades, Card King Clubs, Card Four Hearts])
                        , lifecycleEvent (boutCreated testBoutId testPlayerId testDealerId testRoundId testTableId)
                        , playerHandSet [Card Five Hearts, Card Six Spades]
                        , dealerHandSet [Card Six Hearts, Card Ten Clubs]
                        , gameEvent (BoutPlayerDoubledDown testBoutId)
                        ]
                        testTick

            case withSimCache state $ deref testBoutId of
                Just bout -> do
                    case _bModesPlayerHandFSM (_bModes bout) of
                        SomePlayerHandFSM (PHAwaitingOneCardFSM OCDouble) -> pure ()
                        other -> expectationFailure $ "Expected PHAwaitingOneCardFSM OCDouble but got: " ++ show other
                Nothing -> expectationFailure "Bout not found in cache"

mkTestBasicArchetype :: IO SomePlayerArchetype
mkTestBasicArchetype = do
    chart <- loadCanonicalStrategy
    pure $
        SomePlayerBasicStrategy $
            BasicStrategyArchetype
                (BasicConfig chart (MistakeProfile 0.0 workingMistakeDistribution))
                (BasicState 0 emptySessionStats)
