{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Spec.Pitboss.Simulation.IntentToEventSpec where

import Control.Monad.Reader
import Data.HashMap.Strict.InsOrd qualified as IHM
import System.Random (mkStdGen)
import Test.Hspec

import Pitboss.Blackjack
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Spec.Pitboss.Helpers

spec :: Spec
spec = describe "Intent to Event" $ do
    describe "Player Hand Intent Generation" $ do
        it "generates Surrender intent for 16 vs 10 (correct basic strategy)" $ do
            let playerId = EntityId 100
                handId = EntityId 400
                boutId = EntityId 300
                dealerHandId = EntityId 500
                tableId = EntityId 700

            let playerHand =
                    (mkTestPlayerHand handId (EntityId 0) (EntityId 0) playerId boutId)
                        { _phAttrs =
                            PlayerHandAttrs
                                { _phAttrsHand = characterize [Card Ten Hearts, Card Six Spades]
                                , _phAttrsOriginalBet = Chips 100
                                , _phAttrsSplitDepth = 0
                                , _phAttrsHandIx = 0
                                }
                        , _phModes = PlayerHandModes (SomePlayerHandFSM PHDecisionFSM)
                        }

            let dealerHand =
                    (mkTestDealerHand dealerHandId (EntityId 0) (EntityId 0))
                        { _dhAttrs =
                            DealerHandAttrs
                                { _dhAttrsHand = characterize [Card Ten Diamonds]
                                }
                        }

            let bout = mkTestBout boutId handId dealerHandId (EntityId 0) tableId (EntityId 0)

            let table =
                    ETable
                        { _tAttrs =
                            TableAttrs
                                { _tAttrsName = "Test Table"
                                , _tAttrsCurrentRound = Nothing
                                , _tAttrsOffering = vegas6
                                }
                        , _tModes = TableModes (SomeTableFSM TRoundInProgressFSM)
                        , _tRels = TableRels Nothing
                        }

            playerArchetype <- mkTestBasicStrategy

            let tick = Tick 1000
                cache =
                    (mkTickCache tick)
                        { _cachePlayerHand = IHM.singleton handId playerHand
                        , _cacheBout = IHM.singleton boutId bout
                        , _cacheDealerHand = IHM.singleton dealerHandId dealerHand
                        , _cacheTable = IHM.singleton tableId table
                        , _cachePlayer = IHM.empty
                        , _cacheDealer = IHM.empty
                        , _cacheDealerRound = IHM.empty
                        , _cachePlayerSpot = IHM.empty
                        , _cacheTableShoe = IHM.empty
                        , _cacheIntent = IHM.empty
                        , _cacheEvent = IHM.empty
                        , _cacheTick = tick
                        }

            let gen = mkStdGen 42
                maybeEvent =
                    withTickCache cache $
                        generatePlayerHandIntent playerArchetype playerId handId gen

            case maybeEvent of
                Just (PlayerSurrender pid hid) -> do
                    pid `shouldBe` playerId
                    hid `shouldBe` handId
                Nothing -> expectationFailure "Expected event but got Nothing"
                Just other -> expectationFailure $ "Got unexpected event: " ++ show other

        it "generates Hit intent for low hand without surrender option" $ do
            let playerId = EntityId 100
                handId = EntityId 401
                boutId = EntityId 301
                dealerHandId = EntityId 501
                tableId = EntityId 701

            let playerHand =
                    (mkTestPlayerHand handId (EntityId 0) (EntityId 0) playerId boutId)
                        { _phAttrs =
                            PlayerHandAttrs
                                { _phAttrsHand = characterize [Card Five Hearts, Card Seven Spades]
                                , _phAttrsOriginalBet = Chips 100
                                , _phAttrsSplitDepth = 0
                                , _phAttrsHandIx = 0
                                }
                        , _phModes = PlayerHandModes (SomePlayerHandFSM PHDecisionFSM)
                        }

            let dealerHand =
                    (mkTestDealerHand dealerHandId (EntityId 0) (EntityId 0))
                        { _dhAttrs =
                            DealerHandAttrs
                                { _dhAttrsHand = characterize [Card Three Diamonds]
                                }
                        }

            let bout = mkTestBout boutId handId dealerHandId (EntityId 0) tableId (EntityId 0)

            let table =
                    ETable
                        { _tAttrs =
                            TableAttrs
                                { _tAttrsName = "Test Table"
                                , _tAttrsCurrentRound = Nothing
                                , _tAttrsOffering = vegas6
                                }
                        , _tModes = TableModes (SomeTableFSM TRoundInProgressFSM)
                        , _tRels = TableRels Nothing
                        }

            playerArchetype <- mkTestBasicStrategy

            let tick = Tick 1000
                cache =
                    (mkTickCache tick)
                        { _cachePlayerHand = IHM.singleton handId playerHand
                        , _cacheBout = IHM.singleton boutId bout
                        , _cacheDealerHand = IHM.singleton dealerHandId dealerHand
                        , _cacheTable = IHM.singleton tableId table
                        , _cachePlayer = IHM.empty
                        , _cacheDealer = IHM.empty
                        , _cacheDealerRound = IHM.empty
                        , _cachePlayerSpot = IHM.empty
                        , _cacheTableShoe = IHM.empty
                        , _cacheIntent = IHM.empty
                        , _cacheEvent = IHM.empty
                        , _cacheTick = tick
                        }

            let gen = mkStdGen 42
                maybeEvent =
                    withTickCache cache $
                        generatePlayerHandIntent playerArchetype playerId handId gen

            case maybeEvent of
                Just (PlayerHit pid hid) -> do
                    pid `shouldBe` playerId
                    hid `shouldBe` handId
                Nothing -> expectationFailure "Expected PlayerHit event"
                Just other -> expectationFailure $ "Expected PlayerHit but got: " ++ show other

        it "generates Stand intent for high hand" $ do
            let playerId = EntityId 102
                handId = EntityId 402
                boutId = EntityId 302
                dealerHandId = EntityId 502
                tableId = EntityId 702

            let playerHand =
                    (mkTestPlayerHand handId (EntityId 0) (EntityId 0) playerId boutId)
                        { _phAttrs =
                            PlayerHandAttrs
                                { _phAttrsHand = characterize [Card King Hearts, Card Nine Spades]
                                , _phAttrsOriginalBet = Chips 100
                                , _phAttrsSplitDepth = 0
                                , _phAttrsHandIx = 0
                                }
                        , _phModes = PlayerHandModes (SomePlayerHandFSM PHDecisionFSM)
                        }

            let dealerHand =
                    (mkTestDealerHand dealerHandId (EntityId 0) (EntityId 0))
                        { _dhAttrs =
                            DealerHandAttrs
                                { _dhAttrsHand = characterize [Card Six Diamonds]
                                }
                        }

            let bout = mkTestBout boutId handId dealerHandId (EntityId 0) tableId (EntityId 0)

            let table =
                    ETable
                        { _tAttrs =
                            TableAttrs
                                { _tAttrsName = "Test Table"
                                , _tAttrsCurrentRound = Nothing
                                , _tAttrsOffering = vegas6
                                }
                        , _tModes = TableModes (SomeTableFSM TRoundInProgressFSM)
                        , _tRels = TableRels Nothing
                        }

            playerArchetype <- mkTestBasicStrategy

            let tick = Tick 1000
                cache =
                    (mkTickCache tick)
                        { _cachePlayerHand = IHM.singleton handId playerHand
                        , _cacheBout = IHM.singleton boutId bout
                        , _cacheDealerHand = IHM.singleton dealerHandId dealerHand
                        , _cacheTable = IHM.singleton tableId table
                        , _cachePlayer = IHM.empty
                        , _cacheDealer = IHM.empty
                        , _cacheDealerRound = IHM.empty
                        , _cachePlayerSpot = IHM.empty
                        , _cacheTableShoe = IHM.empty
                        , _cacheIntent = IHM.empty
                        , _cacheEvent = IHM.empty
                        , _cacheTick = tick
                        }

            let gen = mkStdGen 43
                maybeEvent =
                    withTickCache cache $
                        generatePlayerHandIntent playerArchetype playerId handId gen

            case maybeEvent of
                Just (PlayerStood pid hid) -> do
                    pid `shouldBe` playerId
                    hid `shouldBe` handId
                Nothing -> expectationFailure "Expected PlayerStood event"
                Just other -> expectationFailure $ "Expected PlayerStood but got: " ++ show other

        it "generates Double intent when appropriate" $ do
            let playerId = EntityId 103
                handId = EntityId 403
                boutId = EntityId 303
                dealerHandId = EntityId 503
                tableId = EntityId 703

            let playerHand =
                    (mkTestPlayerHand handId (EntityId 0) (EntityId 0) playerId boutId)
                        { _phAttrs =
                            PlayerHandAttrs
                                { _phAttrsHand = characterize [Card Six Hearts, Card Five Spades]
                                , _phAttrsOriginalBet = Chips 100
                                , _phAttrsSplitDepth = 0
                                , _phAttrsHandIx = 0
                                }
                        , _phModes = PlayerHandModes (SomePlayerHandFSM PHDecisionFSM)
                        }

            let dealerHand =
                    (mkTestDealerHand dealerHandId (EntityId 0) (EntityId 0))
                        { _dhAttrs =
                            DealerHandAttrs
                                { _dhAttrsHand = characterize [Card Six Diamonds]
                                }
                        }

            let bout = mkTestBout boutId handId dealerHandId (EntityId 0) tableId (EntityId 0)

            let table =
                    ETable
                        { _tAttrs =
                            TableAttrs
                                { _tAttrsName = "Test Table"
                                , _tAttrsCurrentRound = Nothing
                                , _tAttrsOffering = vegas6
                                }
                        , _tModes = TableModes (SomeTableFSM TRoundInProgressFSM)
                        , _tRels = TableRels Nothing
                        }

            playerArchetype <- mkTestBasicStrategy

            let tick = Tick 1000
                cache =
                    (mkTickCache tick)
                        { _cachePlayerHand = IHM.singleton handId playerHand
                        , _cacheBout = IHM.singleton boutId bout
                        , _cacheDealerHand = IHM.singleton dealerHandId dealerHand
                        , _cacheTable = IHM.singleton tableId table
                        , _cachePlayer = IHM.empty
                        , _cacheDealer = IHM.empty
                        , _cacheDealerRound = IHM.empty
                        , _cachePlayerSpot = IHM.empty
                        , _cacheTableShoe = IHM.empty
                        , _cacheIntent = IHM.empty
                        , _cacheEvent = IHM.empty
                        , _cacheTick = tick
                        }

            let gen = mkStdGen 44
                maybeEvent =
                    withTickCache cache $
                        generatePlayerHandIntent playerArchetype playerId handId gen

            case maybeEvent of
                Just (PlayerDoubledDown pid hid) -> do
                    pid `shouldBe` playerId
                    hid `shouldBe` handId
                Nothing -> expectationFailure "Expected PlayerDoubledDown event"
                Just other -> expectationFailure $ "Expected PlayerDoubledDown but got: " ++ show other

        it "respects hand state - no intent for resolved hands" $ do
            let playerId = EntityId 104
                handId = EntityId 404
                boutId = EntityId 304

            let playerHand =
                    (mkTestPlayerHand handId (EntityId 0) (EntityId 0) playerId boutId)
                        { _phAttrs =
                            PlayerHandAttrs
                                { _phAttrsHand = characterize [Card King Hearts, Card Nine Spades]
                                , _phAttrsOriginalBet = Chips 100
                                , _phAttrsSplitDepth = 0
                                , _phAttrsHandIx = 0
                                }
                        , _phModes = PlayerHandModes (SomePlayerHandFSM (PHResolvedFSM PHStand))
                        }

            playerArchetype <- mkTestBasicStrategy

            let tick = Tick 1000
                cache =
                    (mkTickCache tick)
                        { _cachePlayerHand = IHM.singleton handId playerHand
                        , _cachePlayer = IHM.empty
                        , _cacheBout = IHM.empty
                        , _cacheDealerHand = IHM.empty
                        , _cacheTable = IHM.empty
                        , _cacheDealer = IHM.empty
                        , _cacheDealerRound = IHM.empty
                        , _cachePlayerSpot = IHM.empty
                        , _cacheTableShoe = IHM.empty
                        , _cacheIntent = IHM.empty
                        , _cacheEvent = IHM.empty
                        , _cacheTick = tick
                        }

            let gen = mkStdGen 45
                maybeEvent =
                    withTickCache cache $
                        generatePlayerHandIntent playerArchetype playerId handId gen

            maybeEvent `shouldBe` Nothing

    describe "Intent Validation" $ do
        it "validates Hit intent when legal" $ do
            let ctx =
                    PlayerHitCtx
                        { phICtxPlayerHand = characterize [Card Ten Hearts, Card Six Spades]
                        , phICtxPlayerHandFSM = SomePlayerHandFSM PHDecisionFSM
                        , phICtxIsPlayersTurn = True
                        , phICtxShoeHasCards = True
                        }

            validate ctx `shouldBe` Right ()

        it "rejects Hit intent when not player's turn" $ do
            let ctx =
                    PlayerHitCtx
                        { phICtxPlayerHand = characterize [Card Ten Hearts, Card Six Spades]
                        , phICtxPlayerHandFSM = SomePlayerHandFSM PHDecisionFSM
                        , phICtxIsPlayersTurn = False
                        , phICtxShoeHasCards = True
                        }

            case validate ctx of
                Left msg -> msg `shouldBe` "Not player's turn"
                Right _ -> expectationFailure "Expected validation to fail"

        it "rejects Hit intent for busted hand" $ do
            let bustedHand = characterize [Card Ten Hearts, Card Six Spades, Card King Clubs]

            case bustedHand of
                SomeHand h -> case witness h of
                    BustWitness -> pure ()
                    other -> expectationFailure $ "Expected BustWitness but got: " ++ show other

            let ctx =
                    PlayerHitCtx
                        { phICtxPlayerHand = bustedHand
                        , phICtxPlayerHandFSM = SomePlayerHandFSM PHHittingFSM
                        , phICtxIsPlayersTurn = True
                        , phICtxShoeHasCards = True
                        }

            case validate ctx of
                Left msg -> msg `shouldBe` "Cannot hit on busted hand"
                Right _ -> expectationFailure "Expected validation to fail"
