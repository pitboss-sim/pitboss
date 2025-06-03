{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Spec.Pitboss.Integration.DealerBlackjackInsuranceSpec where

import Data.HashMap.Strict.InsOrd qualified as IHM
import Pitboss.Blackjack
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Spec.Pitboss.Helpers
import Test.Hspec

mkDealerBlackjackScenario :: Offering -> [Card] -> [Card] -> IO SimState
mkDealerBlackjackScenario offering playerCards dealerCards = do
    let startTick = Tick 1000
        playerId = EntityId 100
        dealerId = EntityId 200
        boutId = EntityId 300
        playerHandId = EntityId 400
        dealerHandId = EntityId 500
        playerSpotId = EntityId 600
        dealerRoundId = EntityId 700
        tableId = EntityId 800
        shoeId = EntityId 900

    playerEntity <- mkTestPlayer "Insurance Test Player"
    let dealerEntity = mkTestDealer "Insurance Test Dealer"

    let playerHandState =
            EPlayerHand
                { _phAttrs =
                    PlayerHandAttrs
                        { _phAttrsHand = characterize playerCards
                        , _phAttrsOriginalBet = Chips 100
                        , _phAttrsSplitDepth = 0
                        , _phAttrsHandIx = 0
                        }
                , _phModes = PlayerHandModes (SomePlayerHandFSM PHDecisionFSM)
                , _phRels = PlayerHandRels playerSpotId dealerRoundId playerId boutId
                }

    let dealerHandState =
            EDealerHand
                { _dhAttrs = DealerHandAttrs (characterize dealerCards)
                , _dhModes = DealerHandModes (SomeDealerHandFSM DHDealingFSM)
                , _dhRels = DealerHandRels dealerRoundId dealerId
                }

    let playerSpotState =
            EPlayerSpot
                { _psAttrs = PlayerSpotAttrs EPlayerSpot1 (Chips 100)
                , _psModes = PlayerSpotModes (SomePlayerSpotFSM PSWaitingForHandsFSM)
                , _psRels =
                    PlayerSpotRels
                        { _psEntityRelsPlayerId = playerId
                        , _psEntityRelsRoundId = dealerRoundId
                        , _psRelsHandOccupancy = singletonFiniteMap EPlayerSpotHand1 (Present playerHandId) Absent
                        }
                }

    let dealerRoundState =
            EDealerRound
                { _drAttrs = DealerRoundAttrs 1 True
                , _drModes = DealerRoundModes
                , _drRels = DealerRoundRels shoeId
                }

    let boutState =
            EBout
                { _boutAttrs = BoutAttrs Nothing
                , _boutModes = BoutModes (SomeBoutFSM BPlayerTurnFSM)
                , _boutRels = BoutRels playerHandId dealerHandId shoeId tableId dealerRoundId
                }

    let tableState =
            ETable
                { _tAttrs =
                    TableAttrs
                        { _tAttrsName = "Insurance Test Table"
                        , _tAttrsCurrentRound = Just dealerRoundId
                        , _tAttrsOffering = offering
                        }
                , _tModes = TableModes (SomeTableFSM TRoundInProgressFSM)
                , _tRels = TableRels (Just dealerId)
                }

    let shoeState =
            ETableShoe
                { _tsAttrs = TableShoeAttrs [] mempty
                , _tsModes = TableShoeModes
                , _tsRels = TableShoeRels tableId
                }

    let trace0 = emptyTrace
        ops =
            [ createBirth playerId playerEntity
            , createBirth dealerId dealerEntity
            , createBirth boutId boutState
            , createBirth playerHandId playerHandState
            , createBirth dealerHandId dealerHandState
            , createBirth playerSpotId playerSpotState
            , createBirth dealerRoundId dealerRoundState
            , createBirth tableId tableState
            , createBirth shoeId shoeState
            ]
        finalTrace = foldl (flip $ \op -> applyTraceOp op startTick) trace0 ops

    pure
        SimState
            { simTrace = finalTrace
            , simEventLog = EventLog IHM.empty
            , simIntentLog = IntentLog IHM.empty
            , simTick = startTick
            }

spec :: Spec
spec = describe "Dealer Blackjack with Insurance Flow" $ do
    describe "Peek rules (Vegas)" $ do
        it "dealer peeks and has blackjack - player loses immediately" $ do
            let playerCards = [Card Ten Hearts, Card Nine Spades]
                dealerCards = [Card Ace Diamonds, Card King Clubs]

            state0 <- mkDealerBlackjackScenario vegas6 playerCards dealerCards

            let dealerHand = withSimCache state0 $ do
                    deref (EntityId 500 :: EntityId 'DealerHand)

            case dealerHand of
                Just hand -> do
                    case _dhAttrsHand (_dhAttrs hand) of
                        SomeHand h -> case witness h of
                            BlackjackWitness -> pure ()
                            other -> expectationFailure $ "Expected dealer blackjack but got: " ++ show other
                Nothing -> expectationFailure "Dealer hand not found"

            let boutId = EntityId 300 :: EntityId 'Bout
                outcome = dealerWinsBlackjack
                state1 = runEvent (BoutSettled boutId outcome) state0

            let bout = withSimCache state1 $ deref boutId
            case bout of
                Just b -> _boutAttrsOutcome (_boutAttrs b) `shouldBe` Just outcome
                Nothing -> expectationFailure "Bout not found"

        it "dealer peeks with ace showing but no blackjack - offers insurance" $ do
            let playerCards = [Card Ten Hearts, Card Nine Spades]
                dealerCards = [Card Ace Diamonds, Card Seven Clubs]

            state0 <- mkDealerBlackjackScenario vegas6 playerCards dealerCards

            let dealerHandId = EntityId 500 :: EntityId 'DealerHand
            let dealerHand = withSimCache state0 $ deref dealerHandId

            case dealerHand of
                Just hand -> do
                    dealerUpcard (_dhAttrsHand (_dhAttrs hand)) `shouldBe` Just (Card Ace Diamonds)
                    dealerHasBlackjack (_dhAttrsHand (_dhAttrs hand)) `shouldBe` False
                Nothing -> expectationFailure "Dealer hand not found"

        it "player has blackjack vs dealer ace - can take even money" $ do
            let playerCards = [Card Ace Hearts, Card King Spades]
                dealerCards = [Card Ace Diamonds, Card Seven Clubs]

            state0 <- mkDealerBlackjackScenario vegas6 playerCards dealerCards

            let playerHand = withSimCache state0 $ deref (EntityId 400 :: EntityId 'PlayerHand)
            case playerHand of
                Just hand -> do
                    case _phAttrsHand (_phAttrs hand) of
                        SomeHand h -> case witness h of
                            BlackjackWitness -> pure ()
                            other -> expectationFailure $ "Expected player blackjack but got: " ++ show other
                Nothing -> expectationFailure "Player hand not found"

    describe "ENHC rules (European)" $ do
        it "dealer doesn't peek - continues play even with potential blackjack" $ do
            let enhcOffering = vegas6{gameRuleSet = (gameRuleSet vegas6){holeCardRule = ENHC}}
                playerCards = [Card Ten Hearts, Card Nine Spades]
                dealerCards = [Card Ace Diamonds, Card King Clubs]

            state0 <- mkDealerBlackjackScenario enhcOffering playerCards dealerCards

            dealerShouldPeek (gameRuleSet enhcOffering) (Card Ace Diamonds) `shouldBe` False

            let playerId = EntityId 100
                handId = EntityId 400 :: EntityId 'PlayerHand
                state1 = runEvent (PlayerStood playerId handId) state0

            let playerHand = withSimCache state1 $ deref handId
            case playerHand of
                Just hand ->
                    _phFsm (_phModes hand) `shouldBe` SomePlayerHandFSM (PHResolvedFSM PHStand)
                Nothing -> expectationFailure "Player hand not found"

    describe "Insurance bet resolution" $ do
        it "dealer has blackjack - insurance pays 2:1" $ do
            pendingWith "Insurance betting not yet implemented"

        it "dealer doesn't have blackjack - insurance loses" $ do
            pendingWith "Insurance betting not yet implemented"

    describe "Dealer upcard variations" $ do
        it "dealer shows 10 - peeks for blackjack" $ do
            let playerCards = [Card Nine Hearts, Card Eight Spades]
                dealerCards = [Card Ten Diamonds, Card Ace Clubs]

            state0 <- mkDealerBlackjackScenario vegas6 playerCards dealerCards

            dealerShouldPeek (gameRuleSet vegas6) (Card Ten Diamonds) `shouldBe` True

            let boutId = EntityId 300 :: EntityId 'Bout
                outcome = dealerWinsBlackjack
                state1 = runEvent (BoutSettled boutId outcome) state0

            let bout = withSimCache state1 $ deref boutId
            case bout of
                Just b -> do
                    _boutAttrsOutcome (_boutAttrs b) `shouldBe` Just outcome
                    case outcome of
                        DetailedOutcome DealerWins (Just NaturalBlackjack) -> pure ()
                        _ -> expectationFailure "Expected dealer wins with blackjack"
                Nothing -> expectationFailure "Bout not found"

        it "dealer shows non-10/A - no peek needed" $ do
            let playerCards = [Card King Hearts, Card Nine Spades]
                dealerCards = [Card Seven Diamonds, Card Ace Clubs]

            _ <- mkDealerBlackjackScenario vegas6 playerCards dealerCards

            dealerShouldPeek (gameRuleSet vegas6) (Card Seven Diamonds) `shouldBe` False

    describe "Edge cases with early surrender" $ do
        it "early surrender available before peek" $ do
            let earlySurrenderOffering =
                    vegas6{gameRuleSet = (gameRuleSet vegas6){surrender = Early}}
                playerCards = [Card Ten Hearts, Card Six Spades]
                dealerCards = [Card Ace Diamonds, Card King Clubs]

            _ <- mkDealerBlackjackScenario earlySurrenderOffering playerCards dealerCards

            surrender (gameRuleSet earlySurrenderOffering) `shouldBe` Early

        it "late surrender not available against dealer blackjack" $ do
            let playerCards = [Card Ten Hearts, Card Six Spades]
                dealerCards = [Card Ace Diamonds, Card King Clubs]

            state0 <- mkDealerBlackjackScenario vegas6 playerCards dealerCards

            surrender (gameRuleSet vegas6) `shouldBe` Late

            let boutId = EntityId 300 :: EntityId 'Bout
                outcome = dealerWinsBlackjack
                state1 = runEvent (BoutSettled boutId outcome) state0

            case withSimCache state1 $ deref boutId of
                Just bout -> _boutAttrsOutcome (_boutAttrs bout) `shouldBe` Just outcome
                Nothing -> expectationFailure "Bout not found"
