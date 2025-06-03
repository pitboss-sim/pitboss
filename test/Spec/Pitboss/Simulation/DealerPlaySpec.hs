{-# LANGUAGE DataKinds #-}

module Spec.Pitboss.Simulation.DealerPlaySpec where

import Control.Monad.Reader
import Data.HashMap.Strict.InsOrd qualified as IHM
import Pitboss.Blackjack
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Spec.Pitboss.Helpers
import Test.Hspec

mkDealerPlayState :: GameRuleSet -> [Card] -> [Card] -> IO SimState
mkDealerPlayState rules dealerCards upcomingCards = do
    let startTick = Tick 1000
        dealerId = EntityId 200
        dealerHandId = EntityId 500
        tableId = EntityId 800
        shoeId = EntityId 900
        dealerRoundId = EntityId 700

        dealerState =
            EDealer
                { _dAttrs = DealerAttrs{_dAttrsName = "Test Dealer"}
                , _dModes =
                    DealerModes
                        { _dModesDealerTable = SomeDealerTableFSM DTOnDutyFSM
                        , _dModesDealerRound = PeekDealerRound (SomePeekFSM PeekDealingFSM)
                        , _dModesDealerHand = SomeDealerHandFSM DHDealingFSM
                        }
                , _dRels =
                    DealerRels
                        { _dRelsActiveTable = Just tableId
                        , _dRelsActiveRound = Just dealerRoundId
                        , _dRelsActiveHand = Just dealerHandId
                        }
                }

        dealerHandState =
            EDealerHand
                { _dhAttrs = DealerHandAttrs (characterize dealerCards)
                , _dhModes = DealerHandModes (SomeDealerHandFSM DHDealingFSM)
                , _dhRels = DealerHandRels dealerRoundId dealerId
                }

        tableState =
            ETable
                { _tAttrs =
                    TableAttrs
                        { _tAttrsName = "Test Table"
                        , _tAttrsCurrentRound = Nothing
                        , _tAttrsOffering =
                            mkOffering
                                (Materia D6 FaceUp)
                                rules
                                (TableRuleSet (Chips 10) (Chips 1000) 3 AllowMidShoe SameMinBet SingleCardBurn 0.75)
                        }
                , _tModes = TableModes (SomeTableFSM TRoundInProgressFSM)
                , _tRels = TableRels (Just dealerId)
                }

        shoeState =
            ETableShoe
                { _tsAttrs = TableShoeAttrs upcomingCards mempty
                , _tsModes = TableShoeModes
                , _tsRels = TableShoeRels tableId
                }

        dealerRoundState =
            EDealerRound
                { _drAttrs = DealerRoundAttrs 1 True
                , _drModes = DealerRoundModes
                , _drRels = DealerRoundRels shoeId
                }

        trace0 = emptyTrace
        trace1 = applyTraceOp (createBirth dealerId dealerState) startTick trace0
        trace2 = applyTraceOp (createBirth dealerHandId dealerHandState) startTick trace1
        trace3 = applyTraceOp (createBirth tableId tableState) startTick trace2
        trace4 = applyTraceOp (createBirth shoeId shoeState) startTick trace3
        trace5 = applyTraceOp (createBirth dealerRoundId dealerRoundState) startTick trace4

    pure
        SimState
            { simTrace = trace5
            , simEventLog = EventLog IHM.empty
            , simIntentLog = IntentLog IHM.empty
            , simTick = startTick
            }

generateDealerPlayIntent ::
    EntityId 'Dealer ->
    EntityId 'DealerHand ->
    Reader TickCacheContext (Maybe BlackjackEvent)
generateDealerPlayIntent dealerId handId = do
    maybeHand <- deref handId
    case maybeHand of
        Nothing -> pure Nothing
        Just hand -> do
            maybeTable <- deref (EntityId 800 :: EntityId 'Table)
            case maybeTable of
                Nothing -> pure Nothing
                Just table -> do
                    let rules = gameRuleSet (_tAttrsOffering (_tAttrs table))
                        currentHand = _dhAttrsHand (_dhAttrs hand)

                    if dealerShouldHit rules currentHand
                        then pure $ Just (CardDealt (Card Four Clubs) (ToDealerHand handId))
                        else pure $ Just (DealerRevealed dealerId handId)

spec :: Spec
spec = describe "Dealer Play Within Framework" $ do
    it "can create and retrieve dealer entities" $ do
        let rules = gameRuleSet vegas6
        state0 <- mkDealerPlayState rules [Card Ace Hearts] []

        let dealerId = EntityId 200 :: EntityId 'Dealer
            handId = EntityId 500 :: EntityId 'DealerHand
            tableId = EntityId 800 :: EntityId 'Table

            isJust (Just _) = True
            isJust Nothing = False

        let entities = withSimCache state0 $ do
                d <- deref dealerId
                h <- deref handId
                t <- deref tableId
                pure (isJust d, isJust h, isJust t)

        entities `shouldBe` (True, True, True)

    it "dealer hits soft 17 with H17 rules" $ do
        let h17Rules = (gameRuleSet vegas6){soft17 = HitSoft17}

        state0 <-
            mkDealerPlayState
                h17Rules
                [Card Ace Hearts, Card Six Spades]
                [Card Four Clubs]

        let soft17 = characterize [Card Ace Hearts, Card Six Spades]
        dealerShouldHit h17Rules soft17 `shouldBe` True

        let dealerId = EntityId 200
            handId = EntityId 500
            maybeIntent =
                withSimCache state0 $
                    generateDealerPlayIntent dealerId handId

            isJust (Just _) = True
            isJust Nothing = False

        case maybeIntent of
            Just (CardDealt _ (ToDealerHand hid)) ->
                hid `shouldBe` handId
            Just (DealerRevealed _ _) ->
                expectationFailure "Dealer should hit soft 17 with H17"
            Nothing -> do
                let debugInfo = withSimCache state0 $ do
                        h <- deref (handId :: EntityId 'DealerHand)
                        d <- deref (dealerId :: EntityId 'Dealer)
                        case (h, d) of
                            (_, Just dealer) -> do
                                let tableId = _dRelsActiveTable (_dRels dealer)
                                t <- case tableId of
                                    Just tid -> deref tid
                                    Nothing -> pure Nothing
                                pure (isJust h, isJust d, isJust t, tableId)
                            _ -> pure (isJust h, isJust d, False, Nothing)
                expectationFailure $ "Expected dealer play intent. Debug: " ++ show debugInfo
            _ ->
                expectationFailure "Unexpected event type"

    it "dealer stands on soft 17 with S17 rules" $ do
        let s17Rules = (gameRuleSet vegas6){soft17 = StandSoft17}

        let soft17 = characterize [Card Ace Hearts, Card Six Spades]
        dealerShouldHit s17Rules soft17 `shouldBe` False

        state0 <-
            mkDealerPlayState
                s17Rules
                [Card Ace Hearts, Card Six Spades]
                []

        let dealerId = EntityId 200
            handId = EntityId 500
            maybeIntent =
                withSimCache state0 $
                    generateDealerPlayIntent dealerId handId

        case maybeIntent of
            Just (DealerRevealed did hid) -> do
                did `shouldBe` dealerId
                hid `shouldBe` handId
            Just (CardDealt _ _) ->
                expectationFailure "Dealer should stand on soft 17 with S17"
            _ ->
                expectationFailure "Expected dealer reveal intent"

    it "tracks dealer play through events and deltas" $ do
        let h17Rules = (gameRuleSet vegas6){soft17 = HitSoft17}

        state0 <-
            mkDealerPlayState
                h17Rules
                [Card Ace Hearts, Card Six Spades]
                [Card Four Clubs]

        let handId = EntityId 500
        let state1 = runEvent (CardDealt (Card Four Clubs) (ToDealerHand handId)) state0
        let finalHand = withSimCache state1 $ deref (handId :: EntityId 'DealerHand)

        case finalHand of
            Just hand -> do
                handScore (_dhAttrsHand (_dhAttrs hand)) `shouldBe` 21
            Nothing -> expectationFailure "Dealer hand not found"
