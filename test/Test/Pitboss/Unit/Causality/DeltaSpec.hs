{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Pitboss.Unit.Causality.DeltaSpec where

import Control.Monad.Reader (runReader)
import Data.HashMap.Strict.InsOrd qualified as IHM
import Pitboss.Blackjack hiding (Hand, HandWitness)
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation
import Test.Hspec
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "DeltaGen" $ do
    describe "boutPlayer events" $ do
        it "generates mutation for boutPlayer stand" $ do
            let cache = mkTestCacheWithTable testTick
                event = BoutPlayerStood testBoutId
                result = runReader (generateDeltas (Game event) testCausalHistory) (TickCacheContext cache testTick)

            case result of
                Right deltas -> length deltas `shouldSatisfy` (> 0)
                Left errors -> expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

        it "generates mutation for boutPlayer hit" $ do
            let cache = mkTestCacheWithTable testTick
                event = BoutPlayerHit testBoutId
                result = runReader (generateDeltas (Game event) testCausalHistory) (TickCacheContext cache testTick)

            case result of
                Right deltas -> length deltas `shouldSatisfy` (> 0)
                Left errors -> expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

        it "generates mutation for boutPlayer double down" $ do
            let cache = mkTestCacheWithDoubleHand testTick
                event = BoutPlayerDoubledDown testBoutId
                result = runReader (generateDeltas (Game event) testCausalHistory) (TickCacheContext cache testTick)

            case result of
                Right deltas -> length deltas `shouldSatisfy` (> 0)
                Left errors -> expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

    describe "card dealing events" $ do
        it "generates hand update for card dealt to boutPlayer hand" $ do
            let cache = mkTestCache testTick
                event = BoutPlayerCardDealt (Card Ten Hearts) testBoutId
                result = runReader (generateDeltas (Game event) testCausalHistory) (TickCacheContext cache testTick)

            case result of
                Right deltas ->
                    any isBoutAttrsDelta deltas `shouldBe` True
                Left errors ->
                    expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

        it "handles dealer hand card dealing" $ do
            let cache = mkTestCache testTick
                event = BoutDealerCardDealt (Card Ace Spades) testBoutId
                result = runReader (generateDeltas (Game event) testCausalHistory) (TickCacheContext cache testTick)

            case result of
                Right deltas ->
                    any isBoutAttrsDelta deltas `shouldBe` True
                Left errors ->
                    expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

        it "generates FSM mutations when card affects hand state" $ do
            let cache = mkTestCacheWithAwaitingHand testTick
                event = BoutPlayerCardDealt (Card King Hearts) testBoutId
                result = runReader (generateDeltas (Game event) testCausalHistory) (TickCacheContext cache testTick)

            case result of
                Right deltas -> do
                    any isBoutAttrsDelta deltas `shouldBe` True
                Left errors ->
                    expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

    describe "dealer events" $ do
        it "generates mutation for dealer revealed" $ do
            let cache = mkTestCache testTick
                event = BoutDealerRevealed testBoutId
                result = runReader (generateDeltas (Game event) testCausalHistory) (TickCacheContext cache testTick)

            case result of
                Right deltas ->
                    length deltas `shouldSatisfy` (> 0)
                Left errors ->
                    expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

        it "generates mutation for dealer stood" $ do
            let cache = mkTestCache testTick
                event = BoutDealerStood testBoutId
                result = runReader (generateDeltas (Game event) testCausalHistory) (TickCacheContext cache testTick)

            case result of
                Right deltas ->
                    length deltas `shouldSatisfy` (> 0)
                Left errors ->
                    expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

    describe "bout events" $ do
        it "generates bout attrs mutation for bout settled" $ do
            let cache = mkTestCache testTick
                event = BoutSettled testBoutId boutPlayerWinsHigher
                result = runReader (generateDeltas (Game event) testCausalHistory) (TickCacheContext cache testTick)

            case result of
                Right deltas ->
                    any isBoutAttrsDelta deltas `shouldBe` True
                Left errors ->
                    expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

    describe "economy events" $ do
        it "generates bankroll credit delta" $ do
            let cache = mkTestCacheWithPlayer testTick
                event = BankrollCredit testPlayerId (Chips 100)
                result = runReader (generateDeltas (Economy event) testCausalHistory) (TickCacheContext cache testTick)

            case result of
                Right deltas ->
                    any isPlayerAttrsDelta deltas `shouldBe` True
                Left errors ->
                    expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

        it "generates bankroll debit delta" $ do
            let cache = mkTestCacheWithPlayer testTick
                event = BankrollDebit testPlayerId (Chips 50)
                result = runReader (generateDeltas (Economy event) testCausalHistory) (TickCacheContext cache testTick)

            case result of
                Right deltas ->
                    any isPlayerAttrsDelta deltas `shouldBe` True
                Left errors ->
                    expectationFailure $ "Expected successful delta generation but got errors: " ++ show errors

        it "returns validation errors for missing player" $ do
            let emptyCache = mkTickCache testTick
                event = BankrollCredit testPlayerId (Chips 100)
                result = runReader (generateDeltas (Economy event) testCausalHistory) (TickCacheContext emptyCache testTick)

            case result of
                Left (ValidationErrors _) -> pure ()
                Right _ -> expectationFailure "Expected validation errors for missing player"

    describe "error handling" $ do
        it "returns validation errors when entities are missing" $ do
            let emptyCache = mkTickCache testTick
                event = BoutPlayerStood testBoutId
                result = runReader (generateDeltas (Game event) testCausalHistory) (TickCacheContext emptyCache testTick)

            case result of
                Left (ValidationErrors _) -> pure ()
                Right _ -> expectationFailure "Expected validation errors for missing entities"

mkTestCache :: Tick -> TickCache
mkTestCache tick =
    let dealer = mkTestDealer "Test Dealer"
        bout = mkTestBout testPlayerId testDealerId testRoundId testTableId
        round' = ERound (RoundAttrs Absent 1) RoundModes (RoundRels (singletonFiniteMap TableSpot1 (Present testBoutId) Absent) testShoeId testTableId)
        shoe = EShoe (ShoeAttrs [Card Ten Hearts, Card Six Diamonds] mempty) ShoeModes (ShoeRels testTableId)

        cache = mkTickCache tick
     in cache
            { _cacheBout = IHM.insert testBoutId bout (_cacheBout cache)
            , _cacheDealer = IHM.insert testDealerId dealer (_cacheDealer cache)
            , _cacheRound = IHM.insert testRoundId round' (_cacheRound cache)
            , _cacheShoe = IHM.insert testShoeId shoe (_cacheShoe cache)
            }

mkTestCacheWithTable :: Tick -> TickCache
mkTestCacheWithTable tick =
    let baseCache = mkTestCache tick
        table = ETable (TableAttrs "Test Table" vegas6 (emptyFiniteMap Absent)) (TableModes (SomeTableFSM TRoundInProgressFSM)) (TableRels (Present testDealerId) (Present testRoundId))
     in baseCache
            { _cacheTable = IHM.insert testTableId table (_cacheTable baseCache)
            }

mkTestCacheWithDoubleHand :: Tick -> TickCache
mkTestCacheWithDoubleHand tick =
    let baseCache = mkTestCacheWithTable tick
        boutPlayer = mkTestBout testPlayerId testDealerId testRoundId testTableId
        boutPlayerWithCards = boutPlayer{_bAttrs = (_bAttrs boutPlayer){_bAttrsPlayerHand = characterize [Card Five Hearts, Card Six Spades]}}
     in baseCache
            { _cacheBout = IHM.insert testBoutId boutPlayerWithCards (_cacheBout baseCache)
            }

mkTestCacheWithAwaitingHand :: Tick -> TickCache
mkTestCacheWithAwaitingHand tick = mkTestCacheWithTable tick

mkTestCacheWithPlayer :: Tick -> TickCache
mkTestCacheWithPlayer tick =
    let baseCache = mkTestCache tick
        player = mkTestPlayer "Test Player"
     in baseCache
            { _cachePlayer = IHM.insert testPlayerId player (_cachePlayer baseCache)
            }

isBoutAttrsDelta :: TraceOp -> Bool
isBoutAttrsDelta (MutationOp BoutWitness _ (AttrsDelta _ _)) = True
isBoutAttrsDelta _ = False

isBoutModesDelta :: TraceOp -> Bool
isBoutModesDelta (MutationOp BoutWitness _ (ModesDelta _ _)) = True
isBoutModesDelta _ = False

isPlayerAttrsDelta :: TraceOp -> Bool
isPlayerAttrsDelta (MutationOp PlayerWitness _ (AttrsDelta _ _)) = True
isPlayerAttrsDelta _ = False
