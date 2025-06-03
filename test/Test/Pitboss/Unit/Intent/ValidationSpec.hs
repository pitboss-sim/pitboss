{-# LANGUAGE DataKinds #-}

module Test.Pitboss.Unit.Intent.ValidationSpec where

import Pitboss.Blackjack.Types.Core
import Pitboss.Causality.Types.Core
import Pitboss.Simulation.Intent.Context.Types.Core
import Pitboss.Simulation.Intent.Types
import Pitboss.Simulation.Intent.Validate
import Test.Hspec

dummyBettingSpine :: Spine BettingPlayer
dummyBettingSpine = BettingPlayerSpine undefined undefined

dummyPlayingSpine :: Spine PlayingPlayer
dummyPlayingSpine = PlayingPlayerSpine undefined undefined undefined undefined undefined

dummyDealerSpine :: Spine PlayingDealer
dummyDealerSpine = PlayingDealerSpine undefined undefined undefined undefined undefined

spec :: Spec
spec = describe "Intent Validation" $ do
    describe "BettingPlayer validation" $ do
        it "accepts valid bet within limits and funds" $ do
            let ctx = BettingPlayerCtx dummyBettingSpine (Chips 1000) (Chips 25, Chips 500)
            validateBettingPlayerIntent (PlaceBet (Chips 100)) ctx `shouldBe` Valid

        it "rejects bet below table minimum" $ do
            let ctx = BettingPlayerCtx dummyBettingSpine (Chips 1000) (Chips 25, Chips 500)
            validateBettingPlayerIntent (PlaceBet (Chips 10)) ctx `shouldBe` Invalid "Bet below table minimum"

        it "rejects bet above table maximum" $ do
            let ctx = BettingPlayerCtx dummyBettingSpine (Chips 1000) (Chips 25, Chips 500)
            validateBettingPlayerIntent (PlaceBet (Chips 600)) ctx `shouldBe` Invalid "Bet above table maximum"

        it "rejects bet exceeding available funds" $ do
            let ctx = BettingPlayerCtx dummyBettingSpine (Chips 50) (Chips 25, Chips 500)
            validateBettingPlayerIntent (PlaceBet (Chips 100)) ctx `shouldBe` Invalid "Insufficient funds"

        it "rejects non-betting intents" $ do
            let ctx = BettingPlayerCtx dummyBettingSpine (Chips 1000) (Chips 25, Chips 500)
            validateBettingPlayerIntent Hit' ctx `shouldBe` Invalid "Invalid intent for betting player"

    describe "PlayingPlayer validation" $ do
        it "accepts Hit when available" $ do
            let ctx = PlayingPlayerCtx dummyPlayingSpine [Hit, Stand] (Chips 100)
            validatePlayingPlayerIntent Hit' ctx `shouldBe` Valid

        it "rejects Hit when not available" $ do
            let ctx = PlayingPlayerCtx dummyPlayingSpine [Stand] (Chips 100)
            validatePlayingPlayerIntent Hit' ctx `shouldBe` Invalid "Hit not available"

        it "accepts Stand when available" $ do
            let ctx = PlayingPlayerCtx dummyPlayingSpine [Hit, Stand] (Chips 100)
            validatePlayingPlayerIntent Stand' ctx `shouldBe` Valid

        it "accepts Double when available" $ do
            let ctx = PlayingPlayerCtx dummyPlayingSpine [Hit, Stand, Double] (Chips 100)
            validatePlayingPlayerIntent Double' ctx `shouldBe` Valid

        it "rejects Double when not available" $ do
            let ctx = PlayingPlayerCtx dummyPlayingSpine [Hit, Stand] (Chips 100)
            validatePlayingPlayerIntent Double' ctx `shouldBe` Invalid "Double not available"

        it "rejects non-playing intents" $ do
            let ctx = PlayingPlayerCtx dummyPlayingSpine [Hit, Stand] (Chips 100)
            validatePlayingPlayerIntent (PlaceBet (Chips 50)) ctx `shouldBe` Invalid "Invalid intent for playing player"

    describe "PlayingDealer validation" $ do
        it "accepts DealerHit when available" $ do
            let ctx = PlayingDealerCtx dummyDealerSpine [Hit, Stand]
            validatePlayingDealerIntent DealerHit ctx `shouldBe` Valid

        it "rejects DealerHit when not available" $ do
            let ctx = PlayingDealerCtx dummyDealerSpine [Stand]
            validatePlayingDealerIntent DealerHit ctx `shouldBe` Invalid "Dealer hit not available"

        it "accepts DealerStand when available" $ do
            let ctx = PlayingDealerCtx dummyDealerSpine [Hit, Stand]
            validatePlayingDealerIntent DealerStand ctx `shouldBe` Valid

        it "rejects non-playing dealer intents" $ do
            let ctx = PlayingDealerCtx dummyDealerSpine [Hit, Stand]
            validatePlayingDealerIntent StartRound ctx `shouldBe` Invalid "Invalid intent for playing dealer"
