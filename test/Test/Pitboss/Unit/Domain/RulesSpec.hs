module Test.Pitboss.Unit.Domain.RulesSpec where

import Pitboss.Blackjack
import Test.Hspec
import Test.Pitboss.TestUtils

spec :: Spec
spec = describe "Game Rules" $ do
    describe "Dealer Hit/Stand Logic" $ do
        it "hits on hard 16" $ do
            let hand = characterize [Card Ten Hearts, Card Six Spades]
            dealerShouldHit mkS17Rules hand `shouldBe` True
            dealerShouldHit mkH17Rules hand `shouldBe` True

        it "stands on hard 17" $ do
            let hand = characterize [Card Ten Hearts, Card Seven Spades]
            dealerShouldHit mkS17Rules hand `shouldBe` False
            dealerShouldHit mkH17Rules hand `shouldBe` False

        it "stands on hard 21" $ do
            let hand = characterize [Card Seven Hearts, Card Seven Spades, Card Seven Clubs]
            dealerShouldHit mkS17Rules hand `shouldBe` False
            dealerShouldHit mkH17Rules hand `shouldBe` False

        it "never hits on blackjack" $ do
            let hand = characterize [Card Ace Hearts, Card King Spades]
            dealerShouldHit mkS17Rules hand `shouldBe` False
            dealerShouldHit mkH17Rules hand `shouldBe` False

        it "never hits on bust" $ do
            let hand = characterize [Card Ten Hearts, Card Six Spades, Card King Clubs]
            dealerShouldHit mkS17Rules hand `shouldBe` False
            dealerShouldHit mkH17Rules hand `shouldBe` False

        it "hits on soft 16" $ do
            let hand = characterize [Card Ace Hearts, Card Five Spades]
            dealerShouldHit mkS17Rules hand `shouldBe` True
            dealerShouldHit mkH17Rules hand `shouldBe` True

        it "stands on soft 18" $ do
            let hand = characterize [Card Ace Hearts, Card Seven Spades]
            dealerShouldHit mkS17Rules hand `shouldBe` False
            dealerShouldHit mkH17Rules hand `shouldBe` False

    describe "Soft 17 Rule Differences" $ do
        it "S17: stands on soft 17" $ do
            let hand = characterize [Card Ace Hearts, Card Six Spades]
            dealerShouldHit mkS17Rules hand `shouldBe` False

        it "H17: hits on soft 17" $ do
            let hand = characterize [Card Ace Hearts, Card Six Spades]
            dealerShouldHit mkH17Rules hand `shouldBe` True

        it "S17: stands on multi-card soft 17" $ do
            let hand = characterize [Card Ace Hearts, Card Two Spades, Card Four Clubs]
            dealerShouldHit mkS17Rules hand `shouldBe` False

        it "H17: hits on multi-card soft 17" $ do
            let hand = characterize [Card Ace Hearts, Card Two Spades, Card Four Clubs]
            dealerShouldHit mkH17Rules hand `shouldBe` True

        it "H17: hits on complex soft 17" $ do
            let hand = characterize [Card Ace Hearts, Card Ace Spades, Card Five Clubs]
            dealerShouldHit mkH17Rules hand `shouldBe` True

    describe "Peek Rules" $ do
        it "Peek rules: peeks on ace" $ do
            dealerShouldPeek (gameRuleSet vegas6) (Card Ace Hearts) `shouldBe` True

        it "Peek rules: peeks on ten-value cards" $ do
            dealerShouldPeek (gameRuleSet vegas6) (Card Ten Hearts) `shouldBe` True
            dealerShouldPeek (gameRuleSet vegas6) (Card Jack Spades) `shouldBe` True
            dealerShouldPeek (gameRuleSet vegas6) (Card Queen Diamonds) `shouldBe` True
            dealerShouldPeek (gameRuleSet vegas6) (Card King Clubs) `shouldBe` True

        it "Peek rules: no peek on low cards" $ do
            dealerShouldPeek (gameRuleSet vegas6) (Card Seven Diamonds) `shouldBe` False
            dealerShouldPeek (gameRuleSet vegas6) (Card Two Hearts) `shouldBe` False

        it "ENHC rules: never peeks" $ do
            let enhcRules = (gameRuleSet vegas6){holeCardRule = ENHC}
            dealerShouldPeek enhcRules (Card Ace Hearts) `shouldBe` False
            dealerShouldPeek enhcRules (Card Ten Hearts) `shouldBe` False
            dealerShouldPeek enhcRules (Card Seven Diamonds) `shouldBe` False

    describe "Blackjack Detection" $ do
        it "detects dealer blackjack" $ do
            let hand = characterize [Card Ace Hearts, Card King Spades]
            dealerHasBlackjack hand `shouldBe` True

        it "detects dealer blackjack (ten first)" $ do
            let hand = characterize [Card King Hearts, Card Ace Spades]
            dealerHasBlackjack hand `shouldBe` True

        it "rejects 21 that isn't blackjack" $ do
            let hand = characterize [Card Seven Hearts, Card Seven Spades, Card Seven Clubs]
            dealerHasBlackjack hand `shouldBe` False

        it "rejects soft 21 with more than 2 cards" $ do
            let hand = characterize [Card Ace Hearts, Card Five Spades, Card Five Clubs]
            dealerHasBlackjack hand `shouldBe` False

    describe "Upcard Detection" $ do
        it "extracts dealer upcard from two-card hand" $ do
            let hand = characterize [Card Ace Hearts, Card King Spades]
            dealerUpcard hand `shouldBe` Just (Card Ace Hearts)

        it "extracts dealer upcard from multi-card hand" $ do
            let hand = characterize [Card Seven Hearts, Card Five Spades, Card Six Clubs]
            dealerUpcard hand `shouldBe` Just (Card Seven Hearts)

        it "handles empty hand" $ do
            let hand = characterize []
            dealerUpcard hand `shouldBe` Nothing

    describe "Helper Functions" $ do
        it "identifies S17 rules correctly" $ do
            isH17 mkS17Rules `shouldBe` False

        it "identifies H17 rules correctly" $ do
            isH17 mkH17Rules `shouldBe` True

        it "detects ace showing" $ do
            dealerShowsAce (Card Ace Hearts) `shouldBe` True
            dealerShowsAce (Card King Hearts) `shouldBe` False

        it "detects ten showing" $ do
            dealerShowsTen (Card Ten Hearts) `shouldBe` True
            dealerShowsTen (Card Jack Hearts) `shouldBe` True
            dealerShowsTen (Card Queen Hearts) `shouldBe` True
            dealerShowsTen (Card King Hearts) `shouldBe` True
            dealerShowsTen (Card Ace Hearts) `shouldBe` False

    describe "Edge Cases" $ do
        it "handles pair of 8s as hard 16" $ do
            let hand = characterize [Card Eight Hearts, Card Eight Spades]
            dealerShouldHit mkS17Rules hand `shouldBe` True
            dealerShouldHit mkH17Rules hand `shouldBe` True

        it "handles multiple aces correctly" $ do
            let hand = characterize [Card Ace Hearts, Card Ace Spades, Card Ace Clubs, Card Ace Diamonds, Card Three Hearts]
            dealerShouldHit mkS17Rules hand `shouldBe` False
            dealerShouldHit mkH17Rules hand `shouldBe` True

        it "handles hard 17 from many small cards" $ do
            let hand = characterize [Card Two Hearts, Card Three Spades, Card Four Clubs, Card Four Diamonds, Card Four Hearts]
            dealerShouldHit mkS17Rules hand `shouldBe` False
            dealerShouldHit mkH17Rules hand `shouldBe` False
