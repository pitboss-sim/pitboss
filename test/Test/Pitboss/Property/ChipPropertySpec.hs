{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Pitboss.Property.ChipPropertySpec where

import Pitboss.Blackjack
import Test.Pitboss.TestUtils ()
import Test.Hspec
import Test.QuickCheck hiding (witness)

-- Generators
genCard :: Gen Card
genCard = Card <$> arbitrary <*> arbitrary

genHand :: Gen [Card]
genHand = do
    n <- choose (2, 5)
    vectorOf n genCard

spec :: Spec
spec = describe "Property Tests" $ do
    describe "Chip arithmetic properties" $ do
        it "chips have additive identity" $ do
            (Chips 0 + Chips 100) `shouldBe` Chips 100
            (Chips 100 + Chips 0) `shouldBe` Chips 100

        it "chip addition is associative" $ property $ \a b c ->
            let chips_a = Chips (abs a)
                chips_b = Chips (abs b)
                chips_c = Chips (abs c)
             in (chips_a + chips_b) + chips_c == chips_a + (chips_b + chips_c)
