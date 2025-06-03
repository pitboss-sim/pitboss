{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Pitboss.Property.ChipPropertySpec where

import Pitboss.Blackjack
import Test.Hspec
import Test.Pitboss.TestUtils ()
import Test.QuickCheck

spec :: Spec
spec = describe "Chip Property Tests" $ do
    describe "arithmetic properties" $ do
        it "has additive identity" $ do
            Chips 0 + Chips 100 `shouldBe` Chips 100
            Chips 100 + Chips 0 `shouldBe` Chips 100

        it "addition is commutative" $ property $ \a b ->
            let chipsA = Chips (abs a)
                chipsB = Chips (abs b)
             in chipsA + chipsB == chipsB + chipsA

        it "addition is associative" $ property $ \a b c ->
            let chipsA = Chips (abs a)
                chipsB = Chips (abs b)
                chipsC = Chips (abs c)
             in (chipsA + chipsB) + chipsC == chipsA + (chipsB + chipsC)

        it "preserves ordering under addition" $ property $ \a b c ->
            let chipsA = Chips (abs a)
                chipsB = Chips (abs b)
                chipsC = Chips (abs c)
             in (chipsA <= chipsB) ==> (chipsA + chipsC <= chipsB + chipsC)

    describe "comparison properties" $ do
        it "is reflexive" $ property $ \a ->
            let chips = Chips (abs a)
             in chips <= chips

        it "is transitive" $ property $ \a b c ->
            let chipsA = Chips (abs a `mod` 1000) -- Keep numbers reasonable
                chipsB = Chips (abs b `mod` 1000)
                chipsC = Chips (abs c `mod` 1000)
             in (chipsA <= chipsB && chipsB <= chipsC) ==> (chipsA <= chipsC)

        it "comparison is consistent with equality" $ property $ \a b ->
            let chipsA = Chips (abs a)
                chipsB = Chips (abs b)
             in (chipsA == chipsB) == (chipsA <= chipsB && chipsB <= chipsA)
