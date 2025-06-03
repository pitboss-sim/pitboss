{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Test.Pitboss.Unit.Causality.FiniteMapSpec where

import Pitboss.Blackjack
import Pitboss.Causality
import Test.Hspec
import Test.Pitboss.TestUtils ()
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
    describe "FiniteMap properties" $ do
        it "singleton contains exactly one present value" $ property $ \ix val ->
            let fm =
                    singletonFiniteMap ix (Present val) Absent ::
                        FiniteMap PlayerSpotHandIx (Occupancy (EntityId 'PlayerHand))
                presentCount = length [v | (_, Present v) <- toListFiniteMap fm]
             in presentCount == 1

        it "insert overwrites previous value" $ property $ \ix val1 val2 ->
            let fm1 = singletonFiniteMap ix (Present val1) Absent
                fm2 =
                    insertFiniteMap ix (Present val2) fm1 ::
                        FiniteMap PlayerSpotHandIx (Occupancy (EntityId 'PlayerHand))
             in lookupFiniteMap ix fm2 == Just (Present val2)
