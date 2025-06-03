{-# LANGUAGE DataKinds #-}

module Test.Pitboss.Unit.Causality.FiniteMapSpec where

import Data.Word (Word64)
import Pitboss.Causality
import Test.Hspec

-- Type aliases for readability
type IntFiniteMap = FiniteMap HandIx (Occupancy Int)
type BoutIdFiniteMap = FiniteMap HandIx (Occupancy BoutId)

-- Test constants
testValue :: Int
testValue = 42

largeTestValue :: Word64
largeTestValue = 999

smallTestValue :: Word64
smallTestValue = 123

doubleValue :: Integer
doubleValue = 20

-- Local helpers
emptyIntMap :: IntFiniteMap
emptyIntMap = emptyFiniteMap Absent

emptyBoutIdMap :: BoutIdFiniteMap
emptyBoutIdMap = emptyFiniteMap Absent

shouldLookupTo :: (Eq a, Show a) => FiniteMap HandIx a -> HandIx -> a -> Expectation
shouldLookupTo fm handIx expected = lookupFiniteMap handIx fm `shouldBe` Just expected

spec :: Spec
spec = describe "FiniteMap" $ do
    describe "creation and access" $ do
        it "creates empty finite map with default values" $ do
            emptyIntMap `shouldLookupTo` Hand1 $ Absent

        it "inserts and retrieves values" $ do
            let fm' = insertFiniteMap Hand1 (Present testValue) emptyIntMap
            fm' `shouldLookupTo` Hand1 $ Present testValue

        it "works with entity relationships" $ do
            let fm' = insertFiniteMap Hand2 (Present (EntityId largeTestValue :: BoutId)) emptyBoutIdMap
            fm' `shouldLookupTo` Hand2 $ Present (EntityId largeTestValue :: BoutId)

        it "singleton works correctly" $ do
            let fm = singletonFiniteMap Hand3 (Present (EntityId smallTestValue :: BoutId)) Absent
            fm `shouldLookupTo` Hand3 $ Present (EntityId smallTestValue :: BoutId)
            fm `shouldLookupTo` Hand1 $ Absent

    describe "operations" $ do
        it "maps over values" $ do
            let fm = insertFiniteMap Hand1 (Present 10) emptyIntMap
                doublePresent _ val = case val of
                    Present x -> Present (fromIntegral x * 2)
                    Absent -> Absent
                fm' = mapFiniteMapWithKey doublePresent fm
            fm' `shouldLookupTo` Hand1 $ Present doubleValue

        it "converts to list" $ do
            let fm = insertFiniteMap Hand1 (Present testValue) emptyIntMap
                list = toListFiniteMap fm
            length list `shouldBe` 4
