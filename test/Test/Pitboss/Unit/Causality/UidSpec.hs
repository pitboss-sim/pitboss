{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Pitboss.Unit.Causality.UidSpec where

import Data.Word (Word64)
import GHC.TypeLits (KnownSymbol)
import Pitboss.Causality.Types.Core
import Test.Hspec
import Test.Pitboss.TestUtils (testTick)
import Test.QuickCheck

-- Constants
tickEncodingLimit :: Integer
tickEncodingLimit = 32 ^ (6 :: Integer)

entityEncodingLimit :: Integer
entityEncodingLimit = 32 ^ (8 :: Integer)

practicalEntityLimit :: Integer
practicalEntityLimit = 2 ^ (40 :: Integer)

testEntityValue :: Word64
testEntityValue = 12345

-- Local helpers
mkPlayerUid :: Tick -> Word64 -> Uid 'Player
mkPlayerUid tick entropy = Uid (tick, PlayerId entropy)

mkTestUid :: Word64 -> Uid 'Player
mkTestUid = mkPlayerUid testTick

shouldRoundtrip :: (Eq a, Show a) => a -> a -> Expectation
shouldRoundtrip actual expected = actual `shouldBe` expected

spec :: Spec
spec = describe "UID Encoding/Decoding" $ do
    describe "encodeUid/decodeUid roundtrip" $ do
        it "preserves values within encoding limits" $ property $ \tickNum entropy ->
            let safeTick = Tick (tickNum `mod` fromIntegral tickEncodingLimit)
                safeEntropy = entropy `mod` fromIntegral practicalEntityLimit
                _ = mkPlayerUid safeTick safeEntropy
                encoded = encodeUid safeTick (PlayerId safeEntropy)
                decoded = decodeUid encoded
             in decoded `shouldBe` Just (safeTick, PlayerId safeEntropy)

        it "truncates values beyond encoding limits" $ property $ \tickNum entropy ->
            let tick = Tick tickNum
                entityId = PlayerId entropy
                encoded = encodeUid tick entityId
                decoded = decodeUid encoded
                expectedTick = Tick (tickNum `mod` fromIntegral tickEncodingLimit)
                expectedEntity = PlayerId (entropy `mod` fromIntegral entityEncodingLimit)
             in decoded `shouldBe` Just (expectedTick, expectedEntity)

    describe "encoding behavior" $ do
        it "produces consistent 15-character format" $ do
            let encoded = encodeUid testTick (PlayerId testEntityValue)
            length encoded `shouldBe` 15
            encoded !! 6 `shouldBe` '-'

        it "handles zero values" $ do
            encodeUid (Tick 0) (PlayerId 0) `shouldBe` "000000-00000000"

        it "demonstrates lossy encoding for large values" $ do
            let bigTick = Tick (fromIntegral tickEncodingLimit + 1)
                encoded = encodeUid bigTick (PlayerId 0)
                decoded = decodeUid encoded
            decoded `shouldBe` Just (Tick 1, PlayerId 0)

        it "truncates oversized values consistently" $ do
            let oversizeTick = Tick (fromIntegral tickEncodingLimit + 123)
                oversizeEntity = PlayerId (fromIntegral entityEncodingLimit + 456)
                actual = encodeUid oversizeTick oversizeEntity
                expected = encodeUid (Tick 123) (PlayerId 456)
            actual `shouldBe` expected

    describe "displayUid/parseDisplayUid with prefixes" $ do
        it "roundtrips with correct type prefix" $ do
            let uid = mkTestUid testEntityValue
                displayed = displayUid uid
            displayed `shouldStartWith` "PLR-"
            parseDisplayUid @'Player displayed `shouldBe` Just uid

        it "rejects mismatched type prefixes" $ do
            let uid = mkTestUid testEntityValue
                displayed = displayUid uid
            parseDisplayUid @'Dealer displayed `shouldBe` Nothing

        it "generates correct prefixes for all entity types" $ do
            let testWithPrefix :: forall k. (EntityIdClass (EntityIdFor k), KnownSymbol (UidPrefix k)) => String -> Uid k -> Expectation
                testWithPrefix expectedPrefix uid = displayUid uid `shouldStartWith` expectedPrefix

            testWithPrefix "BOT-" (Uid (testTick, BoutId 0) :: Uid 'Bout)
            testWithPrefix "CNT-" (Uid (testTick, ContestantId 0) :: Uid 'Contestant)
            testWithPrefix "DLR-" (Uid (testTick, DealerId 0) :: Uid 'Dealer)
            testWithPrefix "HND-" (Uid (testTick, HandId 0) :: Uid 'Hand)
            testWithPrefix "PLR-" (Uid (testTick, PlayerId 0) :: Uid 'Player)
            testWithPrefix "DRD-" (Uid (testTick, RoundId 0) :: Uid 'Round)
            testWithPrefix "SHO-" (Uid (testTick, ShoeId 0) :: Uid 'Shoe)
            testWithPrefix "TBL-" (Uid (testTick, TableId 0) :: Uid 'Table)

    describe "base32 encoding properties" $ do
        it "uses only valid base32 characters" $ property $ \(n :: Int) ->
            let value = abs (toInteger n) `mod` entityEncodingLimit
                encoded = showPaddedBase32 value 8
             in all isBase32Char encoded

        it "character encoding roundtrips" $ property $ \(n :: Int) ->
            let value = abs n `mod` 32
                char = "0123456789ABCDEFGHJKMNPQRSTVWXYZ" !! value
             in decodeBase32Char char `shouldBe` Just value
