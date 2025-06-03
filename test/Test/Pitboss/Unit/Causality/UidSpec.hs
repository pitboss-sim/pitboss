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
mkPlayerUid tick entropy = Uid (tick, EntityId entropy :: PlayerId)

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
                encoded = encodeUid safeTick (EntityId safeEntropy :: PlayerId)
                decoded = decodeUid encoded
             in decoded `shouldBe` Just (safeTick, EntityId safeEntropy :: PlayerId)

        it "truncates values beyond encoding limits" $ property $ \tickNum entropy ->
            let tick = Tick tickNum
                entityId = EntityId entropy :: PlayerId
                encoded = encodeUid tick entityId
                decoded = decodeUid encoded
                expectedTick = Tick (tickNum `mod` fromIntegral tickEncodingLimit)
                expectedEntity = EntityId (entropy `mod` fromIntegral entityEncodingLimit) :: PlayerId
             in decoded `shouldBe` Just (expectedTick, expectedEntity)

    describe "encoding behavior" $ do
        it "produces consistent 15-character format" $ do
            let encoded = encodeUid testTick (EntityId testEntityValue :: PlayerId)
            length encoded `shouldBe` 15
            encoded !! 6 `shouldBe` '-'

        it "handles zero values" $ do
            encodeUid (Tick 0) (EntityId 0 :: PlayerId) `shouldBe` "000000-00000000"

        it "demonstrates lossy encoding for large values" $ do
            let bigTick = Tick (fromIntegral tickEncodingLimit + 1)
                encoded = encodeUid bigTick (EntityId 0 :: PlayerId)
                decoded = decodeUid encoded
            decoded `shouldBe` Just (Tick 1, EntityId 0 :: PlayerId)

        it "truncates oversized values consistently" $ do
            let oversizeTick = Tick (fromIntegral tickEncodingLimit + 123)
                oversizeEntity = EntityId (fromIntegral entityEncodingLimit + 456) :: PlayerId
                actual = encodeUid oversizeTick oversizeEntity
                expected = encodeUid (Tick 123) (EntityId 456 :: PlayerId)
            actual `shouldBe` expected

    describe "displayUid/parseDisplayUid with prefixes" $ do
        it "roundtrips with correct type prefix" $ do
            let uid = mkTestUid testEntityValue
                displayed = displayUid uid
            displayed `shouldStartWith` "PLYR-"
            parseDisplayUid @'Player displayed `shouldBe` Just uid

        it "rejects mismatched type prefixes" $ do
            let uid = mkTestUid testEntityValue
                displayed = displayUid uid
            parseDisplayUid @'Dealer displayed `shouldBe` Nothing

        it "generates correct prefixes for all entity types" $ do
            let testWithPrefix :: forall k. (EntityIdClass (EntityIdFor k), KnownSymbol (UidPrefix k)) => String -> Uid k -> Expectation
                testWithPrefix expectedPrefix uid = displayUid uid `shouldStartWith` expectedPrefix

            testWithPrefix "BOUT-" (Uid (testTick, EntityId 0 :: BoutId) :: Uid 'Bout)
            testWithPrefix "DELR-" (Uid (testTick, EntityId 0 :: DealerId) :: Uid 'Dealer)
            testWithPrefix "PLYR-" (Uid (testTick, EntityId 0 :: PlayerId) :: Uid 'Player)
            testWithPrefix "ROND-" (Uid (testTick, EntityId 0 :: RoundId) :: Uid 'Round)
            testWithPrefix "SHOE-" (Uid (testTick, EntityId 0 :: ShoeId) :: Uid 'Shoe)
            testWithPrefix "TABL-" (Uid (testTick, EntityId 0 :: TableId) :: Uid 'Table)

    describe "base32 encoding properties" $ do
        it "uses only valid base32 characters" $ property $ \(n :: Int) ->
            let value = abs (toInteger n) `mod` entityEncodingLimit
                encoded = showPaddedBase32 value 8
             in all isBase32Char encoded

        it "character encoding roundtrips" $ property $ \(n :: Int) ->
            let value = abs n `mod` 32
                char = "0123456789ABCDEFGHJKMNPQRSTVWXYZ" !! value
             in decodeBase32Char char `shouldBe` Just value
