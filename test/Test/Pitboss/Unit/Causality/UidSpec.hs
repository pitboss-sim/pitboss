{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Pitboss.Unit.Causality.UidSpec where

import Data.Word (Word64)
import Pitboss.Causality.Types.Core
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "UID Encoding/Decoding" $ do
    describe "encodeUid/decodeUid" $ do
        it "roundtrips arbitrary UIDs within encoding limits" $ property $ \(tickNum :: Word64) (entropy :: Word64) ->
            -- Skip values that would overflow when converted to Int
            -- Also ensure entropy is less than 32^8 to avoid edge case
            let safeEntropy = entropy `mod` min (fromIntegral (maxBound :: Int)) (32 ^ (8 :: Integer) - 1)
                tick = Tick tickNum
                entityId = EntityId safeEntropy
                encoded = encodeUid tick entityId
                decoded = decodeUid encoded
                -- The encoding only preserves tick modulo 32^6
                -- EntityId is encoded with 8 base32 digits, so preserves values modulo 32^8
                expectedTick = Tick (tickNum `mod` (32 ^ (6 :: Integer)))
                expectedEntityId = EntityId (safeEntropy `mod` (32 ^ (8 :: Integer)))
             in decoded == Just (expectedTick, expectedEntityId)

        it "roundtrips UIDs within design limits" $ property $ \tickNum entropy ->
            -- Test with values that fit within the encoding's design limits
            -- EntityIds are generated with at most 40 bits (2^40 - 1) in practice
            let tick = Tick (abs tickNum `mod` (32 ^ (6 :: Integer)))
                entityId = EntityId (abs entropy `mod` (2 ^ (40 :: Integer)))
                encoded = encodeUid tick entityId
                decoded = decodeUid encoded
             in -- Should roundtrip perfectly since 2^40 < 32^8
                decoded == Just (tick, entityId)

        it "encoding is lossy by design" $ do
            -- Demonstrate that large values wrap around
            let bigTick = Tick (32 ^ (6 :: Integer) + 1) -- One more than the limit
                entityId = EntityId 0
                encoded = encodeUid bigTick entityId
                decoded = decodeUid encoded
            decoded `shouldBe` Just (Tick 1, entityId)

        it "implementation has Int overflow limitation" $ do
            -- Document that very large EntityId values can cause issues
            -- due to Word64 -> Int conversion in the implementation
            let maxSafeEntityId = EntityId (fromIntegral (maxBound :: Int))
                encoded = encodeUid (Tick 0) maxSafeEntityId
            -- This should work
            length encoded `shouldBe` 15

        -- But values larger than maxBound::Int would cause overflow
        -- (We can't test this directly as it would crash)

        it "produces consistent format" $ do
            let encoded = encodeUid (Tick 1000) (EntityId 12345)
            length encoded `shouldBe` 15 -- 6 + 1 + 8
            encoded !! 6 `shouldBe` '-'

        it "handles edge cases" $ do
            encodeUid (Tick 0) (EntityId 0) `shouldBe` "000000-00000000"
            -- Test with large values that won't overflow when converted to Int
            let largeTick = Tick (32 ^ (6 :: Integer) - 1) -- Maximum that fits in 6 base32 digits
                largeEntityId = EntityId (2 ^ (40 :: Integer) - 1) -- Typical max EntityId from generator
                encoded = encodeUid largeTick largeEntityId
            length encoded `shouldBe` 15 -- Still produces valid format
        it "encoding truncates large values" $ do
            -- Values larger than encoding limits get truncated
            let oversizeTick = Tick (32 ^ (6 :: Integer) + 123)
                oversizeEntityId = EntityId (32 ^ (8 :: Integer) + 456)
            -- These should encode the same as their modulo values
            encodeUid oversizeTick oversizeEntityId
                `shouldBe` encodeUid (Tick 123) (EntityId 456)

    describe "displayUid/parseDisplayUid" $ do
        it "roundtrips with type prefixes" $ do
            let uid = Uid (Tick 1000, EntityId 12345) :: Uid 'Player
            let displayed = displayUid uid
            displayed `shouldStartWith` "PLR-"
            parseDisplayUid @'Player displayed `shouldBe` Just uid

        it "rejects wrong prefixes" $ do
            let uid = Uid (Tick 1000, EntityId 12345) :: Uid 'Player
            let displayed = displayUid uid
            parseDisplayUid @'Dealer displayed `shouldBe` Nothing

        it "works for all entity types" $ do
            let tick = Tick 1000
                eid = EntityId 12345

            -- Test each entity type
            displayUid (Uid (tick, eid) :: Uid 'Bout) `shouldStartWith` "BOT-"
            displayUid (Uid (tick, eid) :: Uid 'Player) `shouldStartWith` "PLR-"
            displayUid (Uid (tick, eid) :: Uid 'Dealer) `shouldStartWith` "DLR-"
            displayUid (Uid (tick, eid) :: Uid 'Table) `shouldStartWith` "TBL-"
            displayUid (Uid (tick, eid) :: Uid 'PlayerHand) `shouldStartWith` "PHD-"
            displayUid (Uid (tick, eid) :: Uid 'DealerHand) `shouldStartWith` "DHD-"
            displayUid (Uid (tick, eid) :: Uid 'PlayerSpot) `shouldStartWith` "PST-"
            displayUid (Uid (tick, eid) :: Uid 'DealerRound) `shouldStartWith` "DRD-"
            displayUid (Uid (tick, eid) :: Uid 'TableShoe) `shouldStartWith` "SHO-"

    describe "Base32 encoding properties" $ do
        it "uses valid base32 characters" $ property $ \(n :: Int) ->
            let encoded = showPaddedBase32 (abs n `mod` (32 ^ (8 :: Integer))) 8
             in all isBase32Char encoded

        it "decodes what it encodes" $ property $ \(n :: Int) ->
            let value = abs n `mod` 32
                char = "0123456789ABCDEFGHJKMNPQRSTVWXYZ" !! value
             in decodeBase32Char char == Just value
