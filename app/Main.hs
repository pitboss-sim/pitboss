{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BL
import Pitboss.Blackjack.Card
import Pitboss.Blackjack.Chips
import Pitboss.Blackjack.Offering
import Pitboss.Blackjack.Offering.WellKnown
import Pitboss.FSM.PlayerSpotFSM
import Pitboss.Trace
import Pitboss.Trace.Entity.Capabilities.Clocked hiding (tick)
import Pitboss.Trace.Entity.DealerRound
import Pitboss.Trace.Entity.Offering
import Pitboss.Trace.Entity.Player
import Pitboss.Trace.Entity.Player qualified as PE
import Pitboss.Trace.Entity.PlayerSpot
import Pitboss.Trace.Entity.Shoe
import Pitboss.Trace.Entity.Table
import Pitboss.Trace.Entity.Types.FiniteMap
import Pitboss.Trace.Entity.Types.FiniteMap.Occupancy
import Pitboss.Trace.EntityRegistry
import Pitboss.Trace.EntityRegistry.EntityRef
import Pitboss.Trace.EntityRegistry.Identifier
import System.Random (getStdGen)

main :: IO ()
main = do
  putStrLn "Generating a dummy game trace..."
  gen0 <- getStdGen
  let tick = Tick 0

      -- Generate UIDs
      (offeringUid, gen1) = generateUid 0 gen0
      (playerUid, gen2) = generateUid 1 gen1
      (spotUid, gen3) = generateUid 2 gen2
      (handUid, gen4) = generateUid 3 gen3
      (tableUid, gen5) = generateUid 4 gen4
      (shoeUid, gen6) = generateUid 5 gen5
      (roundUid, _gen7) = generateUid 6 gen6

      -- Identifiers
      offeringId = OfferingId offeringUid
      playerId = PlayerId playerUid
      spotId = PlayerSpotId spotUid
      tableId = TableId tableUid
      shoeId = ShoeId shoeUid
      roundId = RoundId roundUid
      handId = PlayerHandId handUid

      -- Offering
      offering = Offering (matter vegas6) (ruleSet vegas6)
      offeringState = mkOfferingState offering
      offeringEntity = mkOfferingEntity tick offeringState mkOfferingRelations

      -- Player
      playerState = PlayerState "Alice" (Chips 500)
      playerEntity =
        PlayerEntity
          { PE._tick = tick,
            PE._state = playerState,
            PE._rels = PlayerEntityRelations Nothing Nothing
          }

      -- Table
      tableState = mkTableState "Table 1" (EntityRef tick offeringState) (Chips 25)
      tableEntity = mkTableEntity tick tableState (TableRelations Nothing)

      -- Shoe
      fullShoe = [Card rank suit | suit <- [Hearts, Diamonds, Clubs, Spades], rank <- [Two .. Ace]]
      shoeEntity = mkShoeEntity tick fullShoe

      -- Round
      roundState = mkDealerRoundState 1
      roundRels = DealerRoundRelations (EntityRef tick (Pitboss.Trace.Entity.Shoe._state shoeEntity))
      roundEntity = DealerRoundEntity tick roundState roundRels

      -- Spot
      baseSpotState = mkPlayerSpotState PlayerSpot1 (Chips 50)
      updatedOccupancy = insertFiniteMap PlayerSpotHand1 (Present handId) (_handOccupancy baseSpotState)
      finalSpotState = baseSpotState {_handOccupancy = updatedOccupancy}

      spotEntity =
        mkPlayerSpotEntity
          tick
          (SomePlayerSpotFSM SpotEngagedFSM)
          finalSpotState
          PlayerSpotEntityRelations
            { _playerId = playerId,
              _roundId = roundId
            }

      -- Build the Trace
      trace =
        Trace
          { _offeringsEntityRegistry = insertEntity offeringId offeringEntity emptyEntityRegistry,
            _dealersEntityRegistry = emptyEntityRegistry,
            _dealerHandsEntityRegistry = emptyEntityRegistry,
            _playersEntityRegistry = insertEntity playerId playerEntity emptyEntityRegistry,
            _playerHandsEntityRegistry = emptyEntityRegistry,
            _shoesEntityRegistry = insertEntity shoeId shoeEntity emptyEntityRegistry,
            _roundsEntityRegistry = insertEntity roundId roundEntity emptyEntityRegistry,
            _spotsEntityRegistry = insertEntity spotId spotEntity emptyEntityRegistry,
            _tablesEntityRegistry = insertEntity tableId tableEntity emptyEntityRegistry
          }

  BL.writeFile "trace.json" (encodePretty trace)
  putStrLn "✅ Wrote trace.json!"
