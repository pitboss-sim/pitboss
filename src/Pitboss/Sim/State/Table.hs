{-# LANGUAGE FlexibleContexts #-}

module Pitboss.Sim.State.Table where

import Control.Lens (Lens', lens)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Sim.State.Spot
  ( SpotState,
    emptySpot,
  )
import Pitboss.Sim.Types.FiniteMap
  ( FiniteMap,
    emptyFiniteMap,
    insertFiniteMap,
    lookupFiniteMap,
  )
import Pitboss.Sim.Types.Occupancy (Occupancy (..))
import Pitboss.Types.BoundedEnum (BoundedEnum)

data TableSpotIx = Spot1 | Spot2 | Spot3 | Spot4
  deriving (Eq, Ord, Enum, Bounded, Show)

instance BoundedEnum TableSpotIx

data RoundPhase
  = AwaitingEntry
  | PlayingHands
  | DealerActing
  | Settling
  deriving (Eq, Show)

data ShoeState
  = ShoeKnown Int
  | ShoeUnknown
  deriving (Eq, Show)

data TableState = TableState
  { roundPhase :: RoundPhase,
    deck :: [Card],
    shoeState :: ShoeState,
    playerSpots :: FiniteMap TableSpotIx (Occupancy SpotState),
    dealerSpot :: SpotState
  }
  deriving (Eq, Show)

emptyTableState :: TableState
emptyTableState =
  TableState
    { roundPhase = AwaitingEntry,
      deck = [],
      shoeState = ShoeUnknown,
      playerSpots = emptyFiniteMap Absent,
      dealerSpot = emptySpot
    }

claimSpot :: TableSpotIx -> TableState -> Either String TableState
claimSpot spotId ts =
  case lookupFiniteMap spotId (playerSpots ts) of
    Just Absent ->
      let updatedSpots = insertFiniteMap spotId (Present emptySpot) (playerSpots ts)
       in Right ts {playerSpots = updatedSpots}
    Just (Present _) ->
      Left $ "Spot " ++ show spotId ++ " is already claimed."
    Nothing ->
      Left $ "Invalid spot: " ++ show spotId

yieldSpot :: TableSpotIx -> TableState -> Either String TableState
yieldSpot spotId ts =
  case lookupFiniteMap spotId (playerSpots ts) of
    Just (Present _) ->
      let updatedSpots = insertFiniteMap spotId Absent (playerSpots ts)
       in Right ts {playerSpots = updatedSpots}
    Just Absent ->
      Left $ "Spot " ++ show spotId ++ " is already unclaimed."
    Nothing ->
      Left $ "Invalid spot: " ++ show spotId

lensRoundPhase :: Lens' TableState RoundPhase
lensRoundPhase = lens roundPhase (\s x -> s {roundPhase = x})

lensDeck :: Lens' TableState [Card]
lensDeck = lens deck (\s x -> s {deck = x})

lensShoeState :: Lens' TableState ShoeState
lensShoeState = lens shoeState (\s x -> s {shoeState = x})

lensPlayerSpots :: Lens' TableState (FiniteMap TableSpotIx (Occupancy SpotState))
lensPlayerSpots = lens playerSpots (\s x -> s {playerSpots = x})

lensDealerSpot :: Lens' TableState SpotState
lensDealerSpot = lens dealerSpot (\s x -> s {dealerSpot = x})

lensFiniteMapAt :: (Ord k) => k -> Lens' (FiniteMap k v) (Maybe v)
lensFiniteMapAt k =
  lens
    (lookupFiniteMap k)
    ( \m mv ->
        case mv of
          Just v -> insertFiniteMap k v m
          Nothing -> m
    )
