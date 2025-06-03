{-# LANGUAGE DataKinds #-}
-- EBout
{-# LANGUAGE RankNTypes #-}

module Pitboss.State.Entity.Lenses where

import Control.Lens hiding (ix)
import Data.Map.Strict
import Pitboss.Blackjack
import Pitboss.FSM
import Pitboss.Sim.Agency.Archetype.Types
import Pitboss.State.Entity.Types
import Pitboss.State.Types.Core
import Pitboss.State.Types.FiniteMap
import Pitboss.State.Types.FiniteMap.Occupancy
import Prelude hiding (round)

bAttrs :: Lens' (EntityState 'Bout) BoutAttrs
bAttrs f (EBout a m r) = fmap (\a' -> EBout a' m r) (f a)

bModes :: Lens' (EntityState 'Bout) BoutModes
bModes f (EBout a m r) = fmap (\m' -> EBout a m' r) (f m)

bRels :: Lens' (EntityState 'Bout) BoutRels
bRels f (EBout a m r) = fmap (EBout a m) (f r)

bAttrsOutcome :: Lens' BoutAttrs (Maybe DetailedOutcome)
bAttrsOutcome f (BoutAttrs outcome) = fmap BoutAttrs (f outcome)

bModesFSM :: Lens' BoutModes SomeBoutFSM
bModesFSM f (BoutModes fsm) = fmap BoutModes (f fsm)

-- EBout
bRelsDealerHand :: Lens' BoutRels (EntityId 'DealerHand)
bRelsDealerHand f (BoutRels player dealer shoe table round) = fmap (\d -> BoutRels player d shoe table round) (f dealer)

bRelsDealerRound :: Lens' BoutRels (EntityId 'DealerRound)
bRelsDealerRound f (BoutRels player dealer shoe table round) = fmap (BoutRels player dealer shoe table) (f round)

bRelsPlayerHand :: Lens' BoutRels (EntityId 'PlayerHand)
bRelsPlayerHand f (BoutRels player dealer shoe table round) = fmap (\p -> BoutRels p dealer shoe table round) (f player)

bRelsTable :: Lens' BoutRels (EntityId 'Table)
bRelsTable f (BoutRels player dealer shoe table round) = fmap (\t -> BoutRels player dealer shoe t round) (f table)

bRelsTableShoe :: Lens' BoutRels (EntityId 'TableShoe)
bRelsTableShoe f (BoutRels player dealer shoe table round) = fmap (\s -> BoutRels player dealer s table round) (f shoe)

-- EDealer
dAttrs :: Lens' (EntityState 'Dealer) DealerAttrs
dAttrs f (EDealer a m r) = fmap (\a' -> EDealer a' m r) (f a)

dModes :: Lens' (EntityState 'Dealer) DealerModes
dModes f (EDealer a m r) = fmap (\m' -> EDealer a m' r) (f m)

dRels :: Lens' (EntityState 'Dealer) DealerRels
dRels f (EDealer a m r) = fmap (EDealer a m) (f r)

dAttrsName :: Lens' DealerAttrs String
dAttrsName f (DealerAttrs name) = fmap DealerAttrs (f name)

dModesDealerTable :: (Functor f) => (SomeDealerTableFSM -> f SomeDealerTableFSM) -> DealerModes -> f DealerModes
dModesDealerTable f (DealerModes table round hand) = fmap (\t -> DealerModes t round hand) (f table)

dModesDealerRound :: Lens' DealerModes DealerRoundFSM
dModesDealerRound f (DealerModes table round hand) = fmap (\r -> DealerModes table r hand) (f round)

dModesDealerHand :: Lens' DealerModes SomeDealerHandFSM
dModesDealerHand f (DealerModes table round hand) = fmap (DealerModes table round) (f hand)

dRelsActiveTable :: Lens' DealerRels (Maybe (EntityId 'Table))
dRelsActiveTable f (DealerRels table round hand) = fmap (\t -> DealerRels t round hand) (f table)

dRelsActiveRound :: Lens' DealerRels (Maybe (EntityId 'DealerRound))
dRelsActiveRound f (DealerRels table round hand) = fmap (\r -> DealerRels table r hand) (f round)

dRelsActiveHand :: Lens' DealerRels (Maybe (EntityId 'DealerHand))
dRelsActiveHand f (DealerRels table round hand) = fmap (DealerRels table round) (f hand)

-- EDealerHand
dhAttrs :: Lens' (EntityState 'DealerHand) DealerHandAttrs
dhAttrs f (EDealerHand a m r) = fmap (\a' -> EDealerHand a' m r) (f a)

dhModes :: Lens' (EntityState 'DealerHand) DealerHandModes
dhModes f (EDealerHand a m r) = fmap (\m' -> EDealerHand a m' r) (f m)

dhRels :: Lens' (EntityState 'DealerHand) DealerHandRels
dhRels f (EDealerHand a m r) = fmap (EDealerHand a m) (f r)

dhAttrsHand :: Lens' DealerHandAttrs SomeHand
dhAttrsHand f (DealerHandAttrs cards) = fmap DealerHandAttrs (f cards)

dhModesDealerHand :: Lens' DealerHandModes SomeDealerHandFSM
dhModesDealerHand f (DealerHandModes fsm) = fmap DealerHandModes (f fsm)

dhRelsDealerRound :: Lens' DealerHandRels (EntityId 'DealerRound)
dhRelsDealerRound f (DealerHandRels round dealer) = fmap (`DealerHandRels` dealer) (f round)

dhRelsDealer :: Lens' DealerHandRels (EntityId 'Dealer)
dhRelsDealer f (DealerHandRels round dealer) = fmap (DealerHandRels round) (f dealer)

-- EDealerRound
drAttrs :: Lens' (EntityState 'DealerRound) DealerRoundAttrs
drAttrs f (EDealerRound a m r) = fmap (\a' -> EDealerRound a' m r) (f a)

drModes :: Lens' (EntityState 'DealerRound) DealerRoundModes
drModes f (EDealerRound a m r) = fmap (\m' -> EDealerRound a m' r) (f m)

drRels :: Lens' (EntityState 'DealerRound) DealerRoundRels
drRels f (EDealerRound a m r) = fmap (EDealerRound a m) (f r)

drAttrsNumber :: Lens' DealerRoundAttrs Int
drAttrsNumber f (DealerRoundAttrs number active) = fmap (`DealerRoundAttrs` active) (f number)

drAttrsIsActive :: Lens' DealerRoundAttrs Bool
drAttrsIsActive f (DealerRoundAttrs number active) = fmap (DealerRoundAttrs number) (f active)

drRelsTableShoeUsed :: Lens' DealerRoundRels (EntityId 'TableShoe)
drRelsTableShoeUsed f (DealerRoundRels shoe) = fmap DealerRoundRels (f shoe)

-- EPlayer
pAttrs :: Lens' (EntityState 'Player) PlayerAttrs
pAttrs f (EPlayer a m r) = fmap (\a' -> EPlayer a' m r) (f a)

pModes :: Lens' (EntityState 'Player) PlayerModes
pModes f (EPlayer a m r) = fmap (\m' -> EPlayer a m' r) (f m)

pRels :: Lens' (EntityState 'Player) PlayerRels
pRels f (EPlayer a m r) = fmap (EPlayer a m) (f r)

pAttrsName :: Lens' PlayerAttrs String
pAttrsName f (PlayerAttrs name bankroll) = fmap (`PlayerAttrs` bankroll) (f name)

pAttrsBankroll :: Lens' PlayerAttrs Chips
pAttrsBankroll f (PlayerAttrs name bankroll) = fmap (PlayerAttrs name) (f bankroll)

pModesPlayerTable :: Lens' PlayerModes SomePlayerTableFSM
pModesPlayerTable f (PlayerModes table spot hand) = fmap (\t -> PlayerModes t spot hand) (f table)

pModesPlayerSpot :: Lens' PlayerModes SomePlayerSpotFSM
pModesPlayerSpot f (PlayerModes table spot hand) = fmap (\s -> PlayerModes table s hand) (f spot)

pModesPlayerHand :: Lens' PlayerModes SomePlayerHandFSM
pModesPlayerHand f (PlayerModes table spot hand) = fmap (PlayerModes table spot) (f hand)

-- EPlayerHand
phAttrs :: Lens' (EntityState 'PlayerHand) PlayerHandAttrs
phAttrs f (EPlayerHand a m r) = fmap (\a' -> EPlayerHand a' m r) (f a)

phModes :: Lens' (EntityState 'PlayerHand) PlayerHandModes
phModes f (EPlayerHand a m r) = fmap (\m' -> EPlayerHand a m' r) (f m)

phRels :: Lens' (EntityState 'PlayerHand) PlayerHandRels
phRels f (EPlayerHand a m r) = fmap (EPlayerHand a m) (f r)

phAttrsHand :: Lens' PlayerHandAttrs SomeHand
phAttrsHand f (PlayerHandAttrs cards bet depth ix) = fmap (\c -> PlayerHandAttrs c bet depth ix) (f cards)

phAttrsOriginalBet :: Lens' PlayerHandAttrs Chips
phAttrsOriginalBet f (PlayerHandAttrs cards bet depth ix) = fmap (\b -> PlayerHandAttrs cards b depth ix) (f bet)

phAttrsSplitDepth :: Lens' PlayerHandAttrs Int
phAttrsSplitDepth f (PlayerHandAttrs cards bet depth ix) = fmap (\d -> PlayerHandAttrs cards bet d ix) (f depth)

phAttrsHandIx :: Lens' PlayerHandAttrs Int
phAttrsHandIx f (PlayerHandAttrs cards bet depth ix) = fmap (PlayerHandAttrs cards bet depth) (f ix)

phFsm :: Lens' PlayerHandModes SomePlayerHandFSM
phFsm f (PlayerHandModes fsm) = fmap PlayerHandModes (f fsm)

phRelsBelongsToBout :: Lens' PlayerHandRels (EntityId 'Bout)
phRelsBelongsToBout f (PlayerHandRels spot round player bout) = fmap (PlayerHandRels spot round player) (f bout)

phRelsBelongsToPlayerSpot :: Lens' PlayerHandRels (EntityId 'PlayerSpot)
phRelsBelongsToPlayerSpot f (PlayerHandRels spot round player bout) = fmap (\s -> PlayerHandRels s round player bout) (f spot)

phRelsBelongsToDealerRound :: Lens' PlayerHandRels (EntityId 'DealerRound)
phRelsBelongsToDealerRound f (PlayerHandRels spot round player bout) = fmap (\r -> PlayerHandRels spot r player bout) (f round)

phRelsOwnedByPlayer :: Lens' PlayerHandRels (EntityId 'Player)
phRelsOwnedByPlayer f (PlayerHandRels spot round player bout) = fmap (\p -> PlayerHandRels spot round p bout) (f player)

-- EPlayerSpot
psAttrs :: Lens' (EntityState 'PlayerSpot) PlayerSpotAttrs
psAttrs f (EPlayerSpot a m r) = fmap (\a' -> EPlayerSpot a' m r) (f a)

psModes :: Lens' (EntityState 'PlayerSpot) PlayerSpotModes
psModes f (EPlayerSpot a m r) = fmap (\m' -> EPlayerSpot a m' r) (f m)

psRels :: Lens' (EntityState 'PlayerSpot) PlayerSpotRels
psRels f (EPlayerSpot a m r) = fmap (EPlayerSpot a m) (f r)

psAttrsSpotIndex :: Lens' PlayerSpotAttrs PlayerSpotIx
psAttrsSpotIndex f (PlayerSpotAttrs ix wager) = fmap (`PlayerSpotAttrs` wager) (f ix)

psAttrsWager :: Lens' PlayerSpotAttrs Chips
psAttrsWager f (PlayerSpotAttrs ix wager) = fmap (PlayerSpotAttrs ix) (f wager)

psModesPlayerSpot :: Lens' PlayerSpotModes SomePlayerSpotFSM
psModesPlayerSpot f (PlayerSpotModes fsm) = fmap PlayerSpotModes (f fsm)

psEntityRelsPlayerId :: Lens' PlayerSpotRels (EntityId 'Player)
psEntityRelsPlayerId f (PlayerSpotRels player round occupancy) = fmap (\p -> PlayerSpotRels p round occupancy) (f player)

psEntityRelsRoundId :: Lens' PlayerSpotRels (EntityId 'DealerRound)
psEntityRelsRoundId f (PlayerSpotRels player round occupancy) = fmap (\r -> PlayerSpotRels player r occupancy) (f round)

psRelsHandOccupancy :: Lens' PlayerSpotRels (FiniteMap PlayerSpotHandIx (Occupancy (EntityId 'PlayerHand)))
psRelsHandOccupancy f (PlayerSpotRels player round occupancy) = fmap (PlayerSpotRels player round) (f occupancy)

-- ETable
tAttrs :: Lens' (EntityState 'Table) TableAttrs
tAttrs f (ETable a m r) = fmap (\a' -> ETable a' m r) (f a)

tModes :: Lens' (EntityState 'Table) TableModes
tModes f (ETable a m r) = fmap (\m' -> ETable a m' r) (f m)

tRels :: Lens' (EntityState 'Table) TableRels
tRels f (ETable a m r) = fmap (ETable a m) (f r)

tAttrsName :: Lens' TableAttrs String
tAttrsName f (TableAttrs name round offering) = fmap (\n -> TableAttrs n round offering) (f name)

tAttrsCurrentRound :: Lens' TableAttrs (Maybe (EntityId 'DealerRound))
tAttrsCurrentRound f (TableAttrs name round offering) = fmap (\r -> TableAttrs name r offering) (f round)

tAttrsOffering :: Lens' TableAttrs Offering
tAttrsOffering f (TableAttrs name round offering) = fmap (TableAttrs name round) (f offering)

tModesFSM :: Lens' TableModes SomeTableFSM
tModesFSM f (TableModes fsm) = fmap TableModes (f fsm)

tRelsManagedByDealer :: Lens' TableRels (Maybe (EntityId 'Dealer))
tRelsManagedByDealer f (TableRels dealer) = fmap TableRels (f dealer)

-- ETableShoe
tsAttrs :: Lens' (EntityState 'TableShoe) TableShoeAttrs
tsAttrs f (ETableShoe a m r) = fmap (\a' -> ETableShoe a' m r) (f a)

tsModes :: Lens' (EntityState 'TableShoe) TableShoeModes
tsModes f (ETableShoe a m r) = fmap (\m' -> ETableShoe a m' r) (f m)

tsRels :: Lens' (EntityState 'TableShoe) TableShoeRels
tsRels f (ETableShoe a m r) = fmap (ETableShoe a m) (f r)

tsAttrsCards :: Lens' TableShoeAttrs [Card]
tsAttrsCards f (TableShoeAttrs cards states) = fmap (`TableShoeAttrs` states) (f cards)

tsAttrsCardStates :: Lens' TableShoeAttrs (Map CardIx CardState)
tsAttrsCardStates f (TableShoeAttrs cards states) = fmap (TableShoeAttrs cards) (f states)

tsRelsTable :: Lens' TableShoeRels (EntityId 'Table)
tsRelsTable f (TableShoeRels table) = fmap TableShoeRels (f table)
