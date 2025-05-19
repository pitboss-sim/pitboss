{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Pitboss.Causality.Entity.Lenses where

import Control.Lens hiding (ix)
import Data.Map.Strict (Map)
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality.Entity.Types
import Pitboss.Causality.Types.Core
import Pitboss.Causality.Types.FiniteMap
import Pitboss.FSM
import Prelude hiding (round)

-- Bout relationship
bAttrs :: Lens' (EntityState 'Bout) BoutAttrs
bAttrs f (EBout a m r) = fmap (\a' -> EBout a' m r) (f a)

bModes :: Lens' (EntityState 'Bout) BoutModes
bModes f (EBout a m r) = fmap (\m' -> EBout a m' r) (f m)

bRels :: Lens' (EntityState 'Bout) BoutRels
bRels f (EBout a m r) = fmap (EBout a m) (f r)

bAttrsOutcome :: Lens' BoutAttrs (Occupancy DetailedOutcome)
bAttrsOutcome f (BoutAttrs outcome) = fmap BoutAttrs (f outcome)

bModesFSM :: Lens' BoutModes SomeBoutFSM
bModesFSM f (BoutModes fsm) = fmap BoutModes (f fsm)

bRelsPlayerHand :: Lens' BoutRels HandId
bRelsPlayerHand f (BoutRels playerHand dealerHand round shoe table) =
    fmap (\ph -> BoutRels ph dealerHand round shoe table) (f playerHand)

bRelsDealerHand :: Lens' BoutRels HandId
bRelsDealerHand f (BoutRels playerHand dealerHand round shoe table) =
    fmap (\dh -> BoutRels playerHand dh round shoe table) (f dealerHand)

bRelsRound :: Lens' BoutRels RoundId
bRelsRound f (BoutRels playerHand dealerHand round shoe table) =
    fmap (\r -> BoutRels playerHand dealerHand r shoe table) (f round)

bRelsShoe :: Lens' BoutRels ShoeId
bRelsShoe f (BoutRels playerHand dealerHand round shoe table) =
    fmap (\s -> BoutRels playerHand dealerHand round s table) (f shoe)

bRelsTable :: Lens' BoutRels TableId
bRelsTable f (BoutRels playerHand dealerHand round shoe table) =
    fmap (BoutRels playerHand dealerHand round shoe) (f table)

-- Contestant relationship
cAttrs :: Lens' (EntityState 'Contestant) ContestantAttrs
cAttrs f (EContestant a m r) = fmap (\a' -> EContestant a' m r) (f a)

cModes :: Lens' (EntityState 'Contestant) ContestantModes
cModes f (EContestant a m r) = fmap (\m' -> EContestant a m' r) (f m)

cRels :: Lens' (EntityState 'Contestant) ContestantRels
cRels f (EContestant a m r) = fmap (EContestant a m) (f r)

cAttrsActiveHandIx :: Lens' ContestantAttrs HandIx
cAttrsActiveHandIx f (ContestantAttrs activeHandIx) = fmap ContestantAttrs (f activeHandIx)

cModesBoutFSM :: Lens' ContestantModes (Occupancy SomeContestantBoutFSM)
cModesBoutFSM f (ContestantModes boutFSM roundFSM) = fmap (`ContestantModes` roundFSM) (f boutFSM)

cModesRoundFSM :: Lens' ContestantModes SomeContestantRoundFSM
cModesRoundFSM f (ContestantModes boutFSM roundFSM) = fmap (ContestantModes boutFSM) (f roundFSM)

cRelsBouts :: Lens' ContestantRels (FiniteMap HandIx (Occupancy BoutId))
cRelsBouts f (ContestantRels bouts player round shoe) =
    fmap (\b -> ContestantRels b player round shoe) (f bouts)

cRelsPlayer :: Lens' ContestantRels PlayerId
cRelsPlayer f (ContestantRels bouts player round shoe) =
    fmap (\p -> ContestantRels bouts p round shoe) (f player)

cRelsRound :: Lens' ContestantRels RoundId
cRelsRound f (ContestantRels bouts player round shoe) =
    fmap (\r -> ContestantRels bouts player r shoe) (f round)

cRelsShoe :: Lens' ContestantRels ShoeId
cRelsShoe f (ContestantRels bouts player round shoe) =
    fmap (ContestantRels bouts player round) (f shoe)

-- Dealer relationship
dAttrs :: Lens' (EntityState 'Dealer) DealerAttrs
dAttrs f (EDealer a m r) = fmap (\a' -> EDealer a' m r) (f a)

dModes :: Lens' (EntityState 'Dealer) DealerModes
dModes f (EDealer a m r) = fmap (\m' -> EDealer a m' r) (f m)

dRels :: Lens' (EntityState 'Dealer) DealerRels
dRels f (EDealer a m r) = fmap (EDealer a m) (f r)

dAttrsName :: Lens' DealerAttrs String
dAttrsName f (DealerAttrs name) = fmap DealerAttrs (f name)

dModesDealerTable :: Lens' DealerModes SomeDealerTableFSM
dModesDealerTable f (DealerModes dealerTable round) = fmap (`DealerModes` round) (f dealerTable)

dModesRound :: Lens' DealerModes RoundFSM
dModesRound f (DealerModes dealerTable round) = fmap (DealerModes dealerTable) (f round)

dRelsActiveHand :: Lens' DealerRels (Occupancy HandId)
dRelsActiveHand f (DealerRels activeHand activeRound activeTable) =
    fmap (\h -> DealerRels h activeRound activeTable) (f activeHand)

dRelsActiveRound :: Lens' DealerRels (Occupancy RoundId)
dRelsActiveRound f (DealerRels activeHand activeRound activeTable) =
    fmap (\r -> DealerRels activeHand r activeTable) (f activeRound)

dRelsActiveTable :: Lens' DealerRels (Occupancy TableId)
dRelsActiveTable f (DealerRels activeHand activeRound activeTable) =
    fmap (DealerRels activeHand activeRound) (f activeTable)

-- Hand relationship
hAttrs :: Lens' (EntityState 'Hand) HandAttrs
hAttrs f (EHand a m r) = fmap (\a' -> EHand a' m r) (f a)

hModes :: Lens' (EntityState 'Hand) HandModes
hModes f (EHand a m r) = fmap (\m' -> EHand a m' r) (f m)

hRels :: Lens' (EntityState 'Hand) HandRels
hRels f (EHand a m r) = fmap (EHand a m) (f r)

hAttrsHand :: Lens' HandAttrs SomeHand
hAttrsHand f (HandAttrs hand) = fmap HandAttrs (f hand)

hModesHandFSM :: Lens' HandModes SomeHandFSM
hModesHandFSM f (HandModes handFSM) = fmap HandModes (f handFSM)

hRelsOwner :: Lens' HandRels HandOwner
hRelsOwner f (HandRels owner bout shoe) = fmap (\o -> HandRels o bout shoe) (f owner)

hRelsBout :: Lens' HandRels BoutId
hRelsBout f (HandRels owner bout shoe) = fmap (\b -> HandRels owner b shoe) (f bout)

hRelsShoe :: Lens' HandRels ShoeId
hRelsShoe f (HandRels owner bout shoe) = fmap (HandRels owner bout) (f shoe)

-- Player relationship
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

pModesPlayerFSM :: Lens' PlayerModes SomePlayerFSM
pModesPlayerFSM f (PlayerModes playerFSM) = fmap PlayerModes (f playerFSM)

pRelsActiveRound :: Lens' PlayerRels (Occupancy RoundId)
pRelsActiveRound f (PlayerRels activeRound activeTable) =
    fmap (`PlayerRels` activeTable) (f activeRound)

pRelsActiveTable :: Lens' PlayerRels (Occupancy TableId)
pRelsActiveTable f (PlayerRels activeRound activeTable) =
    fmap (PlayerRels activeRound) (f activeTable)

-- Round relationship
rAttrs :: Lens' (EntityState 'Round) RoundAttrs
rAttrs f (ERound a m r) = fmap (\a' -> ERound a' m r) (f a)

rModes :: Lens' (EntityState 'Round) RoundModes
rModes f (ERound a m r) = fmap (\m' -> ERound a m' r) (f m)

rRels :: Lens' (EntityState 'Round) RoundRels
rRels f (ERound a m r) = fmap (ERound a m) (f r)

rAttrsActiveSpotIx :: Lens' RoundAttrs (Occupancy TableSpotIx)
rAttrsActiveSpotIx f (RoundAttrs activeSpotIx number) = fmap (`RoundAttrs` number) (f activeSpotIx)

rAttrsNumber :: Lens' RoundAttrs Int
rAttrsNumber f (RoundAttrs activeSpotIx number) = fmap (RoundAttrs activeSpotIx) (f number)

rRelsDealerHand :: Lens' RoundRels (Occupancy HandId)
rRelsDealerHand f (RoundRels dealerHand contestants shoe table) =
    fmap (\dh -> RoundRels dh contestants shoe table) (f dealerHand)

rRelsContestants :: Lens' RoundRels (FiniteMap TableSpotIx (Occupancy ContestantId))
rRelsContestants f (RoundRels dealerHand contestants shoe table) =
    fmap (\c -> RoundRels dealerHand c shoe table) (f contestants)

rRelsShoe :: Lens' RoundRels ShoeId
rRelsShoe f (RoundRels dealerHand contestants shoe table) =
    fmap (\s -> RoundRels dealerHand contestants s table) (f shoe)

rRelsTable :: Lens' RoundRels TableId
rRelsTable f (RoundRels dealerHand contestants shoe table) =
    fmap (RoundRels dealerHand contestants shoe) (f table)

-- Shoe relationship
sAttrs :: Lens' (EntityState 'Shoe) ShoeAttrs
sAttrs f (EShoe a m r) = fmap (\a' -> EShoe a' m r) (f a)

sModes :: Lens' (EntityState 'Shoe) ShoeModes
sModes f (EShoe a m r) = fmap (\m' -> EShoe a m' r) (f m)

sRels :: Lens' (EntityState 'Shoe) ShoeRels
sRels f (EShoe a m r) = fmap (EShoe a m) (f r)

sAttrsCards :: Lens' ShoeAttrs [Card]
sAttrsCards f (ShoeAttrs cards cardStates) = fmap (`ShoeAttrs` cardStates) (f cards)

sAttrsCardStates :: Lens' ShoeAttrs (Map CardIx CardState)
sAttrsCardStates f (ShoeAttrs cards cardStates) = fmap (ShoeAttrs cards) (f cardStates)

sRelsTable :: Lens' ShoeRels TableId
sRelsTable f (ShoeRels table) = fmap ShoeRels (f table)

-- Table relationship
tAttrs :: Lens' (EntityState 'Table) TableAttrs
tAttrs f (ETable a m r) = fmap (\a' -> ETable a' m r) (f a)

tModes :: Lens' (EntityState 'Table) TableModes
tModes f (ETable a m r) = fmap (\m' -> ETable a m' r) (f m)

tRels :: Lens' (EntityState 'Table) TableRels
tRels f (ETable a m r) = fmap (ETable a m) (f r)

tAttrsName :: Lens' TableAttrs String
tAttrsName f (TableAttrs name offering seats) = fmap (\n -> TableAttrs n offering seats) (f name)

tAttrsOffering :: Lens' TableAttrs Offering
tAttrsOffering f (TableAttrs name offering seats) = fmap (\o -> TableAttrs name o seats) (f offering)

tAttrsSeats :: Lens' TableAttrs (FiniteMap TableSpotIx (Occupancy TableSeat))
tAttrsSeats f (TableAttrs name offering seats) = fmap (TableAttrs name offering) (f seats)

tModesFSM :: Lens' TableModes SomeTableFSM
tModesFSM f (TableModes fsm) = fmap TableModes (f fsm)

tRelsActiveDealer :: Lens' TableRels (Occupancy DealerId)
tRelsActiveDealer f (TableRels activeDealer activeRound) =
    fmap (`TableRels` activeRound) (f activeDealer)

tRelsActiveRound :: Lens' TableRels (Occupancy RoundId)
tRelsActiveRound f (TableRels activeDealer activeRound) =
    fmap (TableRels activeDealer) (f activeRound)
