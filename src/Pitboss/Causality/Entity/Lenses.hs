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

-- Bout lens
bAttrs :: Lens' (EntityState 'Bout) BoutAttrs
bAttrs f (EBout a m r) = fmap (\a' -> EBout a' m r) (f a)

bModes :: Lens' (EntityState 'Bout) BoutModes
bModes f (EBout a m r) = fmap (\m' -> EBout a m' r) (f m)

bRels :: Lens' (EntityState 'Bout) BoutRels
bRels f (EBout a m r) = fmap (EBout a m) (f r)

bAttrsPlayerHand :: Lens' BoutAttrs SomeHand
bAttrsPlayerHand f (BoutAttrs playerHand dealerHand activeHandIx outcome) =
    fmap (\ph -> BoutAttrs ph dealerHand activeHandIx outcome) (f playerHand)

bAttrsDealerHand :: Lens' BoutAttrs SomeHand
bAttrsDealerHand f (BoutAttrs playerHand dealerHand activeHandIx outcome) =
    fmap (\dh -> BoutAttrs playerHand dh activeHandIx outcome) (f dealerHand)

bAttrsActiveHandIx :: Lens' BoutAttrs HandIx
bAttrsActiveHandIx f (BoutAttrs playerHand dealerHand activeHandIx outcome) =
    fmap (\ix -> BoutAttrs playerHand dealerHand ix outcome) (f activeHandIx)

bAttrsOutcome :: Lens' BoutAttrs (Occupancy DetailedOutcome)
bAttrsOutcome f (BoutAttrs playerHand dealerHand activeHandIx outcome) =
    fmap (BoutAttrs playerHand dealerHand activeHandIx) (f outcome)

bModesBoutFSM :: Lens' BoutModes SomeBoutFSM
bModesBoutFSM f (BoutModes boutFSM playerHandFSM dealerHandFSM) =
    fmap (\b -> BoutModes b playerHandFSM dealerHandFSM) (f boutFSM)

bModesPlayerHandFSM :: Lens' BoutModes SomePlayerHandFSM
bModesPlayerHandFSM f (BoutModes boutFSM playerHandFSM dealerHandFSM) =
    fmap (\p -> BoutModes boutFSM p dealerHandFSM) (f playerHandFSM)

bModesDealerHandFSM :: Lens' BoutModes SomeDealerHandFSM
bModesDealerHandFSM f (BoutModes boutFSM playerHandFSM dealerHandFSM) =
    fmap (BoutModes boutFSM playerHandFSM) (f dealerHandFSM)

bRelsPlayer :: Lens' BoutRels PlayerId
bRelsPlayer f (BoutRels player dealer round table playerBouts) =
    fmap (\p -> BoutRels p dealer round table playerBouts) (f player)

bRelsDealer :: Lens' BoutRels DealerId
bRelsDealer f (BoutRels player dealer round table playerBouts) =
    fmap (\d -> BoutRels player d round table playerBouts) (f dealer)

bRelsRound :: Lens' BoutRels RoundId
bRelsRound f (BoutRels player dealer round table playerBouts) =
    fmap (\r -> BoutRels player dealer r table playerBouts) (f round)

bRelsTable :: Lens' BoutRels TableId
bRelsTable f (BoutRels player dealer round table playerBouts) =
    fmap (\t -> BoutRels player dealer round t playerBouts) (f table)

bRelsPlayerBouts :: Lens' BoutRels (FiniteMap HandIx (Occupancy BoutId))
bRelsPlayerBouts f (BoutRels player dealer round table playerBouts) =
    fmap (BoutRels player dealer round table) (f playerBouts)

-- Dealer lens
dAttrs :: Lens' (EntityState 'Dealer) DealerAttrs
dAttrs f (EDealer a m r) = fmap (\a' -> EDealer a' m r) (f a)

dModes :: Lens' (EntityState 'Dealer) DealerModes
dModes f (EDealer a m r) = fmap (\m' -> EDealer a m' r) (f m)

dRels :: Lens' (EntityState 'Dealer) DealerRels
dRels f (EDealer a m r) = fmap (EDealer a m) (f r)

dAttrsName :: Lens' DealerAttrs String
dAttrsName f (DealerAttrs name stagedCard) = fmap (`DealerAttrs` stagedCard) (f name)

dAttrsStagedCard :: Lens' DealerAttrs (Occupancy Card)
dAttrsStagedCard f (DealerAttrs name stagedCard) = fmap (DealerAttrs name) (f stagedCard)

dModesDealerTable :: Lens' DealerModes SomeDealerTableFSM
dModesDealerTable f (DealerModes dealerTable round) = fmap (`DealerModes` round) (f dealerTable)

dModesRound :: Lens' DealerModes RoundFSM
dModesRound f (DealerModes dealerTable round) = fmap (DealerModes dealerTable) (f round)

dRelsActiveRound :: Lens' DealerRels (Occupancy RoundId)
dRelsActiveRound f (DealerRels activeRound activeTable) =
    fmap (`DealerRels` activeTable) (f activeRound)

dRelsActiveTable :: Lens' DealerRels (Occupancy TableId)
dRelsActiveTable f (DealerRels activeRound activeTable) =
    fmap (DealerRels activeRound) (f activeTable)

-- Player lens
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

-- Round lens
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

rRelsBouts :: Lens' RoundRels (FiniteMap TableSpotIx (Occupancy BoutId))
rRelsBouts f (RoundRels boutPlayers shoe table) =
    fmap (\c -> RoundRels c shoe table) (f boutPlayers)

rRelsShoe :: Lens' RoundRels ShoeId
rRelsShoe f (RoundRels boutPlayers shoe table) =
    fmap (\s -> RoundRels boutPlayers s table) (f shoe)

rRelsTable :: Lens' RoundRels TableId
rRelsTable f (RoundRels boutPlayers shoe table) =
    fmap (RoundRels boutPlayers shoe) (f table)

-- Shoe lens
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

-- Table lens
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
