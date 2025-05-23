{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Delta (
    module Pitboss.Trace.Entity.Dealer.Delta,
    module Pitboss.Trace.Entity.DealerHand.Delta,
    module Pitboss.Trace.Entity.DealerRound.Delta,
    module Pitboss.Trace.Entity.Offering.Delta,
    module Pitboss.Trace.Entity.Player.Delta,
    module Pitboss.Trace.Entity.PlayerHand.Delta,
    module Pitboss.Trace.Entity.PlayerSpot.Delta,
    module Pitboss.Trace.Entity.Table.Delta,
    module Pitboss.Trace.Entity.TableShoe.Delta,
    Delta (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Dealer.Delta
import Pitboss.Trace.Entity.DealerHand.Delta
import Pitboss.Trace.Entity.DealerRound.Delta
import Pitboss.Trace.Entity.Offering.Delta
import Pitboss.Trace.Entity.Player.Delta
import Pitboss.Trace.Entity.PlayerHand.Delta
import Pitboss.Trace.Entity.PlayerSpot.Delta
import Pitboss.Trace.Entity.Table.Delta
import Pitboss.Trace.Entity.TableShoe.Delta
import Pitboss.Trace.Entity.Types

data family Delta (k :: EntityKind)

data instance Delta 'Dealer
    = DDealerAttrs' DDealerAttrs
    | DDealerModes' DDealerModes
    | DDealerRels' DDealerRels
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'Dealer)
instance FromJSON (Delta 'Dealer)

data instance Delta 'DealerHand
    = DDealerHandAttrs' DDealerHandAttrs
    | DDealerHandModes' DDealerHandModes
    | DDealerHandRels' DDealerHandRels
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'DealerHand)
instance FromJSON (Delta 'DealerHand)

data instance Delta 'DealerRound
    = DDealerRoundAttrs' DDealerRoundAttrs
    | DDealerRoundModes' DDealerRoundModes
    | DDealerRoundRels' DDealerRoundRels
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'DealerRound)
instance FromJSON (Delta 'DealerRound)

data instance Delta 'Offering
    = DOfferingAttrs' DOfferingAttrs
    | DOfferingModes' DOfferingModes
    | DOfferingRels' DOfferingRels
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'Offering)
instance FromJSON (Delta 'Offering)

data instance Delta 'Player
    = DPlayerAttrs' DPlayerAttrs
    | DPlayerModes' DPlayerModes
    | DPlayerRels' DPlayerRels
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'Player)
instance FromJSON (Delta 'Player)

data instance Delta 'PlayerHand
    = DPlayerHandAttrs' DPlayerHandAttrs
    | DPlayerHandModes' DPlayerHandModes
    | DPlayerHandRels' DPlayerHandRels
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'PlayerHand)
instance FromJSON (Delta 'PlayerHand)

data instance Delta 'PlayerSpot
    = DPlayerSpotAttrs' DPlayerSpotAttrs
    | DPlayerSpotModes' DPlayerSpotModes
    | DPlayerSpotRels' DPlayerSpotRels
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'PlayerSpot)
instance FromJSON (Delta 'PlayerSpot)

data instance Delta 'Table
    = DTableAttrs' DTableAttrs
    | DTableModes' DTableModes
    | DTableRels' DTableRels
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'Table)
instance FromJSON (Delta 'Table)

data instance Delta 'TableShoe
    = DTableShoeAttrs' DTableShoeAttrs
    | DTableShoeModes' DTableShoeModes
    | DTableShoeRels' DTableShoeRels
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'TableShoe)
instance FromJSON (Delta 'TableShoe)
