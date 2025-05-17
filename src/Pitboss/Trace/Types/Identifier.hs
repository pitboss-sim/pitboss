{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- globally unique identifiers

module Pitboss.Trace.Types.Identifier where

import Data.Aeson
import Data.Hashable
import GHC.Generics
import Pitboss.Trace.Types.Uid

mkOfferingEntityId :: Uid -> OfferingEntityId
mkOfferingEntityId = OfferingEntityId

mkTableEntityId :: Uid -> TableEntityId
mkTableEntityId = TableEntityId

mkTableShoeEntityId :: Uid -> TableShoeEntityId
mkTableShoeEntityId = TableShoeEntityId

mkTableShoeCursorEntityId :: Uid -> TableShoeEntityId
mkTableShoeCursorEntityId = TableShoeEntityId

mkDealerEntityId :: Uid -> DealerEntityId
mkDealerEntityId = DealerEntityId

mkDealerRoundEntityId :: Uid -> DealerRoundEntityId
mkDealerRoundEntityId = DealerRoundEntityId

mkDealerHandEntityId :: Uid -> DealerHandEntityId
mkDealerHandEntityId = DealerHandEntityId

mkPlayerEntityId :: Uid -> PlayerEntityId
mkPlayerEntityId = PlayerEntityId

mkPlayerSpotEntityId :: Uid -> PlayerSpotEntityId
mkPlayerSpotEntityId = PlayerSpotEntityId

mkPlayerHandEntityId :: Uid -> PlayerHandEntityId
mkPlayerHandEntityId = PlayerHandEntityId

newtype OfferingEntityId = OfferingEntityId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show OfferingEntityId where show (OfferingEntityId (Uid u)) = "F" ++ u

instance HasUid OfferingEntityId where getUid (OfferingEntityId u) = u

newtype TableEntityId = TableEntityId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show TableEntityId where show (TableEntityId (Uid u)) = "T" ++ u

instance HasUid TableEntityId where getUid (TableEntityId u) = u

newtype TableShoeEntityId = TableShoeEntityId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show TableShoeEntityId where show (TableShoeEntityId (Uid u)) = "S" ++ u

instance HasUid TableShoeEntityId where getUid (TableShoeEntityId u) = u

newtype TableShoeCursorEntityId = TableShoeCursorEntityId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show TableShoeCursorEntityId where show (TableShoeCursorEntityId (Uid u)) = "S" ++ u

instance HasUid TableShoeCursorEntityId where getUid (TableShoeCursorEntityId u) = u

newtype DealerEntityId = DealerEntityId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show DealerEntityId where show (DealerEntityId (Uid u)) = "A" ++ u

instance HasUid DealerEntityId where getUid (DealerEntityId u) = u

newtype DealerRoundEntityId = DealerRoundEntityId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show DealerRoundEntityId where show (DealerRoundEntityId (Uid u)) = "R" ++ u

instance HasUid DealerRoundEntityId where getUid (DealerRoundEntityId u) = u

newtype DealerHandEntityId = DealerHandEntityId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show DealerHandEntityId where show (DealerHandEntityId (Uid u)) = "A" ++ u

instance HasUid DealerHandEntityId where getUid (DealerHandEntityId u) = u

newtype PlayerEntityId = PlayerEntityId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show PlayerEntityId where show (PlayerEntityId (Uid u)) = "A" ++ u

instance HasUid PlayerEntityId where getUid (PlayerEntityId u) = u

newtype PlayerSpotEntityId = PlayerSpotEntityId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show PlayerSpotEntityId where show (PlayerSpotEntityId (Uid u)) = "O" ++ u

instance HasUid PlayerSpotEntityId where getUid (PlayerSpotEntityId u) = u

newtype PlayerHandEntityId = PlayerHandEntityId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show PlayerHandEntityId where show (PlayerHandEntityId (Uid u)) = "A" ++ u
