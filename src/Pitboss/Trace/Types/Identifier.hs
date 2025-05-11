{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- globally unique identifiers

module Pitboss.Trace.Types.Identifier where

import Data.Aeson
import Data.Hashable
import GHC.Generics
import Pitboss.Trace.Types.Uid

newtype OfferingId = OfferingId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show OfferingId where show (OfferingId (Uid u)) = "F" ++ u

instance HasUid OfferingId where getUid (OfferingId u) = u

mkOfferingId :: Uid -> OfferingId
mkOfferingId = OfferingId

newtype DealerId = DealerId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show DealerId where show (DealerId (Uid u)) = "A" ++ u

instance HasUid DealerId where getUid (DealerId u) = u

mkDealerId :: Uid -> DealerId
mkDealerId = DealerId

newtype DealerHandId = DealerHandId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show DealerHandId where show (DealerHandId (Uid u)) = "A" ++ u

instance HasUid HandId where getUid (HandId u) = u

mkDealerHandId :: Uid -> DealerHandId
mkDealerHandId = DealerHandId

newtype PlayerId = PlayerId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show PlayerId where show (PlayerId (Uid u)) = "A" ++ u

instance HasUid PlayerId where getUid (PlayerId u) = u

mkPlayerId :: Uid -> PlayerId
mkPlayerId = PlayerId

newtype PlayerHandId = PlayerHandId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show PlayerHandId where show (PlayerHandId (Uid u)) = "A" ++ u

mkPlayerHandId :: Uid -> PlayerHandId
mkPlayerHandId = PlayerHandId

newtype TableId = TableId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show TableId where show (TableId (Uid u)) = "T" ++ u

instance HasUid TableId where getUid (TableId u) = u

mkTableId :: Uid -> TableId
mkTableId = TableId

newtype ShoeId = ShoeId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show ShoeId where show (ShoeId (Uid u)) = "S" ++ u

instance HasUid ShoeId where getUid (ShoeId u) = u

mkShoeId :: Uid -> ShoeId
mkShoeId = ShoeId

newtype ShoeCursorId = ShoeCursorId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show ShoeCursorId where show (ShoeCursorId (Uid u)) = "S" ++ u

instance HasUid ShoeCursorId where getUid (ShoeCursorId u) = u

mkShoeCursorId :: Uid -> ShoeId
mkShoeCursorId = ShoeId

newtype DealerRoundId = DealerRoundId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show DealerRoundId where show (DealerRoundId (Uid u)) = "R" ++ u

instance HasUid DealerRoundId where getUid (DealerRoundId u) = u

mkDealerRoundId :: Uid -> DealerRoundId
mkDealerRoundId = DealerRoundId

newtype PlayerSpotId = PlayerSpotId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show PlayerSpotId where show (PlayerSpotId (Uid u)) = "O" ++ u

instance HasUid PlayerSpotId where getUid (PlayerSpotId u) = u

mkPlayerSpotId :: Uid -> PlayerSpotId
mkPlayerSpotId = PlayerSpotId

newtype HandId = HandId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show HandId where show (HandId (Uid u)) = "H" ++ u

mkHandId :: Uid -> HandId
mkHandId = HandId
