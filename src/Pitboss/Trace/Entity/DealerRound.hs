{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.DealerRound where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkDealerRoundState :: Int -> DealerRoundState
mkDealerRoundState n = DealerRoundState n False

mkDealerRound :: Meta DealerRoundId -> DealerRoundState -> DealerRoundRelations -> DealerRound
mkDealerRound = DealerRound

mkDealerRoundRelations :: EntityRef ShoeId -> DealerRoundRelations
mkDealerRoundRelations = DealerRoundRelations

data DealerRound = DealerRound
  { _dealerRoundMeta :: Meta DealerRoundId,
    _dealerRoundState :: DealerRoundState,
    _dealerRoundRels :: DealerRoundRelations
  }
  deriving (Eq, Show, Generic)

data DealerRoundState = DealerRoundState
  { _dealerRoundStateNumber :: Int,
    _dealerRoundStateIsActive :: Bool
  }
  deriving (Eq, Show, Generic)

data DealerRoundRelations = DealerRoundRelations
  { _dealerRoundRelsShoeUsed :: EntityRef ShoeId
  }
  deriving (Eq, Show, Generic)

instance ToJSON DealerRound

instance FromJSON DealerRound

instance ToJSON DealerRoundState

instance FromJSON DealerRoundState

instance ToJSON DealerRoundRelations

instance FromJSON DealerRoundRelations
