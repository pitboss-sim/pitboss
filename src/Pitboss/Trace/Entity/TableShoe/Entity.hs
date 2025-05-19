{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.TableShoe.Entity where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Void (Void)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card
import Pitboss.Trace.Entity.Types.EntityId

mkETableShoeAttrs :: [Card] -> Map CardIx CardState -> ETableShoeAttrs
mkETableShoeAttrs = ETableShoeAttrs'

mkETableShoeModes :: ETableShoeModes
mkETableShoeModes = ETableShoeModes' undefined

mkETableShoeRels :: ClockedRef ETableId -> ETableShoeRels
mkETableShoeRels = ETableShoeRels'

type CardIx = Int

data CardState
    = InHand
    | InDiscard
    | Burned
    deriving (Eq, Show, Generic)

instance ToJSON CardState
instance FromJSON CardState

data ETableShoeAttrs = ETableShoeAttrs'
    { _tsAttrsCards :: [Card]
    , _tsAttrsCardStates :: Map CardIx CardState
    }
    deriving (Eq, Show, Generic)

data ETableShoeModes = ETableShoeModes' Void
    deriving (Eq, Show, Generic)

data ETableShoeRels = ETableShoeRels'
    { _tsRelsTable :: ClockedRef ETableId
    }
    deriving (Eq, Show, Generic)

instance ToJSON ETableShoeAttrs
instance FromJSON ETableShoeAttrs

instance ToJSON ETableShoeModes
instance FromJSON ETableShoeModes

instance ToJSON ETableShoeRels
instance FromJSON ETableShoeRels
