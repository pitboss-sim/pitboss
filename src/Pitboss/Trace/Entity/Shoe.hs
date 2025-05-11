{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.Shoe where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.Identifier

mkShoe :: Meta ShoeId -> [Card] -> Shoe
mkShoe meta cards = Shoe meta (ShoeState cards)

mkShoeState :: [Card] -> ShoeState
mkShoeState = ShoeState

mkShoeRelations :: a
mkShoeRelations = undefined

data Shoe = Shoe
  { _meta :: Meta ShoeId,
    _state :: ShoeState
  }
  deriving (Eq, Show, Generic)

data ShoeState = ShoeState
  { _shoeCards :: [Card]
  }
  deriving (Eq, Show, Generic)

instance ToJSON Shoe

instance FromJSON Shoe

instance ToJSON ShoeState

instance FromJSON ShoeState
