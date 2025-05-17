{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Player.Delta
  ( module Pitboss.Trace.Entity.Player.Delta.Attrs,
    module Pitboss.Trace.Entity.Player.Delta.Modes,
    module Pitboss.Trace.Entity.Player.Delta.Rels,
    PlayerEntityDelta (..),
  )
where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Player
import Pitboss.Trace.Entity.Player.Delta.Attrs
import Pitboss.Trace.Entity.Player.Delta.Modes
import Pitboss.Trace.Entity.Player.Delta.Rels

data PlayerEntityDelta
  = PlayerEntityAttrsDelta PlayerEntityAttrsDelta
  | PlayerEntityModesDelta PlayerEntityModesDelta
  | PlayerEntityRelsDelta PlayerEntityRelsDelta
  deriving (Eq, Show, Generic)

instance ToJSON PlayerEntityDelta

instance FromJSON PlayerEntityDelta

instance Incremental PlayerEntityDelta where
  type Entity PlayerEntityDelta = Player

  applyDelta delta e = case delta of
    PlayerEntityAttrsDelta d -> e {_playerState = applyDelta d (_playerState e)}
    PlayerEntityModesDelta _ -> e -- no FSM yet
    PlayerEntityRelsDelta d -> e {_playerRels = applyDelta d (_playerRels e)}

  previewDelta delta e = Just $ applyDelta delta e

  describeDelta delta e = case delta of
    PlayerEntityAttrsDelta d -> describeDelta d (_playerState e)
    PlayerEntityModesDelta d -> describeDelta d ()
    PlayerEntityRelsDelta d -> describeDelta d (_playerRels e)

instance Reversible PlayerEntityDelta where
  invert = \case
    PlayerEntityAttrsDelta d -> PlayerEntityAttrsDelta <$> invert d
    PlayerEntityModesDelta d -> PlayerEntityModesDelta <$> invert d
    PlayerEntityRelsDelta d -> PlayerEntityRelsDelta <$> invert d
