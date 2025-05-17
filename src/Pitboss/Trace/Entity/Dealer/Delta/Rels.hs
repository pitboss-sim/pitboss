{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Dealer.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Dealer
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data DealerRelationsDelta
  = UpdateRound (Maybe (EntityRef DealerRoundId)) (Maybe (EntityRef DealerRoundId))
  | UpdateHand (Maybe (EntityRef DealerHandId)) (Maybe (EntityRef DealerHandId))
  deriving (Eq, Show, Generic)

instance ToJSON DealerRelationsDelta

instance FromJSON DealerRelationsDelta

instance Incremental DealerRelationsDelta where
  type Entity DealerRelationsDelta = DealerRelations

  applyDelta delta rels = case delta of
    UpdateRound _ new -> rels {_dealerRelsCurrentRound = new}
    UpdateHand _ new -> rels {_dealerRelsActiveHand = new}

  previewDelta delta state = case delta of
    UpdateRound old _ ->
      if _dealerRelsCurrentRound state == old
        then Just $ applyDelta delta state
        else Nothing
    UpdateHand old _ ->
      if _dealerRelsActiveHand state == old
        then Just $ applyDelta delta state
        else Nothing

  describeDelta delta _ = case delta of
    UpdateRound old new ->
      "Updated dealer round: " ++ show old ++ " → " ++ show new
    UpdateHand old new ->
      "Updated dealer hand: " ++ show old ++ " → " ++ show new

instance Reversible DealerRelationsDelta where
  invert = \case
    UpdateRound old new -> Right (UpdateRound new old)
    UpdateHand old new -> Right (UpdateHand new old)
