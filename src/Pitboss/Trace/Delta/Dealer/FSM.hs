{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.Dealer.FSM where

import Data.Aeson
import Data.Text
import GHC.Generics
import Pitboss.FSM.DealerHandFSM
import Pitboss.FSM.DealerRoundFSM
import Pitboss.FSM.DealerTableFSM
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Entity.Dealer

data DealerFSMDelta
  = ReplaceTableFSM SomeDealerTableFSM SomeDealerTableFSM
  | ReplaceRoundFSM DealerRoundFSM DealerRoundFSM
  | ReplaceHandFSM SomeDealerHandFSM SomeDealerHandFSM
  deriving (Eq, Show, Generic)

instance Incremental DealerFSMDelta where
  type Entity DealerFSMDelta = Dealer

  applyDelta :: DealerFSMDelta -> Dealer -> Dealer
  applyDelta delta entity =
    let (ft, fr, fh) = (_fsmTable entity, _fsmRound entity, _fsmHand entity)
        (ft', fr', fh') = case delta of
          ReplaceTableFSM _ new -> (new, fr, fh)
          ReplaceRoundFSM _ new -> (ft, new, fh)
          ReplaceHandFSM _ new -> (ft, fr, new)
     in entity {_fsmTable = ft', _fsmRound = fr', _fsmHand = fh'}

  previewDelta delta entity = Just $ applyDelta delta entity

  previewDelta :: DealerFSMDelta -> Dealer -> Maybe Dealer
  describeDelta delta _ = case delta of
    ReplaceTableFSM _ _ ->
      "Replaced DealerTableFSM"
    ReplaceRoundFSM _ _ ->
      "Replaced DealerRoundFSM"
    ReplaceHandFSM _ _ ->
      "Replaced DealerHandFSM"

instance ToJSON DealerFSMDelta where
  toJSON = \case
    ReplaceTableFSM _ new ->
      object ["tag" .= String "ReplaceTableFSM", "new" .= new]
    ReplaceRoundFSM _ new ->
      object ["tag" .= String "ReplaceRoundFSM", "new" .= new]
    ReplaceHandFSM _ new ->
      object ["tag" .= String "ReplaceHandFSM", "new" .= new]

instance FromJSON DealerFSMDelta where
  parseJSON = withObject "DealerFSMDelta" $ \obj -> do
    tag <- obj .: "tag"
    case tag :: Text of
      "ReplaceTableFSM" ->
        ReplaceTableFSM undefined <$> obj .: "new"
      "ReplaceRoundFSM" ->
        ReplaceRoundFSM undefined <$> obj .: "new"
      "ReplaceHandFSM" ->
        ReplaceHandFSM undefined <$> obj .: "new"
      _ -> fail $ "Unknown tag for DealerFSMDelta: " ++ unpack tag

instance Reversible DealerFSMDelta where
  invert = \case
    ReplaceTableFSM old new -> Right (ReplaceTableFSM new old)
    ReplaceRoundFSM old new -> Right (ReplaceRoundFSM new old)
    ReplaceHandFSM old new -> Right (ReplaceHandFSM new old)
