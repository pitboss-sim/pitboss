{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Dealer.Delta.Modes where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Pitboss.FSM.DealerHand
import Pitboss.FSM.DealerRound
import Pitboss.FSM.DealerTable
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Dealer.Types

data DealerEntityModesDelta
    = ReplaceTableFSM SomeDealerTableFSM SomeDealerTableFSM
    | ReplaceRoundFSM DealerRoundFSM DealerRoundFSM
    | ReplaceHandFSM SomeDealerHandFSM SomeDealerHandFSM
    deriving (Eq, Show, Generic)

instance ToJSON DealerEntityModesDelta where
    toJSON = \case
        ReplaceTableFSM _ new -> object ["tag" .= String "ReplaceTableFSM", "new" .= new]
        ReplaceRoundFSM _ new -> object ["tag" .= String "ReplaceRoundFSM", "new" .= new]
        ReplaceHandFSM _ new -> object ["tag" .= String "ReplaceHandFSM", "new" .= new]

instance FromJSON DealerEntityModesDelta where
    parseJSON = withObject "DealerEntityModesDelta" $ \obj -> do
        tag <- obj .: "tag"
        case tag :: Text of
            "ReplaceTableFSM" -> ReplaceTableFSM undefined <$> obj .: "new"
            "ReplaceRoundFSM" -> ReplaceRoundFSM undefined <$> obj .: "new"
            "ReplaceHandFSM" -> ReplaceHandFSM undefined <$> obj .: "new"
            _ -> fail $ "Unknown tag for DealerEntityModesDelta: " ++ show tag

instance Incremental DealerEntityModesDelta where
    type Target DealerEntityModesDelta = DealerEntityModes

    applyDelta delta entity =
        let (ft, fr, fh) = (_dealerEntityModesDealerTable entity, _dealerEntityModesDealerRound entity, _dealerEntityModesDealerHand entity)
            (ft', fr', fh') = case delta of
                ReplaceTableFSM _ new -> (new, fr, fh)
                ReplaceRoundFSM _ new -> (ft, new, fh)
                ReplaceHandFSM _ new -> (ft, fr, new)
         in entity
                { _dealerEntityModesDealerTable = ft'
                , _dealerEntityModesDealerRound = fr'
                , _dealerEntityModesDealerHand = fh'
                }

    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta :: DealerEntityModesDelta -> DealerEntityModes -> String
    describeDelta delta _ = case delta of
        ReplaceTableFSM _ new -> "Replaced DealerTableFSM with " ++ show new
        ReplaceRoundFSM _ new -> "Replaced DealerRoundFSM with " ++ show new
        ReplaceHandFSM _ new -> "Replaced DealerHandFSM with " ++ show new

instance Reversible DealerEntityModesDelta where
    invert = \case
        ReplaceTableFSM old new -> Right (ReplaceTableFSM new old)
        ReplaceRoundFSM old new -> Right (ReplaceRoundFSM new old)
        ReplaceHandFSM old new -> Right (ReplaceHandFSM new old)
