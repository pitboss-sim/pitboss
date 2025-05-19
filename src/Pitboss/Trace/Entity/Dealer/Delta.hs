{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.Trace.Entity.Dealer.Delta where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import Pitboss.FSM.DealerHand
import Pitboss.FSM.DealerRound
import Pitboss.FSM.DealerTable
import Pitboss.Trace.Entity.Types.EntityId

data DealerEntityAttrsDelta
    = RenameDealer String String
    | ReplaceAssignedTable (Maybe (ClockedRef TableEntityId)) (Maybe (ClockedRef TableEntityId))
    deriving (Eq, Show, Generic)

instance ToJSON DealerEntityAttrsDelta
instance FromJSON DealerEntityAttrsDelta

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

data DealerEntityRelsDelta
    = UpdateRound (Maybe (ClockedRef DealerRoundEntityId)) (Maybe (ClockedRef DealerRoundEntityId))
    | UpdateHand (Maybe (ClockedRef DealerHandEntityId)) (Maybe (ClockedRef DealerHandEntityId))
    deriving (Eq, Show, Generic)

instance ToJSON DealerEntityRelsDelta
instance FromJSON DealerEntityRelsDelta
