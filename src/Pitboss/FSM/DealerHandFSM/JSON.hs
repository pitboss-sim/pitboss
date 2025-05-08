{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Pitboss.FSM.DealerHandFSM.JSON where

import Data.Aeson
import Pitboss.FSM.DealerHandFSM.Existential
import Pitboss.FSM.DealerHandFSM.Types

instance ToJSON DealerHandResolution

instance FromJSON DealerHandResolution

instance ToJSON DealerHandPhase

instance FromJSON DealerHandPhase

instance ToJSON SomeDealerHandFSM where
  toJSON (SomeDealerHandFSM fsm) = case fsm of
    DealingFSM -> object ["tag" .= String "Dealing"]
    EvaluatingFSM -> object ["tag" .= String "Evaluating"]
    ResolvedFSM r -> object ["tag" .= String "Resolved", "resolution" .= r]
    InterruptedFSM r -> object ["tag" .= String "Interrupted", "reason" .= r]

instance FromJSON SomeDealerHandFSM where
  parseJSON = withObject "SomeDealerHandFSM" $ \obj -> do
    tag <- obj .: "tag"
    case tag of
      "Dealing" -> pure $ SomeDealerHandFSM DealingFSM
      "Evaluating" -> pure $ SomeDealerHandFSM EvaluatingFSM
      "Resolved" -> do
        r <- obj .: "resolution"
        pure $ SomeDealerHandFSM (ResolvedFSM r)
      other -> fail $ "Unknown tag for SomeDealerHandFSM: " ++ other
