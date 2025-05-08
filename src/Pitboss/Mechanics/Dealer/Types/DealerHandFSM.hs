{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pitboss.Mechanics.Dealer.Types.DealerHandFSM where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Mechanics.Types.Transitionable

data DealerHandResolution
  = DealerBlackjack
  | DealerStand
  | DealerBust
  deriving (Eq, Show, Generic)

instance ToJSON DealerHandResolution

instance FromJSON DealerHandResolution

data DealerHandPhase
  = Dealing
  | Evaluating
  | Resolved DealerHandResolution
  deriving (Eq, Show, Generic)

instance ToJSON DealerHandPhase

instance FromJSON DealerHandPhase

data DealerHandFSM (p :: DealerHandPhase) where
  DealingFSM :: DealerHandFSM 'Dealing
  EvaluatingFSM :: DealerHandFSM 'Evaluating
  ResolvedFSM :: DealerHandResolution -> DealerHandFSM ('Resolved r)

deriving instance Show (DealerHandFSM p)

deriving instance Eq (DealerHandFSM p)

instance Transitionable (DealerHandFSM p) where
  transitionType = \case
    DealingFSM -> AwaitInput
    EvaluatingFSM -> AutoAdvance
    ResolvedFSM _ -> TerminalPhase

data SomeDealerHandFSM = forall p. SomeDealerHandFSM (DealerHandFSM p)

instance Show SomeDealerHandFSM where
  show (SomeDealerHandFSM fsm) = "SomeDealerHandFSM (" ++ show fsm ++ ")"

instance Eq SomeDealerHandFSM where
  (SomeDealerHandFSM f1) == (SomeDealerHandFSM f2) = case (f1, f2) of
    (DealingFSM, DealingFSM) -> True
    (EvaluatingFSM, EvaluatingFSM) -> True
    (ResolvedFSM r1, ResolvedFSM r2) -> r1 == r2
    _ -> False

instance ToJSON SomeDealerHandFSM where
  toJSON (SomeDealerHandFSM fsm) = case fsm of
    DealingFSM -> object ["tag" .= String "Dealing"]
    EvaluatingFSM -> object ["tag" .= String "Evaluating"]
    ResolvedFSM r -> object ["tag" .= String "Resolved", "resolution" .= r]

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

mkDealerHandFSMResolved :: DealerHandResolution -> SomeDealerHandFSM
mkDealerHandFSMResolved res = SomeDealerHandFSM (ResolvedFSM res)
