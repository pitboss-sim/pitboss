module Pitboss.Blackjack.Types.Hand.Witness where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Blackjack.Types.Core.Card

data ValueWitness = HardWitness | SoftWitness | BlackjackWitness | BustWitness
    deriving (Eq, Show, Generic)

instance Ord ValueWitness where
    compare BustWitness _ = LT
    compare _ BustWitness = GT
    compare BlackjackWitness BlackjackWitness = EQ
    compare BlackjackWitness _ = GT
    compare _ BlackjackWitness = LT
    compare SoftWitness HardWitness = GT
    compare HardWitness SoftWitness = LT
    compare SoftWitness SoftWitness = EQ
    compare HardWitness HardWitness = EQ

instance Ord StructureWitness where
    compare EmptyWitness _ = LT
    compare _ EmptyWitness = GT
    compare SingletonWitness SingletonWitness = EQ
    compare SingletonWitness _ = LT
    compare _ SingletonWitness = GT
    compare (PairWitness r1) (PairWitness r2) = compare r1 r2
    compare (PairWitness _) NonPairWitness = LT
    compare NonPairWitness (PairWitness _) = GT
    compare NonPairWitness NonPairWitness = EQ

data StructureWitness = EmptyWitness | SingletonWitness | PairWitness Rank | NonPairWitness
    deriving (Eq, Show, Generic)

data CompletenessWitness = NoneWitness | PartialWitness | FullWitness | ExtendedWitness
    deriving (Eq, Show, Generic)

data ActionWitness = CanHitWitness | CanStandWitness | CanDoubleWitness | CanSplitWitness | CanSurrenderWitness
    deriving (Eq, Show, Generic)

data HandWitness = HandWitness
    { valueType :: ValueWitness
    , structure :: StructureWitness
    , completeness :: CompletenessWitness
    , availableActions :: [ActionWitness]
    , numericValue :: Int
    }
    deriving (Eq, Show, Generic)

instance ToJSON ValueWitness
instance FromJSON ValueWitness

instance ToJSON StructureWitness
instance FromJSON StructureWitness

instance ToJSON CompletenessWitness
instance FromJSON CompletenessWitness

instance ToJSON ActionWitness
instance FromJSON ActionWitness

instance ToJSON HandWitness
instance FromJSON HandWitness
