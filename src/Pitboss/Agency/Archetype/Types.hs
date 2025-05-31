module Pitboss.Agency.Archetype.Types where

data ArchetypeKind
    = APlayerBasicStrategy
    | APlayerPerfect
    | APlayerAdvantage
    | ADealerRookie
    | ADealerByTheBook
    | ADealerVeteran
    deriving (Eq, Show)
