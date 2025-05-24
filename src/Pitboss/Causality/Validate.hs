{-# LANGUAGE DataKinds #-}

module Pitboss.Causality.Validate where

import Control.Monad.Reader
import Pitboss.Causality.TickCache

data Validation e a = Failure e | Success a
    deriving (Eq, Show, Functor)

instance (Semigroup e) => Applicative (Validation e) where
    pure = Success
    Success f <*> Success a = Success (f a)
    Success _ <*> Failure e = Failure e
    Failure e <*> Success _ = Failure e
    Failure e1 <*> Failure e2 = Failure (e1 <> e2)

data EntityLookupError
    = EntityNotFound String
    | WrongOwnerType String String
    | InvalidFSMState String
    | TemporalCoherenceViolation String
    deriving (Eq, Show)

newtype ValidationErrors = ValidationErrors [EntityLookupError]
    deriving (Eq, Show)

instance Semigroup ValidationErrors where
    ValidationErrors a <> ValidationErrors b = ValidationErrors (a <> b)

instance Monoid ValidationErrors where
    mempty = ValidationErrors []

type ValidatedReader a = Reader TickCacheContext (Validation ValidationErrors a)

derefV :: (Deref id (Reader TickCacheContext), Show id) => id -> ValidatedReader (DerefTarget id)
derefV entityId = do
    result <- deref entityId
    pure $ case result of
        Just entity -> Success entity
        Nothing -> Failure $ ValidationErrors [EntityNotFound $ show entityId]
