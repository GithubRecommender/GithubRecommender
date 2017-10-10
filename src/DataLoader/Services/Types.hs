{-# LANGUAGE DeriveGeneric #-}

module DataLoader.Services.Types where
import GHC.Generics
import Control.Exception
import Data.Text (Text)

newtype Id = Id { unId :: String } deriving (Eq, Show)

data RepositoryReference = RepositoryReference Id

data ServiceError        = ServiceError deriving (Eq, Show)
instance Exception ServiceError

class Identifyable a where
  id :: a -> Id

class Named a where
  name :: a -> Text
