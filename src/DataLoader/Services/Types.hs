{-# LANGUAGE DeriveGeneric #-}

module DataLoader.Services.Types where
import Data.Aeson
import GHC.Generics
import Control.Exception
import Data.Text (Text)

newtype Id = Id { unId :: String } deriving (Eq, Show, Generic)

instance FromJSON Id

data RepositoryReference = RepositoryReference Id

data ServiceError = ServiceError
                  | WithMessage String deriving (Eq, Show)
instance Exception ServiceError

class Identifyable a where
  id :: a -> Id

class Named a where
  name :: a -> Text
