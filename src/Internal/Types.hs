{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Internal.Types where
import Data.Text (Text)
import Control.Lens (makeLenses)
import GHC.Generics

newtype ID = ID {
  unID :: String
  } deriving (Eq, Show, Generic)

data RepositoryReference = RepositoryReference {
      _repositoryOwner :: Text
    , _repositoryName  :: Text
    } deriving (Eq, Show, Generic)


makeLenses ''RepositoryReference
