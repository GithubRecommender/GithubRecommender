{-# LANGUAGE DeriveGeneric #-}

module DataLoader.Services.RepositoryEvents where
import GHC.Generics
import Control.Exception
import Data.Text (Text)
import DataLoader.Services.Types

newtype BatchSize = BatchSize { unSize :: Int }

data RepositoryEvent = RepositoryEvent {
     repoId    :: Id
   , repoName  :: Text
   , ownerName :: Text
 } deriving (Eq, Show, Generic)

instance Identifyable RepositoryEvent where id = repoId
instance Named RepositoryEvent where name = repoName

class RepositoryEventService s where
  fetchSingle :: s -> Either ServiceError RepositoryEvent
  fetchBatch  :: (Traversable t) => s -> BatchSize -> Either ServiceError (t RepositoryEvent)
