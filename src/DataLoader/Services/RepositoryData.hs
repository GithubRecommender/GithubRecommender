{-# LANGUAGE DeriveGeneric #-}

module DataLoader.Services.RepositoryData where
import GHC.Generics
import Control.Exception
import Data.Text (Text)
import DataLoader.Services.Types

data RepositoryData = RepositoryData {
     repoId     :: Id
   , repoName   :: Text
   , isFork     :: Bool
   , isPrivate  :: Bool
   , isMirror   :: Bool
   , isArchived :: Bool
   , isLocked   :: Bool
   , languages  :: [RepositoryLanguage]
   , topics     :: [RepositoryTopic]
 } deriving (Eq, Show, Generic)

instance Identifyable RepositoryData where id = repoId
instance Named RepositoryData where name = repoName

data RepositoryLanguage = RepositoryLanguage {
    languageId :: Id
  , languageName :: Text
 } deriving (Eq, Show, Generic)

instance Identifyable RepositoryLanguage where id = languageId
instance Named RepositoryLanguage where name = languageName

data RepositoryTopic = RepositoryTopic {
     topicId       :: Id
   , topicName     :: Text
   , relatedTopics :: [RepositoryTopic]
 } deriving (Eq, Show, Generic)

instance Identifyable RepositoryTopic where id = topicId
instance Named RepositoryTopic where name = topicName

class RepositoryDataService s where
  fetchSingle   :: s -> RepositoryReference -> Either ServiceError RepositoryData
  fetchMultiple :: (Traversable t) => s -> t RepositoryReference -> Either ServiceError (t RepositoryData)
