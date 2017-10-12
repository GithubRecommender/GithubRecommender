{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module DataMining.DataSource.RepositoryData.Types where

import GHC.Generics
import Control.Exception
import Control.Lens (makeLenses)
import Data.Text (Text)
import Internal.Types
import DataMining.DataSource.Types

data RepositoryData = RepositoryData {
     _repoId         :: ID
   , _repoName       :: Text
   , _repoIsFork     :: Bool
   , _repoIPrivate   :: Bool
   , _repoIsMirror   :: Bool
   , _repoIsArchived :: Bool
   , _repoIsLocked   :: Bool
   , _repoLanguages  :: [RepositoryLanguage]
   , _repoTopics     :: [RepositoryTopic]
 } deriving (Eq, Show, Generic)

data RepositoryLanguage = RepositoryLanguage {
    _repoLanguageId :: ID
  , _repoLanguageName :: Text
 } deriving (Eq, Show, Generic)

data RepositoryTopic = RepositoryTopic {
     _repoTopicId       :: ID
   , _repoTopicName     :: Text
   , _repoRelatedTopics :: [RepositoryTopic]
 } deriving (Eq, Show, Generic)

makeLenses ''RepositoryData
makeLenses ''RepositoryLanguage
makeLenses ''RepositoryTopic

class (Exception e, Show e) => RepositoryDataSource s e where
  fetchSingle   :: s -> RepositoryReference -> IO (Either (DataSourceError e) RepositoryData)
