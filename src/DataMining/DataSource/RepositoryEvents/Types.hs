{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataMining.DataSource.RepositoryEvents.Types where

import Control.Exception
import Control.Lens (makeLenses)

import DataMining.DataSource.Types
import DataMining.DataSource.RepositoryEvents.GithubArchive.Download (ArchiveName)
import Internal.Types

data EventType = RepoChanged deriving (Eq, Show)

data RepositoryEvent = RepositoryEvent {
   _repoEventType :: EventType
 , _repoReference :: RepositoryReference
 } deriving (Eq, Show)

makeLenses ''RepositoryEvent

class (Exception e, Show e) => RepositoryEventSource s e where
  fetchEvents :: s -> IO [(ArchiveName, (Either (DataSourceError e) RepositoryEvent))]
