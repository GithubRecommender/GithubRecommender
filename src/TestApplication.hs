{-# LANGUAGE OverloadedStrings #-}

module TestApplication (main) where

import System.Environment
import Data.Maybe (isJust, fromJust)
import Servant.Client
import Data.Time (Day)

import Internal.Types

import DataMining.DataSource.RepositoryEvents.Types
import qualified DataMining.DataSource.RepositoryData.DefaultDataSource as RepoData
import qualified DataMining.DataSource.RepositoryEvents.DefaultDataSource as RepoEvent
import DataMining.DataSource.RepositoryEvents.GithubArchive.Download (dayFromString)

main :: IO ()
main = do
  events    <- fmap (take 1) $ batch
  eventData <- traverse (repoData . _repoReference) events
  print eventData
  pure ()
 where
   batch :: RepoEvent.GithubEventBatch
   batch   = head repoEvents

repoEvents = do
  let day = fromJust $ dayFromString "2017-10-10"
  RepoEvent.eventStream (RepoEvent.GithubArchiveEventSource [day])

repoData :: RepositoryReference -> IO RepoData.GithubDataResult
repoData reference = do
  service <- RepoData.GithubRepositoryData <$> getEnv "GITHUB_TOKEN"
  RepoData.fetchSingle service reference
