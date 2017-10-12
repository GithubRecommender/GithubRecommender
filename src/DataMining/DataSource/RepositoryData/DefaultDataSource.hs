{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataMining.DataSource.RepositoryData.DefaultDataSource
  (
      GithubRepositoryData(..)
    , GithubDataResult
    , fetchSingle
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe (maybeToList, fromMaybe)
import Data.Text (Text, unpack)
import Data.Vector (Vector, toList)
import GHC.Generics
import Text.Heredoc

import Internal.Types
import DataMining.DataSource.Types
import DataMining.DataSource.RepositoryData.Types
import DataMining.DataSource.RepositoryData.GithubAPI.TokenAuthentication
import DataMining.DataSource.RepositoryData.GithubAPI.Client

dataQuery :: Text
dataQuery = [str|
                |fragment topicInfo on Topic {
                |  topicId: id
                |  topicName: name
                |}
                |
                |query RepositoryInfo($name: String!, $owner: String!) {
                |  repository(name: $name, owner: $owner) {
                |    repoId: id
                |    repoName: name
                |    isFork
                |    isPrivate
                |    isMirror
                |    isArchived
                |    isLocked
                |    languages(first: 10, orderBy: {field: SIZE, direction: DESC}) {
                |      nodes {
                |        languageId: id
                |        languageName: name
                |      }
                |    }
                |
                |    topics: repositoryTopics(first: 50) {
                |      nodes {
                |        topic {
                |          ...topicInfo
                |
                |          relatedTopics {
                |            ...topicInfo
                |          }
                |        }
                |      }
                |    }
                |  }
                |}
                |]

type BackendResult    = Either (ClientError Value) Value
type GithubDataError  = DataSourceError (ClientError Value)
type GithubDataResult = Either GithubDataError RepositoryData

data GithubRepositoryData = GithubRepositoryData {
  _token :: String
  } deriving (Eq, Show)

instance RepositoryDataSource GithubRepositoryData (ClientError Value) where
  fetchSingle d ref = fetchDataFor authToken ref
    where
      authToken = BearerToken . _token $ d

fetchDataFor :: BearerToken -> RepositoryReference -> IO GithubDataResult
fetchDataFor token ref = extractData <$> runQuery
  where
    runQuery :: ClientResponse Value
    runQuery = runRequest token (mkRequest ref)

mkRequest :: RepositoryReference -> GraphQLRequest
mkRequest ref = GraphQLRequest dataQuery (Just variables) Nothing
  where
    variables = object ["name" .= (_repositoryName ref), "owner" .= (_repositoryOwner ref)]

extractData :: BackendResult -> GithubDataResult
extractData (Left e)      = Left (BackendError e)
extractData (Right value) = case result of
                              Just r  -> Right r
                              Nothing -> Left (GenericError "Couldn not extract RepoData from response")
  where
    repo field tpe = value ^? key "repository" . key field . tpe
    repoId         = ID . unpack <$> (repo "repoId" _String)
    result         = RepositoryData            <$>
                     repoId                    <*>
                     (repo "repoName" _String) <*>
                     (repo "isFork" _Bool)     <*>
                     (repo "isPrivate" _Bool)  <*>
                     (repo "isMirror" _Bool)   <*>
                     (repo "isArchived" _Bool) <*>
                     (repo "isLocked" _Bool)   <*>
                     languages                 <*>
                     topics
    languages = extractLanguages $ toList <$> value ^? key "repository" . key "languages" . key "nodes" . _Array
    topics    = extractTopics    $ toList <$> value ^? key "repository" . key "topics"    . key "nodes" . _Array

extractLanguages :: Maybe [Value] -> Maybe [RepositoryLanguage]
extractLanguages v = (v >>= (sequence . map language)) `orElse` (Just [])
  where
    language v = RepositoryLanguage <$>
                 (ID . unpack <$> (v ^? key "languageId"._String)) <*>
                 v ^? key "languageName"._String

extractTopics :: Maybe [Value] -> Maybe [RepositoryTopic]
extractTopics v = (v >>= (sequence . map topic)) `orElse` (Just [])
  where
    topic v = RepositoryTopic <$>
              (ID . unpack <$> (v ^? key "topic" . key "topicId"._String)) <*>
              (v ^? key "topic" . key "topicName"._String)          <*>
              extractTopics (toList <$> v ^? key "topic" . key "relatedTopics"._Array)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing a = a
orElse v _       = v
