{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module DataLoader.Services.RepositoryData.Default where

import DataLoader.GithubAPI.TokenAuthentication
import DataLoader.GithubAPI.Client
import DataLoader.Services.Types
import DataLoader.Services.RepositoryData
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Text.Heredoc

-- data RepoDataService = RepoDataService { token :: BearerToken }

-- instance RepositoryDataService RepoDataService where
--   fetchSingle   = fetchSingleFromApi
--   fetchMultiple = fetchMultipleFromApi

dataQuery :: Text
dataQuery = [str|
                |fragment topicInfo on Topic {
                |  topicId: id
                |  topicName: name
                |}
                |
                |query RepositoryInfo($name: String!, $owner: String!) {
                |  repository(name: $name, owner: $owner) {
                |    id
                |    name
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

data ViewRepositoryData = ViewRepositoryData {
      id         :: String
    , name       :: Text
    , isFork     :: Bool
    , isPrivate  :: Bool
    , isMirror   :: Bool
    , isArchived :: Bool
    , isLocked   :: Bool
    , languages  :: ViewNodes ViewRepositoryLanguage
    , topics     :: ViewNodes ViewRepositoryTopicNode
  } deriving (Eq, Show, Generic)

data ViewRepositoryNode = ViewRepositoryNode {
   repository :: ViewRepositoryData
 } deriving (Eq, Show, Generic)

data ViewNodes a = ViewNodes {
  nodes :: [a]
 } deriving (Eq, Show, Generic)

data ViewRepositoryTopicNode = ViewRepositoryTopicNode {
  topic :: ViewRepositoryTopic
 } deriving (Eq, Show, Generic)

data ViewRepositoryTopic = ViewRepositoryTopic {
     topicId       :: String
   , topicName     :: Text
   , relatedTopics :: Maybe [ViewRepositoryTopic]
 } deriving (Eq, Show, Generic)


data ViewRepositoryLanguage = ViewRepositoryLanguage {
    languageId :: String
  , languageName :: Text
 } deriving (Eq, Show, Generic)

instance FromJSON ViewRepositoryData
instance (FromJSON a) => FromJSON (ViewNodes a)
instance FromJSON ViewRepositoryTopicNode
instance FromJSON ViewRepositoryTopic
instance FromJSON ViewRepositoryLanguage
instance FromJSON ViewRepositoryNode

fetchData :: BearerToken -> Text -> Text -> ClientResponse ViewRepositoryNode
fetchData token repoName repoOwner = do
  runRequest token query
  -- viewData <$> response
  where
    query     = GraphQLRequest dataQuery (Just variables) Nothing
    variables = object [ "name" .= repoName, "owner" .= repoOwner ]
