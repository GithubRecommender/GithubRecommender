{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module DataLoader.Services.RepositoryData.Default where

import DataLoader.GithubAPI.TokenAuthentication
import DataLoader.GithubAPI.Client
import DataLoader.Services.Types
import DataLoader.Services.RepositoryData (RepositoryData(RepositoryData),
                                           RepositoryLanguage(RepositoryLanguage),
                                           RepositoryTopic(RepositoryTopic))
import Data.Aeson
import Data.Text (Text)
import Data.Maybe (maybeToList)
import GHC.Generics
import Text.Heredoc

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

data ViewRepositoryData = ViewRepositoryData {
      repoId     :: String
    , repoName   :: Text
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

fetchData :: BearerToken -> Text -> Text -> IO (Either ServiceError RepositoryData)
fetchData token repoName' repoOwner = mapError <$> fetch
  where
    query     = GraphQLRequest dataQuery (Just variables) Nothing
    variables = object [ "name" .= repoName', "owner" .= repoOwner ]
    fetch :: ClientResponse ViewRepositoryNode
    fetch     = runRequest token query
    mapError (Left _)     = Left ServiceError
    mapError (Right view) = Right (viewData view)

viewData :: ViewRepositoryNode -> RepositoryData
viewData view = RepositoryData
                (Id . repoId $ repoData)
                (repoName repoData)
                (isFork repoData)
                (isPrivate repoData)
                (isMirror repoData)
                (isArchived repoData)
                (isLocked repoData)
                languages'
                topics'
  where
    repoData   = repository view
    languages' =  map languageData (nodes . languages $ repoData)
    languageData (ViewRepositoryLanguage i n) = RepositoryLanguage (Id i) n
    topics'' :: [ViewRepositoryTopicNode]
    topics''   = nodes . topics $ repoData
    topics'    = map (topicData . topic) topics''
    topicData (ViewRepositoryTopic i n r) = RepositoryTopic (Id i) n (related' r)
    related' (Just rel) = map topicData rel
    related' Nothing    = []
