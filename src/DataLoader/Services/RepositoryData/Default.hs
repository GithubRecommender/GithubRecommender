{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}

module DataLoader.Services.RepositoryData.Default where
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.Vector (Vector, toList)
import Data.Text (Text, unpack)
import Data.Maybe (maybeToList, fromMaybe)
import GHC.Generics
import Text.Heredoc

import DataLoader.GithubAPI.TokenAuthentication
import DataLoader.GithubAPI.Client
import DataLoader.Services.Types
import DataLoader.Services.RepositoryData (RepositoryData(RepositoryData),
                                           RepositoryLanguage(RepositoryLanguage),
                                           RepositoryTopic(RepositoryTopic))
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

fetchData token repoName repoOwner = extractData <$> runQuery
  where
    runQuery :: ClientResponse Value
    runQuery = runRequest token (mkRequest repoName repoOwner)


mkRequest :: Text -> Text -> GraphQLRequest
mkRequest repoName repoOwner = GraphQLRequest dataQuery (Just variables) Nothing
  where
    variables = object ["name" .= repoName, "owner" .= repoOwner]

mkId :: Text -> Id
mkId = Id . unpack

extractData :: Either (ClientError Value) Value -> Either ServiceError RepositoryData
extractData (Left e)      = Left ServiceError
extractData (Right value) = case result of
                              Just v  -> Right v
                              Nothing -> Left ServiceError
  where
    repo field tpe = value ^? key "repository" . key field . tpe
    repoId         = Id . unpack <$> (repo "repoId" _String)
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
                 (mkId <$> (v ^? key "languageId"._String)) <*>
                 v ^? key "languageName"._String

extractTopics :: Maybe [Value] -> Maybe [RepositoryTopic]
extractTopics v = (v >>= (sequence . map topic)) `orElse` (Just [])
  where
    topic v = RepositoryTopic <$>
              (mkId <$> (v ^? key "topic" . key "topicId"._String)) <*>
              (v ^? key "topic" . key "topicName"._String)          <*>
              extractTopics (toList <$> v ^? key "topic" . key "relatedTopics"._Array)

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing a = a
orElse v _       = v
