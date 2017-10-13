{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DataMining.DataSource.RepositoryEvents.DefaultDataSource
  (
    GithubArchiveEventSource(..)
  , GithubEventBatch
  , eventStream
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens hiding ((.=))
import Data.Aeson (decode, Value)
import Data.Aeson.Lens
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Time (Day)
import System.Directory
import Data.List.Split (splitOn)


import Internal.Types
import DataMining.DataSource.Types
import DataMining.DataSource.RepositoryEvents.Types
import DataMining.DataSource.RepositoryEvents.GithubArchive.Download

data GithubArchiveEventSource = GithubArchiveEventSource {
  _days :: [Day]
  } deriving (Eq, Show)

type GithubEventBatch = IO [RepositoryEvent]

eventStream :: GithubArchiveEventSource -> [GithubEventBatch]
eventStream (GithubArchiveEventSource [])   = [pure []]
eventStream (GithubArchiveEventSource days) = foldMap getData days
  where
    getData day = extractEvents <$> (downloadArchivesForDay day)

extractEvents :: IO ArchiveDownload -> IO [RepositoryEvent]
extractEvents action = do
  download <- action
  case download of
    (SuccessfulDownload _ content) -> pure $ extractEvents' content
    _                              -> pure []

extractEvents' :: BS.ByteString -> [RepositoryEvent]
extractEvents' input = eventsData
   where
     eventsData      = catMaybes (map decodeLine (C.lines input))
     decodeLine line = ((decode line) :: Maybe Value) >>= dataFromValue
     dataFromValue v = RepositoryEvent <$> Just RepoChanged <*> (repoReferenceFromValue v)

repoReferenceFromValue :: Value -> Maybe RepositoryReference
repoReferenceFromValue v = repoEntry >>= referenceFromString
  where
    repoEntry             = v ^? key "repo" . key "name" . _String
    referenceFromString s = case T.split (=='/') s of
                              owner:repo:[] -> Just $ RepositoryReference owner repo
                              _             -> Nothing

defaultCacheDirectory = "/tmp"

downloadArchivesForDay :: Day -> [IO ArchiveDownload]
downloadArchivesForDay day = map (downloadArchive' defaultCacheDirectory) (archivesOn day)

downloadArchive' :: FilePath -> ArchiveName -> IO ArchiveDownload
downloadArchive' cacheDirectory archiveName = do
  cachedContent <- readCached cacheFile

  case cachedContent of
    Just content -> pure $ SuccessfulDownload archiveName content
    Nothing      -> do

      download <- downloadArchive archiveName

      case download of
        (SuccessfulDownload name content) -> do
          writeCached cacheFile content
          pure download
        _failed ->
          pure _failed
  where
    cacheFile = cacheFilePath cacheDirectory archiveName

writeCached :: FilePath -> BS.ByteString -> IO ()
writeCached = BS.writeFile

readCached :: FilePath -> IO (Maybe BS.ByteString)
readCached cacheFile = do
  cacheFileExists <- doesFileExist cacheFile
  if cacheFileExists then
    (Just <$> (BS.readFile cacheFile))
  else
    pure Nothing

cacheFilePath :: FilePath -> ArchiveName -> FilePath
cacheFilePath cacheDirectory (ArchiveName name) = cacheDirectory ++ "/" ++ name ++ ".cache"
