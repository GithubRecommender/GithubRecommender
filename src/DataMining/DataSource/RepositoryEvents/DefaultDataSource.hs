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
import Data.Aeson (decode, Value)
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Time (Day)
import System.Directory


import Internal.Types
import DataMining.DataSource.Types
import DataMining.DataSource.RepositoryEvents.Types
import DataMining.DataSource.RepositoryEvents.GithubArchive.Download

data GithubArchiveEventSource = GithubArchiveEventSource {
  _days :: [Day]
  } deriving (Eq, Show)

instance RepositoryEventSource GithubArchiveEventSource DownloadError where
  fetchEvents = fetchEvents'

type GithubEventBatch = IO [RepositoryEvent]

eventStream :: GithubArchiveEventSource -> [GithubEventBatch]
eventStream (GithubArchiveEventSource [])   = [pure []]
eventStream (GithubArchiveEventSource days) = extractEvents <$> (downloadArchivesForDay day)
  where
    day = head days

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
     decodeLine line = dataFromValue <$> ((decode line) :: Maybe Value)
     dataFromValue v = RepositoryEvent RepoChanged ref
     ref             = RepositoryReference "foo" "bar"



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

fetchEvents' = undefined

-- fetchEvents' :: GithubArchiveEventSource -> IO GithubEventResult
-- fetchEvents' (GithubArchiveEventSource [])   = pure []
-- fetchEvents' (GithubArchiveEventSource days) = do
--   archives <- downloadArchives day
--   pure (archives >>= archiveEvents)
--   where
--     day = head days

-- extractEvents :: BS.ByteString -> [Either (DataSourceError DownloadError) RepositoryEvent]
-- extractEvents input = map extractData eventsData
--   where
--     eventsData :: [Either String Value]
--     eventsData                = map eitherDecode (C.lines input)
--     extractData (Left e)      = Left (GenericError e)
--     extractData (Right event) = Right $ dataFromValue event
--     dataFromValue v           = RepositoryEvent RepoChanged ref
--     ref                       = RepositoryReference "foo" "bar"


-- archiveEvents :: ArchiveDownload -> (ArchiveName, Either (DataSourceError DownloadError) [RepositoryEvent])
-- archiveEvents (SuccessfulDownload archiveName content) = (archiveName, ) <$> (extractEvents content)
-- archiveEvents (FailedDownload archiveName)             = (archiveName, Left (BackendError DownloadFailed))

-- downloadArchives :: ArchiveName -> IO [ArchiveDownload]
-- downloadArchives day = traverse downloadArchive (archivesOn day)
