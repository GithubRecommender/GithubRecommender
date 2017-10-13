{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataMining.DataSource.RepositoryEvents.GithubArchive.Download where

import qualified Codec.Compression.GZip as C
import Control.Exception
import Data.Time (UTCTime(..), defaultTimeLocale, Day(..), addDays, parseTimeM, showGregorian)
import qualified Data.Set as Set
import Data.Proxy
import Data.Typeable
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API hiding (addHeader)
import Servant.Client
import qualified Network.HTTP.Media as M

data GzipedData deriving (Typeable)

type GithubArchiveAPI = Capture "archive" String :> Get '[GzipedData] B.ByteString

instance Accept GzipedData where
    contentType _ = "application" M.// "x-gzip"

instance MimeUnrender GzipedData B.ByteString where
  -- TODO add error handling
  mimeUnrender _ =  Right . C.decompress

newtype ArchiveName = ArchiveName String deriving (Eq, Show)

data DownloadError = DownloadFailed
                   | DecodingFailed deriving (Eq, Show, Typeable)

instance Exception DownloadError

data ArchiveDownload = FailedDownload ArchiveName
                     | SuccessfulDownload ArchiveName B.ByteString deriving (Eq, Show)

archiveURL :: String
archiveURL = "data.githubarchive.org"

downloadArchive :: ArchiveName -> IO ArchiveDownload
downloadArchive archiveName @ (ArchiveName name) = do
  manager  <- newManager tlsManagerSettings
  response <- runClientM request (ClientEnv manager baseUrl)

  case response of
    (Left _)       -> pure $ FailedDownload archiveName
    (Right result) -> pure $ SuccessfulDownload archiveName result
  where
    request = client (Proxy :: Proxy GithubArchiveAPI) name
    baseUrl = BaseUrl Http archiveURL 80 ""

dayFromString :: String -> Maybe Day
dayFromString str = utctDay <$> parseTimeM True defaultTimeLocale "%Y-%m-%d" str

archivesOn :: Day -> [ArchiveName]
archivesOn day = [archiveName h | h <- [0..24]]
  where
    archiveName hour = ArchiveName $ showGregorian day ++ "-" ++ show hour ++ ".json.gz"
