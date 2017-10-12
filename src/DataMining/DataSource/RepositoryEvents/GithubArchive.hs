{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataLoader.GithubArchive where

import Data.Time (UTCTime(..), defaultTimeLocale, Day(..), addDays, parseTimeM, showGregorian)
import qualified Data.Set as Set
import qualified Codec.Compression.GZip as C
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API hiding (addHeader)
import Servant.Client
import Data.Proxy
import Data.Typeable
import qualified Data.ByteString.Lazy as B
import qualified Network.HTTP.Media               as M

data GzipedData deriving (Typeable)

type GithubArchive = Capture "archive" String :> Get '[GzipedData] B.ByteString

instance Accept GzipedData where
    contentType _ = "application" M.// "x-gzip"

instance MimeUnrender GzipedData B.ByteString where
  mimeUnrender _ =  Right . C.decompress

downloadChunks :: [String] -> IO [(String, Either ServantError B.ByteString)]
downloadChunks = mapM download
  where
    download archive = fmap (archive, ) (downloadChunk archive)

downloadChunk :: String -> IO (Either ServantError B.ByteString)
downloadChunk archive = do
  manager <- newManager tlsManagerSettings
  runClientM (query archive) (ClientEnv manager baseUrl)
  where
    query   = client (Proxy :: Proxy GithubArchive)
    baseUrl = BaseUrl Http "data.githubarchive.org" 80 ""

daysFrom :: String -> Integer -> [Day]
daysFrom t num = case start of
                   Just d  -> [(utctDay d)..(end d)]
                   Nothing -> []
  where
    start :: Maybe UTCTime
    start = parseTimeM True defaultTimeLocale "%Y-%m-%d" t
    end   = addDays num . utctDay

archiveNames :: [Int] -> [Day] -> [String]
archiveNames hours days = [toName d h | h <- hours, d <- dedup days]
  where
    dedup      = Set.toList . Set.fromList
    toName d h = showGregorian d ++ "-" ++ show h ++ ".json.gz"

urls :: [Int] -> [Day] -> [String]
urls hours days = [toURL d h | h <- hours, d <- dedup days]
  where
    dedup     = Set.toList . Set.fromList
    toURL d h = "http://data.githubarchive.org/" ++ showGregorian d ++ "-" ++ show h ++ ".json.gz"
