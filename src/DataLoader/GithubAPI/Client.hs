{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module DataLoader.GithubAPI.Client
  (
     GraphQLRequest(..)
   , GraphQLResponse(..)
   , ClientResponse
   , runRequest
  )
where

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics

import Control.Exception

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API hiding (addHeader)
import Servant.Client

import DataLoader.GithubAPI.TokenAuthentication

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.github.com" 443 ""

defaultUserAgent :: String
defaultUserAgent = "servantclient/0.5"

type WithUserAgent = (Header "User-Agent" String)

data GraphQLRequest = GraphQLRequest  {
    _query     :: Text
  , _variables :: Maybe Value
  , _operation :: Maybe Text
  } deriving (Eq, Show, Generic)


instance ToJSON GraphQLRequest where
  toJSON request = object [
      "query"         .= _query request
    , "variables"     .= _variables request
    , "operationName" .= _operation request
    ]

data GraphQLResponse result = GraphQLResponse {
  _data   :: Maybe result,
  _errors :: Maybe Value
  } deriving (Eq, Show, Generic)

instance (FromJSON result) => FromJSON (GraphQLResponse result) where
  parseJSON (Object response) = GraphQLResponse
                                <$> response .:? "data"
                                <*> response .:? "errors"

type GithubAPIV4 a = "graphql" :> WithUserAgent :> BearerTokenProtected :> ReqBody '[JSON] GraphQLRequest :> Post '[JSON] (GraphQLResponse a)

data ClientError a    = FatalError ServantError -- ^ No response at all
                      | ServerError Value       -- ^ Errors
                      | MalformedResponse
                      | PartialError Value a    -- ^ Parts of the response are not available due to resolution errors
                      deriving (Eq, Show)


type ClientResponse a = IO (Either (ClientError a) a)

runRequest :: (FromJSON result) => BearerToken -> GraphQLRequest -> ClientResponse result
runRequest token graphqlRequest = do
  manager <- newManager tlsManagerSettings
  extractResponse <$> runClientM request (ClientEnv manager baseUrl)
  where
    request            = query (Just defaultUserAgent) authenticate graphqlRequest
    authenticate       = mkAuthenticateReq token authenticateWithBearerToken
    query              = client (Proxy :: Proxy (GithubAPIV4 result))

extractResponse :: (FromJSON result) => Either ServantError (GraphQLResponse result) -> Either (ClientError result) result
extractResponse (Right (GraphQLResponse (Just data') Nothing))  = Right data'
extractResponse (Right (GraphQLResponse (Just data') (Just e))) = Left (PartialError e data')
extractResponse (Right (GraphQLResponse Nothing (Just e)))      = Left (ServerError e)
extractResponse (Right (GraphQLResponse Nothing Nothing))       = Left MalformedResponse
extractResponse (Left e)                                        = Left (FatalError e)
