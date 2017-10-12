{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module DataMining.DataSource.RepositoryData.GithubAPI.Client
  (
     GraphQLRequest(..)
   , GraphQLResponse(..)
   , ClientResponse
   , ClientError(..)
   , runRequest
  )
where

import Control.Exception
import Control.Lens (makeLenses)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Text (Text)
import Data.Typeable
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API hiding (addHeader)
import Servant.Client

import DataMining.DataSource.RepositoryData.GithubAPI.TokenAuthentication

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.github.com" 443 ""

defaultUserAgent :: String
defaultUserAgent = "servantclient/0.5"

type WithUserAgent = (Header "User-Agent" String)

data GraphQLRequest = GraphQLRequest  {
    _requestQuery     :: Text
  , _requestVariables :: Maybe Value
  , _requestOperation :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON GraphQLRequest where
  toJSON request = object [
      "query"         .= _requestQuery request
    , "variables"     .= _requestVariables request
    , "operationName" .= _requestOperation request
    ]

data GraphQLResponse result = GraphQLResponse {
  _reponseData   :: Maybe result,
  _reponseErrors :: Maybe Value
  } deriving (Eq, Show, Generic)

makeLenses ''GraphQLResponse
makeLenses ''GraphQLRequest

instance (FromJSON result) => FromJSON (GraphQLResponse result) where
  parseJSON (Object response) = GraphQLResponse
                                <$> response .:? "data"
                                <*> response .:? "errors"
  parseJSON _                 = mempty

type GithubAPIV4 a = "graphql" :> WithUserAgent :> BearerTokenProtected :> ReqBody '[JSON] GraphQLRequest :> Post '[JSON] (GraphQLResponse a)


data ClientError a = FatalError ServantError -- ^ No response at all
                   | ServerError Value       -- ^ Errors
                   | MalformedResponse
                   | PartialError Value a    -- ^ Parts of the response are not available due to resolution errors
                   deriving (Eq, Show, Typeable)

instance (Typeable a, Show a) => Exception (ClientError a)

type ClientResponse a = IO (Either (ClientError a) a)

runRequest :: (FromJSON result) => BearerToken -> GraphQLRequest -> ClientResponse result
runRequest token graphqlRequest = do
  manager <- newManager tlsManagerSettings
  extractResponse <$> runClientM request (ClientEnv manager baseUrl)
  where
    request      = query (Just defaultUserAgent) authenticate graphqlRequest
    authenticate = mkAuthenticateReq token authenticateWithBearerToken
    query        = client (Proxy :: Proxy (GithubAPIV4 result))

extractResponse :: (FromJSON result) => Either ServantError (GraphQLResponse result) -> Either (ClientError result) result
extractResponse (Right (GraphQLResponse (Just data') Nothing))  = Right data'
extractResponse (Right (GraphQLResponse (Just data') (Just e))) = Left (PartialError e data')
extractResponse (Right (GraphQLResponse Nothing (Just e)))      = Left (ServerError e)
extractResponse (Right (GraphQLResponse Nothing Nothing))       = Left MalformedResponse
extractResponse (Left e)                                        = Left (FatalError e)
