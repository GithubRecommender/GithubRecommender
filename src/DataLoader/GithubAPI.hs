{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module DataLoader.GithubAPI where

import Data.Aeson
import Data.Proxy
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import Control.Monad.Except (runExceptT)
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.API hiding (addHeader)
import Servant.Client
import Servant.Common.Req (Req, addHeader)
import DataLoader.GithubAPI.TokenAuthentication

githubURL :: String
githubURL = "https://api.github.com"

testToken :: BearerToken
testToken = BearerToken "db77c2fbdfc5ce4a8fb8a8f2499854fc04cafa23"

-- | API client
type GithubAPI       = "graphql" :> (Header "User-Agent" String) :> BearerTokenProtected :> ReqBody '[JSON] GraphQLRequest :> Post '[JSON] GraphQLResponse
data GraphQLRequest  = GraphQLRequest  { _query :: Text, _variables :: Maybe Object } deriving (Eq, Show, Generic)
data GraphQLResponse = GraphQLResponse { _data :: Maybe Object, _errors :: (Maybe [Object])  } deriving (Eq, Show, Generic)

instance ToJSON GraphQLRequest where
  toJSON request = object [ "query" .= _query request, "variables" .= _variables request]

instance FromJSON GraphQLResponse where
  parseJSON (Object response) = GraphQLResponse <$> response .:? "data" <*> response .:? "errors"

postQuery = client api
  where
    api :: Proxy GithubAPI
    api = Proxy

runRequest :: IO ()
runRequest = do
  manager <- newManager tlsManagerSettings
  result  <- runClientM ((postQuery (Just "haskell/1.0") withAuthentication) req) (ClientEnv manager (BaseUrl Https "api.github.com" 443 ""))
  print result
  pure ()
  where
    req = GraphQLRequest "{ viewer { id } }" Nothing
    withAuthentication = (mkAuthenticateReq testToken authenticateWithBearerToken)
