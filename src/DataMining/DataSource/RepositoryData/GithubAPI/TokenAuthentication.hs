{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module DataMining.DataSource.RepositoryData.GithubAPI.TokenAuthentication
  (
    BearerToken(..),
    BearerTokenProtected,
    authenticateWithBearerToken
  )

where

import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Client
import Servant.Common.Req (Req, addHeader)

newtype BearerToken = BearerToken { unToken :: String } deriving (Eq, Show)

headerValue :: BearerToken -> String
headerValue = ("bearer "  ++) . unToken

headerKey :: String
headerKey = "Authorization"

type BearerTokenProtected = AuthProtect "bearer-token-auth"

type instance AuthClientData BearerTokenProtected = BearerToken

authenticateWithBearerToken :: BearerToken -> Req -> Req
authenticateWithBearerToken token = addHeader headerKey (headerValue token)
