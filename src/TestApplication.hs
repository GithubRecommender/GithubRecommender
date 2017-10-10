{-# LANGUAGE OverloadedStrings #-}

module TestApplication (main) where

import DataLoader.GithubAPI.TokenAuthentication
import DataLoader.Services.RepositoryData.Default
import System.Environment

main :: IO ()
main = do
  token <- BearerToken <$> getEnv "GITHUB_TOKEN"
  fetchData token "dotty" "lampepfl" >>= print
  -- (runRequest token request :: ClientResponse Value) >>= print
