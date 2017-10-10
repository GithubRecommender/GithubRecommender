{-# LANGUAGE OverloadedStrings #-}

module TestApplication (main) where

import DataLoader.GithubAPI.TokenAuthentication
import DataLoader.GithubAPI.Client
import System.Environment

main :: IO ()
main = do
  token <- BearerToken <$> getEnv "GITHUB_TOKEN"
  runRequest token request >>= print
  where
    request = GraphQLRequest "{ viewer { id } }" Nothing Nothing
