{-# LANGUAGE OverloadedStrings #-}

module TestApplication (main) where

import System.Environment
import Data.Maybe (isJust)
import Servant.Client

import Internal.Types
import qualified DataMining.DataSource.RepositoryData.DefaultDataSource as RepoData

main :: IO ()
main = do
  -- let days = daysFrom "2017-10-10" 0
  -- results <- downloadChunks (archiveNames [15] days)
  -- mapM_ printResult results
  -- pure ()
  -- where
  --   printResult (uri, Left e)  = putStrLn $ uri ++ " ... failed .." ++ showError e
  --   printResult (uri, Right _) = putStrLn $ uri ++ " ... success"
  --   showError FailureResponse{} = " Failure"
  --   showError DecodeFailure{} = " DecoreFailure"
  --   showError (UnsupportedContentType mt _) = " UnsupportedContentType: "  ++ show mt
  --   showError InvalidContentTypeHeader{} = " InvalidContentTypeHeader"
  --   showError ConnectionError{} = " ConnectionError"
  repoData >>= print


repoData :: IO RepoData.GithubDataResult
repoData = do
  service <- RepoData.GithubRepositoryData <$> getEnv "GITHUB_TOKEN"
  RepoData.fetchSingle service (RepositoryReference "lampepfl" "dotty")
