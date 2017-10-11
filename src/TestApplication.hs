{-# LANGUAGE OverloadedStrings #-}

module TestApplication (main) where

import DataLoader.GithubAPI.TokenAuthentication
import DataLoader.Services.RepositoryData.Default
import DataLoader.GithubArchive
import System.Environment
import Data.Maybe (isJust)
import Servant.Client

main :: IO ()
main = do
  let days = daysFrom "2017-10-10" 0
  results <- downloadChunks (archiveNames [15] days)
  mapM_ printResult results
  pure ()
  where
    printResult (uri, Left e)  = putStrLn $ uri ++ " ... failed .." ++ showError e
    printResult (uri, Right _) = putStrLn $ uri ++ " ... success"
    showError FailureResponse{} = " Failure"
    showError DecodeFailure{} = " DecoreFailure"
    showError (UnsupportedContentType mt _) = " UnsupportedContentType: "  ++ show mt
    showError InvalidContentTypeHeader{} = " InvalidContentTypeHeader"
    showError ConnectionError{} = " ConnectionError"

  -- token <- BearerToken <$> getEnv "GITHUB_TOKEN"
  -- fetchData token "dotty" "lampepfl" >>= print
