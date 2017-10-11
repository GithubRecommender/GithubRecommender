{-#LANGUAGE ScopedTypeVariables #-}
module DataLoader.Worker (main) where
import Control.Concurrent (threadDelay)
import Transient.Base(parallel,keep,threads,StreamData(..),killChilds)
import Transient.Backtrack(onFinish)
import Control.Exception
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMChan (readTBMChan, writeTBMChan, newTBMChan, closeTBMChan)
import System.Random (randomRIO)
import Control.Applicative
import Control.Monad.IO.Class
import System.IO

workerCount   = 10
workloadCount = 100 :: Int
minDelay      = 250000 -- in microseconds, == 0.25 seconds
maxDelay      = 750000 --                  == 0.75 seconds

worker requestChan workerId = do
      liftIO $ hSetBuffering stdout LineBuffering

      r <- threads 1 $ parallel $ do
            delay <- randomRIO (minDelay, maxDelay)
            threadDelay delay
            mint <- atomically $ readTBMChan requestChan
            case mint of
                Nothing -> return SDone
                Just int -> return $ SMore(workerId, int, int * int)
      case r of
         SDone -> empty  -- no response, stop further actions
         SMore r -> return r

main =  keep $ do
    requestChan <- liftIO . atomically $ newTBMChan (workerCount * 2)
    let
        runWorkers = do
            foldr  (<|>) empty $ map (worker requestChan) [1..workerCount]

        fillRequests = do
            liftIO $ mapM_ (atomically . writeTBMChan requestChan) [1..workloadCount]
            liftIO $ atomically $ closeTBMChan requestChan
            empty

        printResult  (workerId, int, square) = liftIO $ do
                    putStrLn $ concat
                        [ "Worker #"
                        , show workerId
                        , ": square of "
                        , show int
                        , " is "
                        , show square
                        ]

--    onFinish (\(Just (e :: SomeException)) -> killChilds)
    r <- runWorkers <|> fillRequests
    printResult r
    pure $ Just ()
