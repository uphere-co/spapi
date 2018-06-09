{-# LANGUAGE OverloadedStrings #-}

module Main where

import API

import Data.Proxy (Proxy(..))
import System.Environment
import Servant.API ((:<|>)((:<|>)))
import Servant.Utils.StaticFiles (serveDirectoryFileServer)
import Servant.Server (Handler,serve)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.ETag


import           Control.Concurrent                  (forkIO,threadDelay)
import           Control.Concurrent.STM              (atomically,retry,newTVarIO
                                                     ,modifyTVar',readTVar,writeTVar)
import           Control.Distributed.Process.Lifted  (SendPort,spawnLocal)
import           Control.Monad                       (forever)
import           Control.Monad.IO.Class              (liftIO)
import qualified Data.IntMap                   as IM

import           CloudHaskell.QueryQueue             (QueryStatus(..),QQVar,emptyQQ,next
                                                     ,singleQuery
                                                     )
import           SemanticParserAPI.Compute.Type      (ComputeQuery(..),ComputeResult(..)
                                                     ,ResultSentence(..)
                                                     )
import           SemanticParserAPI.CLI.Client        (consoleClient)
import           CloudHaskell.Util                   (LogProcess
                                                     ,onesecond,tellLog,queryProcess
                                                     ,client,mainP,initP)


api :: Proxy API
api = Proxy


webClient :: QQVar ComputeQuery ComputeResult
          -> SendPort (ComputeQuery, SendPort ComputeResult)
          -> LogProcess ()
webClient qqvar sc = do
  forever $ do
    (i,q) <- liftIO $ atomically $ do
               qq <- readTVar qqvar
               case next qq of
                 Nothing -> retry
                 Just (i,q) -> do
                   let qq' = IM.update (\_ -> Just (BeingProcessed q)) i qq
                   writeTVar qqvar qq'
                   return (i,q)
    tellLog ("query start: " ++ show (i,q))
    spawnLocal $ do
      r <- queryProcess sc q return
      liftIO $ atomically $ modifyTVar' qqvar (IM.update (\_ -> Just (Answered q r)) i)
      test <- liftIO $ atomically $ readTVar qqvar
      tellLog (show test)


exampleItem :: Item
exampleItem = Item 0 "example item"

getItem :: QQVar ComputeQuery ComputeResult -> Handler Item
getItem qqvar = do
  let sent = "I sent a letter to him."
  CR_Sentence (ResultSentence _ tokss mgs) <- liftIO (singleQuery qqvar (CQ_Sentence sent))
  -- liftIO $ print mgs
  pure (Item 0 (show mgs))

main = do
  (d:_) <- getArgs
  -- putStrLn "Serving on localhost:8080/static/, visit http://localhost:8080/static/index.html"

  qqvar <- newTVarIO emptyQQ

  let port = 12933
      hostg = "127.0.0.1"
      hostl = "127.0.0.1"
      serverip = "127.0.0.1"
      serverport = 12930

  forkIO $ client (port,hostg,hostl,serverip,serverport) (initP (mainP (webClient qqvar)))

  etagcontext <- defaultETagContext False
  run 8080 $
    etag etagcontext NoMaxAge  $
      serve api (serveDirectoryFileServer d :<|>
                 getItem qqvar
                )



