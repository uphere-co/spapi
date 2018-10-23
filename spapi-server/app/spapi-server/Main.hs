{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators #-}
-- {-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import           Control.Concurrent                  (forkIO)
import           Control.Concurrent.STM              (newTVarIO)
import           Control.Distributed.Process.Lifted  (spawnLocal,expect)
import           Control.Lens                        ((^.))
import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Trans.Except          (ExceptT(..),runExceptT,withExceptT)
import           Data.Aeson                          (eitherDecodeStrict)
import qualified Data.ByteString.Char8 as B
import           Data.Proxy                          (Proxy(..))
import           Data.Semigroup                      ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Network.Wai.Handler.Warp            (run)
import           Network.Wai.Middleware.ETag         (MaxAge(..),etag,defaultETagContext)
import           Options.Applicative                 (Parser,ParserInfo
                                                     ,info,fullDesc,progDesc
                                                     ,help,short,long,execParser,strOption
                                                     )
import           Servant.API                         ((:<|>)((:<|>)),(:>))
import           Servant.Utils.StaticFiles           (serveDirectoryFileServer)
import           Servant.Server                      (serve)
import           Servant.API.WebSocket               (WebSocket)
-- language-engine layer
import           FrameNet.Query.Frame                (loadFrameData)
import           Lexicon.Query                       (loadRoleInsts)
import           SRL.Analyze.Config (
                   SRLConfig
                 , srlcfg_framenet_framedir
                 , srlcfg_rolemap_file
                 )
-- compute-pipeline layer
import           CloudHaskell.Client                 (client
                                                     ,clientUnit
                                                     ,routerHandshake
                                                     ,serviceHandshake
                                                     ,heartBeatHandshake)
import           CloudHaskell.QueryQueue             (emptyQQ)
import           CloudHaskell.Type                   (TCPPort(..),Gateway(..))
import           CloudHaskell.Util                   (lookupRouter,tellLog)
import           SemanticParserAPI.Compute.Task      (rtable)
import           SemanticParserAPI.Compute.Type      (ComputeConfig(..), NetworkConfig(..))
import           SemanticParserAPI.Compute.Type.Status (StatusQuery(..),StatusResult(..))
import           Task.CoreNLP                        (QCoreNLP,RCoreNLP)
import           Task.SemanticParser                 (ComputeQuery(..),ComputeResult(..))
-- spapi layer
import           API
import           SemanticParserAPI.Server.Handler
                 ( getStatus
                 , postAnalysis
                 , wsStream
                 , postCoreNLP
                 )
import           SemanticParserAPI.Server.Type
                 ( ShowError(..)
                 , SPAPIConfig(..)
                 , SPAPIServerError(..)
                 )


data ServerConfig = ServerConfig {
                      computeConfig :: FilePath
                    , langConfig    :: FilePath
                    , spapiConfig   :: FilePath
                    }
                  deriving (Show)


pOptions :: Parser ServerConfig
pOptions = ServerConfig <$> strOption (long "compute" <> short 'c' <> help "Compute pipeline configuration")
                        <*> strOption (long "lang"    <> short 'l' <> help "Language engine configuration")
                        <*> strOption (long "spapi"   <> short 's' <> help "SPAPI web server configuration")


serverConfig :: ParserInfo ServerConfig
serverConfig = info pOptions (fullDesc <> progDesc "spapi-server")


api :: Proxy ("stream" :> WebSocket :<|> API)
api = Proxy


handleError :: (ShowError e) => ExceptT e IO () -> IO ()
handleError action = do
  r <- runExceptT action
  case r of
    Left e -> TIO.putStrLn (showError e)
    Right _ -> pure ()


main :: IO ()
main = do
  handleError $ do
    cfg <- liftIO $ execParser serverConfig

    qqvar1 <- liftIO $ newTVarIO emptyQQ
    qqvar2 <- liftIO $ newTVarIO emptyQQ
    qqvar3 <- liftIO $ newTVarIO emptyQQ

    compcfg  <- withExceptT (\x -> SPAPIServerConfigError ("ComputeConfig: " <> T.pack x)) $
                  ExceptT $
                    eitherDecodeStrict @ComputeConfig <$> B.readFile (computeConfig cfg)
    langcfg  <- withExceptT (\x -> SPAPIServerConfigError ("SRLConfig: " <> T.pack x)) $
                  ExceptT $
                    eitherDecodeStrict @SRLConfig <$> B.readFile (langConfig cfg)
    spapicfg <- withExceptT (\x -> SPAPIServerConfigError ("SPAPIConfig: " <> T.pack x)) $
                  ExceptT $
                    eitherDecodeStrict <$> B.readFile (spapiConfig cfg)
    liftIO $ print (compcfg,langcfg,spapicfg)
    let cport  = port  (computeWeb compcfg)
        chostg = hostg (computeWeb compcfg)
        chostl = hostl (computeWeb compcfg)
        shostg = hostg (computeServer compcfg)
        sport  = TCPPort (port  (computeServer compcfg))
        framedir = langcfg ^. srlcfg_framenet_framedir
        rolemapfile = langcfg ^. srlcfg_rolemap_file
    framedb <- liftIO $ loadFrameData framedir
    rolemap <- liftIO $ loadRoleInsts rolemapfile
    _ <- liftIO $ forkIO $
      client
        rtable
        (cport,chostg,chostl,shostg,sport)
        (\gw -> do
            let them_ping = gatewayWeb gw
            heartBeatHandshake them_ping $
              routerHandshake $ \router -> do
                spid0 <- lookupRouter "query" router
                tellLog $ "spid0 = " ++ show spid0
                spid1 <- lookupRouter "test" router
                tellLog $ "spid1 = " ++ show spid1
                spid2 <- lookupRouter "corenlp" router
                tellLog $ "spid2 = " ++ show spid2

                _ <- spawnLocal $ serviceHandshake spid0 (clientUnit @ComputeQuery @ComputeResult qqvar1)
                _ <- spawnLocal $ serviceHandshake spid1 (clientUnit @StatusQuery @StatusResult qqvar2)
                _ <- spawnLocal $ serviceHandshake spid2 (clientUnit @QCoreNLP @RCoreNLP qqvar3)
                () <- expect -- indefinite wait. TODO: make this more idiomatic
                pure ()
        )
    etagcontext <- liftIO $ defaultETagContext False
    liftIO $ run (spapiPort spapicfg) $
      etag etagcontext NoMaxAge  $
        serve api (     wsStream qqvar2
                   :<|> serveDirectoryFileServer (spapiStaticDir spapicfg)
                   :<|> postAnalysis framedb rolemap qqvar1
                   :<|> getStatus qqvar2
                   :<|> postCoreNLP qqvar3
                  )
