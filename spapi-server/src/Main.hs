{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main where

import           Control.Concurrent                  (forkIO)
import           Control.Concurrent.STM              (newTVarIO)
import           Control.Distributed.Process.Lifted  (spawnLocal,expect)
import           Control.Lens                        ((^.))
import           Data.Aeson                          (eitherDecodeStrict
                                                     ,FromJSON(..),ToJSON(..)
                                                     ,genericParseJSON,genericToJSON
                                                     ,defaultOptions,fieldLabelModifier)
import qualified Data.ByteString.Char8         as B
import           Data.Char                           (toLower)
import           Data.Foldable                       (for_)
import           Data.Semigroup                      ((<>))
import           GHC.Generics                        (Generic)
import           Network.Wai.Handler.Warp            (run)
import           Network.Wai.Middleware.ETag
import           Options.Applicative                 (Parser,ParserInfo
                                                     ,info,fullDesc,progDesc
                                                     ,help,short,long,execParser,strOption
                                                     )
import           Servant.API                         ((:<|>)((:<|>)))
import           Servant.Utils.StaticFiles           (serveDirectoryFileServer)
import           Servant.Server                      (serve)
-- language-engine layer
import           FrameNet.Query.Frame                (loadFrameData)
import           Lexicon.Data                        (LexDataConfig
                                                     ,cfg_framenet_framedir
                                                     ,cfg_rolemap_file)
import           Lexicon.Query                       (loadRoleInsts)
-- compute-pipeline layer
import           CloudHaskell.Client                 (client
                                                     ,clientUnit
                                                     ,routerHandshake
                                                     ,serviceHandshake
                                                     ,heartBeatHandshake)
import           CloudHaskell.QueryQueue             (emptyQQ)
import           CloudHaskell.Type                   (Q(..),R(..),TCPPort(..),Gateway(..))
import           CloudHaskell.Util                   (lookupRouter,tellLog)
import           SemanticParserAPI.Compute.Task      (rtable)
import           SemanticParserAPI.Compute.Type      (ComputeQuery(..),ComputeResult(..)
                                                     ,ComputeConfig(..), NetworkConfig(..))
-- spapi layer
import           Worker (batchTest,postAnalysis,api)


data SPAPIConfig = SPAPIConfig {
                     spapiStaticDir :: FilePath
                   , spapiPort      :: Int
                   }
                 deriving (Show,Eq,Ord,Generic)

instance FromJSON SPAPIConfig where
  parseJSON = genericParseJSON
                defaultOptions {
                  fieldLabelModifier = \name ->
                    case drop 5 name of
                      x : xs -> toLower x : xs
                      xs -> xs
                }


instance ToJSON SPAPIConfig where
  toJSON = genericToJSON
             defaultOptions {
               fieldLabelModifier = \name ->
                 case drop 5 name of
                   x : xs -> toLower x : xs
                   xs -> xs
             }

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


main :: IO ()
main = do
  cfg <- execParser serverConfig

  qqvar1 <- newTVarIO emptyQQ
  qqvar2 <- newTVarIO emptyQQ

  ecompcfg :: Either String ComputeConfig <- eitherDecodeStrict <$> B.readFile (computeConfig cfg)
  elangcfg :: Either String LexDataConfig <- eitherDecodeStrict <$> B.readFile (langConfig cfg)
  espapicfg :: Either String SPAPIConfig <- eitherDecodeStrict <$> B.readFile (spapiConfig cfg)

  for_ ((,,) <$> ecompcfg <*> elangcfg <*> espapicfg) $ \(compcfg,langcfg,spapicfg) -> do
    let cport  = port  (computeWeb compcfg)
        chostg = hostg (computeWeb compcfg)
        chostl = hostl (computeWeb compcfg)
        shostg = hostg (computeServer compcfg)
        sport  = TCPPort (port  (computeServer compcfg))
        framedir = langcfg ^. cfg_framenet_framedir
        rolemapfile = langcfg ^. cfg_rolemap_file
    framedb <- loadFrameData framedir
    rolemap <- loadRoleInsts rolemapfile
    forkIO $
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
                tellLog $ "spid0 = " ++ show spid1
                spawnLocal $ serviceHandshake spid0 (clientUnit @ComputeQuery @ComputeResult qqvar1)
                spawnLocal $ serviceHandshake spid1 (clientUnit @Q @R qqvar2)
                () <- expect -- indefinite wait. TODO: make this more idiomatic
                pure ()
        )
    etagcontext <- defaultETagContext False
    run (spapiPort spapicfg) $
      etag etagcontext NoMaxAge  $
        serve api (serveDirectoryFileServer (spapiStaticDir spapicfg)  :<|>
                   postAnalysis framedb rolemap qqvar1       :<|>
                   batchTest qqvar2
                  )
