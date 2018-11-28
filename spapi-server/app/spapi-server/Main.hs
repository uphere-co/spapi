{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Control.Lens                ( (^.) )
import           Control.Monad.IO.Class      ( liftIO )
import           Control.Monad.Trans.Except  ( ExceptT(..), withExceptT )
import           Data.Aeson                  ( eitherDecodeStrict )
import qualified Data.ByteString.Char8 as B
import           Data.Proxy                  ( Proxy(..) )
import           Data.Semigroup              ( (<>) )
import qualified Data.Text as T
import           Network.HTTP.Client         ( defaultManagerSettings, newManager )
import           Network.Wai.Handler.Warp    ( run )
import           Network.Wai.Middleware.ETag ( MaxAge(..), etag, defaultETagContext )
import           Options.Applicative         ( Parser, ParserInfo
                                             , info, fullDesc, progDesc
                                             , help, short, long, execParser
                                             , strOption
                                             )
import           Servant.API                 ( (:<|>)((:<|>)), (:>) )
import           Servant.Client              ( ClientEnv(..), parseBaseUrl )
import           Servant.Server.StaticFiles  ( serveDirectoryFileServer )
import           Servant.Server              ( serve )
import           Servant.API.WebSocket       ( WebSocket )
------ language-engine layer
import           FrameNet.Query.Frame        ( loadFrameData )
import           Lexicon.Query               ( loadRoleInsts )
import           SRL.Analyze.Config          ( SRLConfig
                                             , srlcfg_framenet_framedir
                                             , srlcfg_rolemap_file
                                             )
------ compute-pipeline layer
import           CloudHaskell.Util           ( handleError )
------ spapi layer
import           API
import           SemanticParserAPI.Server.Handler
                                             ( getStatus
                                             , postAnalysis
                                             , wsStream
                                             )
import           SemanticParserAPI.Server.Type
                                             ( SPAPIConfig(..)
                                             , SPAPIServerError(..)
                                             )


data ServerConfig = ServerConfig {
                      computeURL :: String
                    , langConfig    :: FilePath
                    , spapiConfig   :: FilePath
                    }
                  deriving (Show)


pOptions :: Parser ServerConfig
pOptions = ServerConfig <$> strOption (long "compute" <> short 'c' <> help "Compute pipeline base URL")
                        <*> strOption (long "lang"    <> short 'l' <> help "Language engine configuration")
                        <*> strOption (long "spapi"   <> short 's' <> help "SPAPI web server configuration")


serverConfig :: ParserInfo ServerConfig
serverConfig = info pOptions (fullDesc <> progDesc "spapi-server")


api :: Proxy ("stream" :> WebSocket :<|> API)
api = Proxy


main :: IO ()
main = do
  handleError $ do
    cfg <- liftIO $ execParser serverConfig
    langcfg  <- withExceptT (\x -> SPAPIServerConfigError ("SRLConfig: " <> T.pack x)) $
                  ExceptT $
                    eitherDecodeStrict @SRLConfig <$> B.readFile (langConfig cfg)
    spapicfg <- withExceptT (\x -> SPAPIServerConfigError ("SPAPIConfig: " <> T.pack x)) $
                  ExceptT $
                    eitherDecodeStrict <$> B.readFile (spapiConfig cfg)
    -- prepare for FrameNet
    let framedir = langcfg ^. srlcfg_framenet_framedir
        rolemapfile = langcfg ^. srlcfg_rolemap_file
    framedb <- liftIO $ loadFrameData framedir
    rolemap <- liftIO $ loadRoleInsts rolemapfile
    -- prepare for servant client
    manager' <- liftIO $ newManager defaultManagerSettings
    baseurl <- liftIO$ parseBaseUrl (computeURL cfg)
    let env = ClientEnv manager' baseurl Nothing

    etagcontext <- liftIO $ defaultETagContext False
    liftIO $ run (spapiPort spapicfg) $
      etag etagcontext NoMaxAge  $
        serve api (     wsStream
                   :<|> serveDirectoryFileServer (spapiStaticDir spapicfg)
                   :<|> postAnalysis env framedb rolemap
                   :<|> getStatus
                  )
