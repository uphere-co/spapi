{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Main where

import           Control.Concurrent                  (forkIO,threadDelay)
import           Control.Concurrent.STM              (atomically,retry,newTVarIO
                                                     ,modifyTVar',readTVar,writeTVar)
import           Control.Distributed.Process.Lifted  (SendPort,ReceivePort,send,getSelfPid
                                                     ,spawnLocal)
import           Control.Lens                        ((^.),(^..))
import           Control.Monad                       (forever,void)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Aeson                          (eitherDecodeStrict)
import qualified Data.ByteString.Base64        as B64
import qualified Data.ByteString.Char8         as B
import           Data.Foldable                       (forM_)
import qualified Data.HashMap.Strict           as HM
import qualified Data.IntMap                   as IM
import           Data.Maybe                          (fromMaybe)
import           Data.Proxy                          (Proxy(..))
import           Data.Semigroup                      ((<>))
import           Data.Text                           (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.IO                  as TIO
import qualified Data.Text.Lazy                as TL
import           Network.Wai.Handler.Warp            (run)
import           Network.Wai.Middleware.ETag
import           Options.Applicative                 (Parser,ParserInfo
                                                     ,auto,info,fullDesc,progDesc,option
                                                     ,help,short,long,execParser,strOption
                                                     )
import           Servant.API                         ((:<|>)((:<|>)))
import           Servant.Utils.StaticFiles           (serveDirectoryFileServer)
import           Servant.Server                      (Handler,serve)
import           System.Directory                    (getCurrentDirectory
                                                     ,setCurrentDirectory
                                                     ,getTemporaryDirectory
                                                     ,removeFile)
import           System.FilePath                     ((</>),(<.>))
import           System.Environment                  (getArgs)
import           System.Process                      (readProcess)
-- nlp layer
import           FrameNet.Query.Frame                (FrameDB,frameDB,loadFrameData)
import qualified FrameNet.Type.Definition      as F
import           FrameNet.Type.Frame                 (frame_definition)
import           Lexicon.Data                        (LexDataConfig
                                                     ,cfg_framenet_framedir
                                                     ,cfg_rolemap_file)
import           Lexicon.Query                       (loadRoleInsts)
import           Lexicon.Type                        (RoleInstance)
import           NLP.Semantics.Type                  (MeaningRoleContent(..),MeaningTree(..)
                                                     ,mt_frame,mt_arguments,mt_subordinates
                                                     ,mr_content,po_main)
import           SRL.Analyze.ARB                     (mkARB)
import           SRL.Analyze.Format                  (dotMeaningGraph)
import           SRL.Analyze.Format.OGDF             (mkOGDFSVG)
import           SRL.Analyze.MeaningTree             (mkMeaningTree)
import           SRL.Analyze.Type                    (AnalyzePredata,ConsoleOutput
                                                     ,DocStructure,MeaningGraph
                                                     ,analyze_framedb,analyze_rolemap
                                                     ,ds_mtokenss,ds_sentStructures,ss_tagged
                                                     ,outputDocStructure,outputMatchedFrames,outputX'tree
                                                     )
-- spapi layer
import           CloudHaskell.Client                 (queryProcess,client,mainP
                                                     ,heartBeatHandshake)
import           CloudHaskell.QueryQueue             (QueryStatus(..),QQVar,emptyQQ,next
                                                     ,singleQuery)
import           CloudHaskell.Type                   (Pipeline,Q(..),R(..))
import           CloudHaskell.Util                   (onesecond,tellLog)
import           SemanticParserAPI.Compute.Type      (ComputeQuery(..),ComputeResult(..)
                                                     ,ResultSentence(..)
                                                     ,ComputeConfig(..), NetworkConfig(..)
                                                     )
import           SemanticParserAPI.CLI.Client        (webClient)
import           SemanticParserAPI.Type              (InputSentence(..),PNGData(..),APIResult(..)
                                                     ,DefRoot(..),CContent(..),EContent(..)
                                                     ,SVGData(..)
                                                     )
import qualified SemanticParserAPI.Type            as S  (ConsoleOutput(ConsoleOutput))
--
import           API

api :: Proxy API
api = Proxy


withTempFile :: (String,String) -> Int -> (FilePath -> IO a) -> IO a
withTempFile (base,ext) i action = do
  tmpdir <- getTemporaryDirectory
  let file = tmpdir </> base ++ show i <.> ext
  r <- action file
  removeFile file
  pure r


uriEncode mimetype bstr = TE.decodeUtf8 ("data:" <> mimetype <> ";base64," <> B64.encode bstr)

createDotGraph :: MeaningGraph -> IO PNGData
createDotGraph mg = do
  let dotstr = dotMeaningGraph Nothing mg
  cdir <- getCurrentDirectory
  tdir <- getTemporaryDirectory
  setCurrentDirectory tdir
  TIO.writeFile "test.dot" dotstr
  -- TODO remove explicit path
  void (readProcess "/nix/store/hxwdxsg6w79cnj2slkhk3bs8fx6nvdyk-graphviz-2.40.1/bin/dot" ["-Tpng","test.dot","-otest.png"] "")
  -- TODO use temporary name and remove
  bstr <- B.readFile "test.png"
  setCurrentDirectory cdir
  let pngdata = PNGData (uriEncode "image/png" bstr)
  pure pngdata


createOGDFSVG :: (Int,MeaningGraph) -> IO SVGData
createOGDFSVG (i,mg) =
  withTempFile ("test","svg") i $ \file -> do
    mkOGDFSVG file mg
    bstr <- B.readFile file
    let svgdata = SVGData (uriEncode "image/svg+xml" bstr)
    pure svgdata


allFrames :: MeaningTree -> [Text]
allFrames mt = let frm0 = mt^.mt_frame
               in frm0 : (ys ++ zs)
  where
    xs = mt^..mt_arguments.traverse.mr_content
    ys = concatMap (f . (^.po_main)) xs
    zs = concatMap allFrames (mt^.mt_subordinates)
    --
    f (SubFrame x) = allFrames x
    f (Modifier _ subs) = concatMap allFrames subs
    f _ = []


convertDefRoot :: F.DefRoot -> DefRoot
convertDefRoot (F.DefRoot lst) = DefRoot (map convertCContent lst)
  where
    convertCContent (F.CTEXT txt) = CTEXT txt
    convertCContent (F.CFEN txt)  = CFEN txt
    convertCContent (F.CEX xs)    = CEX (map convertEContent xs)
    convertCContent F.CRET        = CRET ()
    --
    convertEContent (F.ETEXT txt) = ETEXT txt
    convertEContent F.ERET        = ERET ()
    convertEContent (F.EM txt)    = EM txt
    convertEContent (F.EFEX txt)  = EFEX txt

deriving instance Show F.DefRoot


mkFrameNetData :: FrameDB -> Text -> (Text,DefRoot)
mkFrameNetData framemap fname = fromMaybe (fname,DefRoot []) $ do
  frm <- HM.lookup fname (framemap^.frameDB)
  let txt = frm^.frame_definition
      defroot0 = F.p_defRoot (TL.fromStrict txt)
  return (fname,convertDefRoot defroot0)

postAnalysis ::
     FrameDB
  -> [RoleInstance]
  -> QQVar ComputeQuery ComputeResult
  -> InputSentence
  -> Handler APIResult
postAnalysis framedb rolemap qqvar (InputSentence sent) = do
  CR_Sentence (ResultSentence _ tokss mgs cout) <- liftIO (singleQuery qqvar (CQ_Sentence sent))
  dots <- liftIO $ mapM createDotGraph mgs
  svgs <- liftIO $ mapM createOGDFSVG (zip [1..] mgs)
  let mts = concatMap (mkMeaningTree rolemap) mgs
      arbs = concatMap (mkARB rolemap) mgs
      fns = map (mkFrameNetData framedb) (concatMap allFrames mts)
      cout' = S.ConsoleOutput (cout^.outputX'tree) (cout^.outputDocStructure) (cout^.outputMatchedFrames)
  pure (APIResult tokss mts arbs dots svgs fns cout')


batchTest :: QQVar Q R -> Handler Text
batchTest qqvar = T.pack . show <$> liftIO (singleQuery qqvar Q)


data ServerConfig = ServerConfig {
                      computeConfig :: FilePath
                    , langConfig :: FilePath
                    , staticDir :: FilePath
                    , webPort :: Int
                    }
                  deriving (Show)


pOptions :: Parser ServerConfig
pOptions = ServerConfig <$> strOption (long "compute" <> short 'c' <> help "Compute engine server/client configuration")
                        <*> strOption (long "lang"    <> short 'l' <> help "Language engine server/client configuration")
                        <*> strOption (long "static"  <> short 's' <> help "Directory of static html files")
                        <*> option auto (long "port" <> short 'p' <> help "web port")

serverConfig :: ParserInfo ServerConfig
serverConfig = info pOptions (fullDesc <> progDesc "spapi-server")


main :: IO ()
main = do
  cfg <- execParser serverConfig

  qqvar1 <- newTVarIO emptyQQ
  qqvar2 <- newTVarIO emptyQQ

  ecompcfg :: Either String ComputeConfig <- eitherDecodeStrict <$> B.readFile (computeConfig cfg)
  elangcfg :: Either String LexDataConfig <- eitherDecodeStrict <$> B.readFile (langConfig cfg)

  forM_ ((,) <$> ecompcfg <*> elangcfg) $ \(compcfg,langcfg) -> do
    let cport  = port  (computeClient compcfg)
        chostg = hostg (computeClient compcfg)
        chostl = hostl (computeClient compcfg)
        shostg = hostg (computeServer compcfg)
        sport  = port  (computeServer compcfg)
        framedir = langcfg ^. cfg_framenet_framedir
        rolemapfile = langcfg ^. cfg_rolemap_file
    framedb <- loadFrameData framedir
    rolemap <- loadRoleInsts rolemapfile
    forkIO $
      client
        (cport,chostg,chostl,shostg,sport)
        (\them_ping -> do
              heartBeatHandshake them_ping (mainP (webClient qqvar1))
              pure ()
        )
    etagcontext <- defaultETagContext False
    run (webPort cfg) $
      etag etagcontext NoMaxAge  $
        serve api (serveDirectoryFileServer (staticDir cfg)  :<|>
                   postAnalysis framedb rolemap qqvar1       :<|>
                   batchTest qqvar2
                  )
