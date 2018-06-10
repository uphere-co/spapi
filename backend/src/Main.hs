{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import           Control.Concurrent                  (forkIO,threadDelay)
import           Control.Concurrent.STM              (atomically,retry,newTVarIO
                                                     ,modifyTVar',readTVar,writeTVar)
import           Control.Distributed.Process.Lifted  (SendPort,spawnLocal)
import           Control.Lens                        ((^.),(^..))
import           Control.Monad                       (forever,void)
import           Control.Monad.IO.Class              (liftIO)
import qualified Data.ByteString.Base64        as B64
import qualified Data.ByteString.Char8         as B
import qualified Data.HashMap.Strict           as HM
import qualified Data.IntMap                   as IM
import           Data.Maybe                          (fromMaybe)
import           Data.Proxy                          (Proxy(..))
import           Data.Semigroup                      ((<>))
import           Data.Text                           (Text)
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.IO                  as TIO
import qualified Data.Text.Lazy                as TL
import           Network.Wai.Handler.Warp            (run)
import           Network.Wai.Middleware.ETag
import           Servant.API                         ((:<|>)((:<|>)))
import           Servant.Utils.StaticFiles           (serveDirectoryFileServer)
import           Servant.Server                      (Handler,serve)
import           System.Directory                    (getCurrentDirectory
                                                     ,setCurrentDirectory
                                                     ,getTemporaryDirectory)
import           System.Environment                  (getArgs)
import           System.Process                      (readProcess)
-- nlp layer
import           FrameNet.Query.Frame                (FrameDB,frameDB,loadFrameData)
import qualified FrameNet.Type.Definition      as F
import           FrameNet.Type.Frame                 (frame_definition)
import           Lexicon.Query                       (loadRoleInsts)
import           Lexicon.Type                        (RoleInstance)
import           NLP.Semantics.Type                  (MeaningRoleContent(..),MeaningTree(..)
                                                     ,mt_frame,mt_arguments,mt_subordinates
                                                     ,mr_content,po_main)
import           SRL.Analyze.ARB                     (mkARB)
import           SRL.Analyze.Format                  (dotMeaningGraph)
import           SRL.Analyze.MeaningTree             (mkMeaningTree)
import           SRL.Analyze.Type                    (AnalyzePredata,DocStructure,MeaningGraph
                                                     ,analyze_framedb,analyze_rolemap
                                                     ,ds_mtokenss,ds_sentStructures,ss_tagged)
-- spapi layer
import           CloudHaskell.QueryQueue             (QueryStatus(..),QQVar,emptyQQ,next
                                                     ,singleQuery)
import           CloudHaskell.Util                   (LogProcess
                                                     ,onesecond,tellLog,queryProcess
                                                     ,client,mainP,initP)
import           SemanticParserAPI.Compute.Type      (ComputeQuery(..),ComputeResult(..)
                                                     ,ResultSentence(..))
import           SemanticParserAPI.CLI.Client        (consoleClient)
import           SemanticParserAPI.Type              (InputSentence(..),PNGData(..),APIResult(..)
                                                     ,DefRoot(..),CContent(..),EContent(..))
--
import           API

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
  let pngdata = PNGData (TE.decodeUtf8 ("data:image/png;base64," <> B64.encode bstr))
  return pngdata

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








getItem ::
     FrameDB
  -> [RoleInstance]
  -> QQVar ComputeQuery ComputeResult
  -> Handler APIResult
getItem framedb rolemap qqvar = do
  let sent = "I sent a letter to him."
  CR_Sentence (ResultSentence _ tokss mgs) <- liftIO (singleQuery qqvar (CQ_Sentence sent))
  -- liftIO $ print mgs
  dots <- liftIO $ mapM createDotGraph mgs
  let mts = concatMap (mkMeaningTree rolemap) mgs
      arbs = concatMap (mkARB rolemap) mgs
      fns = map (mkFrameNetData framedb) (concatMap allFrames mts)
  pure (APIResult tokss mts arbs dots fns)

  -- pure (Item 0 (show mgs))

main = do
  (d:_) <- getArgs
  -- putStrLn "Serving on localhost:8080/static/, visit http://localhost:8080/static/index.html"

  qqvar <- newTVarIO emptyQQ

  -- TODO: move this to configuration
  let framedir = "/data/groups/uphere/data/NLP/FrameNet/1.7/fndata/fndata-1.7/frame"
  let rolemapfile = "/home/wavewave/repo/srcp/lexicon-builder/mapping/final.txt"

  framedb <- loadFrameData framedir
  rolemap <- loadRoleInsts rolemapfile

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
                 getItem framedb rolemap qqvar
                )
