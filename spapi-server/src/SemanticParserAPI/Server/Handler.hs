module SemanticParserAPI.Server.Handler where

import           Control.Concurrent            ( threadDelay )
import           Control.Lens                  ( (^.) )
import           Control.Monad.IO.Class        ( MonadIO(liftIO) )
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable                 ( for_ )
import           Data.Text                     ( Text )
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.WebSockets.Connection ( Connection
                                               , forkPingThread
                                               , sendTextData
                                               )
import           Servant                       ( throwError )
import           Servant.Server                ( Handler, err404 )
-- language-engine layer
import           FrameNet.Query.Frame          ( FrameDB )
import           Lexicon.Type                  ( RoleInstance )
import           SRL.Analyze.ARB               ( mkARB )
import           SRL.Analyze.MeaningTree       ( mkMeaningTree )
import           SRL.Analyze.Type              ( outputDocStructure
                                               , outputMatchedFrames
                                               , outputX'tree
                                               )
-- compute-pipeline layer
import           CloudHaskell.QueryQueue       ( QQVar, singleQuery )
import           Compute.Type.Status           ( StatusQuery(..), StatusResult(..) )
import           SemanticParserAPI.Type        ( InputSentence(..), APIResult(..) )
import qualified SemanticParserAPI.Type as S   ( ConsoleOutput(ConsoleOutput)
                                               , StatusResult(..)
                                               )
import           Task.CoreNLP                  ( QCoreNLP(..), RCoreNLP(..) )
import           Task.SemanticParser           ( ComputeQuery(..)
                                               , ComputeResult(..)
                                               , ResultSentence(..)
                                               )
--
import           SemanticParserAPI.Server.Worker
                 ( allFrames
                 , createDotGraph
                 , createOGDFSVG
                 , mkFrameNetData
                 )


postAnalysis ::
     FrameDB
  -> [RoleInstance]
  -- -> QQVar ComputeQuery ComputeResult
  -> InputSentence
  -> Handler APIResult
postAnalysis framedb rolemap {- qqvar -} (InputSentence sent) = do
{-  CR_Sentence (ResultSentence _ tokss mgs cout) <- liftIO (singleQuery qqvar (CQ_Sentence sent))
  dots <- liftIO $ mapM createDotGraph mgs
  svgs <- liftIO $ mapM createOGDFSVG (zip [1..] mgs)
  let mts = concatMap (mkMeaningTree rolemap) mgs
      arbs = concatMap (mkARB rolemap) mgs
      fns = map (mkFrameNetData framedb) (concatMap allFrames mts)
      cout' = S.ConsoleOutput (cout^.outputX'tree) (cout^.outputDocStructure) (cout^.outputMatchedFrames)
  pure (APIResult tokss mts arbs dots svgs fns cout')
-}
  throwError err404

getStatus :: {- QQVar StatusQuery StatusResult -> -} Handler S.StatusResult
getStatus {- qqvar -} = do
    {- SR lst <- liftIO (singleQuery qqvar SQ)
    pure (S.StatusResult lst)
    -}
    pure (S.StatusResult [])


wsStream :: MonadIO m => {- QQVar StatusQuery StatusResult -> -} Connection -> m ()
wsStream {- qqvar -} conn = do
    liftIO $ forkPingThread conn 10
    liftIO $ for_ ([1..] :: [Int]) $ \_ -> do
      -- SR lst <- liftIO (singleQuery qqvar SQ)
      -- let statusData = S.StatusResult lst
      let statusData = S.StatusResult []
      liftIO $ print statusData
      sendTextData conn (TE.decodeUtf8 (BL.toStrict (A.encode statusData)))
      threadDelay 1000000

{-
postCoreNLP ::
     QQVar QCoreNLP RCoreNLP
  -> Text
  -> Handler Text
postCoreNLP qqvar txt = do
  RCoreNLP doc <- liftIO (singleQuery qqvar (QCoreNLP txt))
  pure (T.pack (show doc))
-}
