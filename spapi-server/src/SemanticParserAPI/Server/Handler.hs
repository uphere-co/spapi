{-# LANGUAGE OverloadedStrings #-}
module SemanticParserAPI.Server.Handler where

import           Control.Concurrent            ( threadDelay )
import           Control.Lens                  ( (^.) )
import           Control.Monad.IO.Class        ( MonadIO(liftIO) )
import           Control.Monad.Trans.Except    ( ExceptT(..), withExceptT )
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Foldable                 ( for_ )
import qualified Data.Text.Encoding as TE
import           Network.WebSockets.Connection ( Connection
                                               , forkPingThread
                                               , sendTextData
                                               )
import           Servant.Client                ( ClientM, ClientEnv(..)
                                               , client, runClientM
                                               )
import           Servant.Server                ( Handler(..), err404 )
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
import           SemanticParserAPI.Type        ( InputSentence(..), APIResult(..) )
import qualified SemanticParserAPI.Type as S   ( ConsoleOutput(ConsoleOutput)
                                               , StatusResult(..)
                                               )
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


-- TODO: the following getSemantic client should be located elsewhere.
import           Worker.API (soAPI)
-- client

getSemantic :: ComputeQuery -> ClientM ComputeResult
getSemantic = client  soAPI


-- TODO: FrameDB and [RoleInstance] should be hidden in Reader monad
postAnalysis ::
     ClientEnv
  -> FrameDB
  -> [RoleInstance]
  -> InputSentence
  -> Handler APIResult
postAnalysis env framedb rolemap (InputSentence sent) = do
  let query = CQ_Sentence sent
  CR_Sentence (ResultSentence _ tokss mgs cout) <-
    Handler $ withExceptT (const err404) $
      ExceptT $ liftIO $ runClientM (getSemantic query) env
  dots <- liftIO $ mapM createDotGraph mgs
  svgs <- liftIO $ mapM createOGDFSVG (zip [1..] mgs)
  let mts = concatMap (mkMeaningTree rolemap) mgs
      arbs = concatMap (mkARB rolemap) mgs
      fns = map (mkFrameNetData framedb) (concatMap allFrames mts)
      cout' = S.ConsoleOutput (cout^.outputX'tree) (cout^.outputDocStructure) (cout^.outputMatchedFrames)
  pure (APIResult tokss mts arbs dots svgs fns cout')

getStatus :: Handler S.StatusResult
getStatus = do
    -- old code
    {- SR lst <- liftIO (singleQuery qqvar SQ)
    pure (S.StatusResult lst)
    -}
    pure (S.StatusResult [])


wsStream :: MonadIO m => Connection -> m ()
wsStream conn = do
    liftIO $ forkPingThread conn 10
    liftIO $ for_ ([1..] :: [Int]) $ \_ -> do
      -- SR lst <- liftIO (singleQuery qqvar SQ)
      -- let statusData = S.StatusResult lst
      let statusData = S.StatusResult []
      liftIO $ print statusData
      sendTextData conn (TE.decodeUtf8 (BL.toStrict (A.encode statusData)))
      threadDelay 10000000
